package dotty.tools.pc

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.ExecutionContextExecutor
import scala.meta.internal.metals.ReportContext
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.internal.pc.CompilerJobQueue
import scala.meta.pc.CancelToken
import scala.compiletime.uninitialized
import scala.meta.pc.VirtualFileParams
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import scala.meta.internal.pc.CompilerThrowable
import scala.meta.internal.metals.Report
import java.util.logging.Logger
import scala.meta.internal.mtags.CommonMtagsEnrichments.XtensionVirtualFileParams
import java.util.logging.Level
import scala.util.control.NonFatal
import scala.collection.mutable.HashSet
import dotty.tools.dotc.sbt.interfaces.ProgressCallback
import dotty.tools.dotc.CompilationUnit
import scala.meta.internal.pc.CompilerJobQueue.LastInFirstOutBlockingQueue
import scala.collection.mutable.PriorityQueue
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ExecutorService
import scala.meta.internal.metals.EmptyCancelToken
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Executors
import dotty.tools.dotc.core.Contexts.Context
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext
import scala.collection.mutable.ListBuffer
import org.eclipse.lsp4j.jsonrpc.CompletableFutures
import java.util.concurrent.atomic.AtomicLong
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.Queue
import java.net.URI
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.Semaphore
import java.util.concurrent.CancellationException
import dotty.tools.dotc.transform.LazyVals.lazyNme.result
import scala.meta.pc.OffsetParams
import scala.jdk.CollectionConverters.*
import java.util.concurrent.atomic.AtomicInteger
import java.nio.file.Files
import java.nio.file.Path
import com.google.gson.Gson
import dotty.tools.dotc.report.log
import java.util.concurrent.TimeoutException
import java.util.concurrent.TimeUnit
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.ScheduledFuture
import java.lang.management.ThreadMXBean
import java.lang.management.ManagementFactory

enum TraceName:
  case CancellableTask
  case UncancellableTask
  case Started
  case Timeout
  case Completed
  case CancelRequested
  case CacheHit
  case CacheMiss
  case GracefulCancelTriggered
  case ClientCancelled
  case ServerCancelled
  case CompletedExceptionally
  case Compilation

enum LspRequest:
  case SemanticTokens, InlayHints, Completion, Definition, TypeDefinition,
    DocumentHighlight, References, InferType, SemanticDBDocument, CompletionItem,
    AutoImports, ImplementAbstractMembers, InsertInferredType, InlineValue,
    ExtractMethod, ConvertToNamedArguments, SelectionRange, Hover, PrepareRename,
    GetTasty, InferExpectedType, Info, CompletionItemResolve, DidClose,
    Rename, SignatureHelp, DidChange, Unknown // we will need to track the request type to implement server cancel

case class CompilationInputs(uri: URI, code: String, taskName: LspRequest, cancelToken: CancelToken = EmptyCancelToken, cleanDriver: Boolean = false):
  def show: String =
    s"""|uri: $uri
        |code: $code
        |cancelToken: $cancelToken
        |cleanDriver: $cleanDriver
        |""".stripMargin

object CompilationInputs:
  def empty: CompilationInputs = CompilationInputs(new URI(""), "", LspRequest.Unknown, EmptyCancelToken, false)
  def fromParams(params: VirtualFileParams | OffsetParams, lspRequest: LspRequest, cleanDriver: Boolean = false): CompilationInputs =
    CompilationInputs(params.uri().nn, params.text().nn, lspRequest, params.token().nn, cleanDriver)

enum EventPhases(val ph: String):
  case Begin extends EventPhases("B")
  case End extends EventPhases("E")
  case Duration extends EventPhases("X")
  case Instant extends EventPhases("i")

case class JsonWritingProfiler(outputFolder: Option[Path]) extends Profiler:
  private val logger: Logger = Logger.getLogger(getClass.getName).nn

  private val outputFolder0 = outputFolder.map(_.resolve(".metals").nn).getOrElse(Files.createTempDirectory("metals-trace").nn)
  private val gson = new Gson()
  private val outputDir = Files.createDirectories(outputFolder0)
  private val outputFile = outputFolder0.resolve("traces.json")

  Files.deleteIfExists(outputFile)
  Files.createFile(outputFile)
  logger.info(s"Writing traces to $outputFile")

  def reportEvent(event: util.TraceEvent): Unit = write(event)


  private def write(event: util.TraceEvent): Unit =
    val json = gson.toJson(event).nn + ",\n"
    Files.write(outputFile, json.getBytes(), java.nio.file.StandardOpenOption.APPEND)

trait Profiler:
  def reportEvent(event: util.TraceEvent): Unit
  def reportEvent(
    name: TraceName,
    cat: String,
    eventPhase: EventPhases,
    ts: Long,
    dur: Long,
    pid: Int,
    tid: Int = Thread.currentThread().nn.getId().toInt,
    args: Seq[String] = Nil
  ): Unit =
    reportEvent(util.TraceEvent(name.toString, cat, eventPhase.ph, ts / 1000, dur / 1000, 1, 1, args.asJava))


class DriverAccess(
  config: PresentationCompilerConfig,
  driverSettings: List[String],
  reportContext: ReportContext,
  profiler: Profiler
):
  // TODO ADD CHECK MEMORY FROM DOTTY, ADD BETTER WAYS TO DEBUG PC

  private val logger: Logger = Logger.getLogger(getClass.getName).nn
  private val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor().nn
  private val timeoutExecutor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor().nn


  private def newCompiler(): CachingDriver = new CachingDriver(driverSettings)
  private val _driver: AtomicReference[CachingDriver] = AtomicReference(newCompiler())

  private val currentTask: AtomicReference[Option[Task[_]]] = AtomicReference(None)
  private val queue: Queue[Task[_]] = Queue[Task[_]]() // Not started tasks only
  private val currentId: AtomicInteger = AtomicInteger(0)

  def getDriver(cleanDriver: Boolean): CachingDriver =
    if cleanDriver then new CachingDriver(driverSettings)
    else _driver.get().nn

  private sealed trait Task[T]:
    val inputs: CompilationInputs
    val future: CompletableFuture[T] = new CompletableFuture()
    val taskName: LspRequest
    val runId = currentId.incrementAndGet()
    def execute(): Unit
    def compile(): T
    def timeout(): Unit

    def recordAction[T](name: TraceName): Unit =
      profiler.reportEvent(name, taskName.toString, EventPhases.Instant, System.nanoTime(), 0, runId)

    def traced[T](name: TraceName)(f: => T): T =
      val start = System.nanoTime()
      try f
      finally
        val end = System.nanoTime()
        profiler.reportEvent(name, taskName.toString, EventPhases.Duration, start, end - start, runId)

  /**
   * This task can be cancelled by the user. It will cancel any tasks that did not start yet.
   * If there is currently evaluated task, we will kindly ask it to cancel itself.
   */
  private case class Cancellable[T](token: CancelToken, val inputs: CompilationInputs)(f: InteractiveDriver => T) extends Task[T]:
    val taskName: LspRequest = inputs.taskName
    private val cancelled = AtomicBoolean(false)
    recordAction(TraceName.CancellableTask)

    override def timeout(): Unit =
      cancelled.set(true)
      // Add track that we may have a zombie task !!!!!!!
      // If this runs for more than config setting we kill the app ?
      future.cancel(true)

    private class InteractiveProgressCallback() extends ProgressCallback:
      override def isCancelled(): Boolean =
        if cancelled.get() || token.nn.isCanceled() || Thread.interrupted() then
          recordAction(TraceName.GracefulCancelTriggered)
          true
        else false

    def cancelNotStartedTask(): Unit = this.synchronized:
      recordAction(TraceName.ServerCancelled)
      future.cancel(true)

    def cancel(): Unit =
      recordAction(TraceName.CancelRequested)
      cancelled.set(true)

    override def compile(): T =
      import inputs.*
      val driver = getDriver(cleanDriver)
      driver.runWithProgressCallback(uri, code, InteractiveProgressCallback())
      f(driver)

    override def execute(): Unit = this.synchronized:
      try
        if token.isCanceled() then
          recordAction(TraceName.ClientCancelled)
        if !future.isCancelled() && !cancelled.get() then
          traced(TraceName.Compilation):
            val res = compile()
            future.complete(res)
        else
          recordAction(TraceName.ClientCancelled) // cancelled before started
          future.cancel(true)
      // zlapac tez timeout exception i go zalogowac jakos humanitarnie
      catch case ex: Exception =>
        // cleanup memory, if this happened the execution failed and will not run in the background

        // recordAction(TraceName.CompletedExceptionally)
        // shutdownCurrentCompiler() // tutaj raczej odpalic metode cleanup ktora sprzata / moze gc odpala
        // if cancelled then complete with cancel
        if cancelled.get() || token.isCanceled() then
          println("CANCELLLED")
          recordAction { if token.isCanceled() then TraceName.ClientCancelled else TraceName.ServerCancelled }
          future.completeExceptionally(new CancellationException()) // return just a cancellation, this error will be random
        else
          recordAction(TraceName.CompletedExceptionally)
          handleError(ex, (inputs))
          future.completeExceptionally(ex)


  private case class NonCancellable[T](val inputs: CompilationInputs)(f: InteractiveDriver => T) extends Task[T]:
    val taskName = inputs.taskName
    recordAction(TraceName.UncancellableTask)

    override def timeout(): Unit = ()

    override def compile(): T =
      import inputs.*
      val driver = getDriver(cleanDriver)
      driver.run(uri, code) // maybe check if cancel is at least requested and add it to stats ?
      f(driver)

    override def execute(): Unit =
      try
        traced(TraceName.Compilation):
          future.complete(compile())
      catch case ex: Exception =>
        // cleanup memory, if this happened the execution failed and will not run in the background
        recordAction(TraceName.CompletedExceptionally)
        // shutdownCurrentCompiler()
        handleError(ex, (inputs))
        future.completeExceptionally(ex)

  private case class LookupTask[T](taskName: LspRequest)(f: InteractiveDriver => T) extends Task[T]:
    // traces += UncancellableTask()

    override val inputs: CompilationInputs = CompilationInputs.empty
    override def timeout(): Unit = ()

    override def compile(): T =
      val driver = _driver.get().nn
      f(driver)

    override def execute(): Unit =
      try
        traced(TraceName.Compilation):
          future.complete(compile())
      catch case ex: Exception =>
        // cleanup memory, if this happened the execution failed and will not run in the background
        recordAction(TraceName.CompletedExceptionally)
        // handle
        // shutdownCurrentCompiler()
        future.completeExceptionally(ex)

  def enqueueUncancellable[T](
    inputs: CompilationInputs
  )(f: InteractiveDriver => T): CompletableFuture[T] = queue.synchronized:
    val task = NonCancellable(inputs)(f)
    queue.enqueue(task)
    scheduleProcessing()
    task.future

  def lookup[T](taskName: LspRequest)(f: InteractiveDriver => T): CompletableFuture[T] =
    val task = LookupTask(taskName)(f)
    queue.enqueue(task)
    scheduleProcessing()
    task.future

  def enqueueCancellable[T](inputs: CompilationInputs)
    (f: CancelToken ?=> InteractiveDriver => T): CompletableFuture[T] = queue.synchronized:

    def cancelCurrentCancellable(): Unit =
      currentTask.get() match
        case Some(t: Cancellable[_]) if t.inputs.taskName == inputs.taskName =>
          t.cancel()
          // We should now report when we lose a driver which may never end
          // try t.future.get().nn catch case _: Exception => ()
          // // chain cacel here on the original future
          // shutdownCurrentCompiler()
        case _ =>

    def queueNewTask(task: Task[_]): Unit =

      // SERVER CANCELLATION ENABLED
      queue.collect { case t: Cancellable[_] if t.inputs.taskName == inputs.taskName => t.cancelNotStartedTask() }
      // drop only when inputs are the same, as they will result in the same result
      queue.dropWhileInPlace {
        case t: Cancellable[_] if t.inputs.taskName == inputs.taskName => true
        case _ => false
      }
      queue.enqueue(task)

    given token: CancelToken = inputs.cancelToken
    val task = Cancellable(token, inputs)(f)

    cancelCurrentCancellable() // SERVER CANCELLATION
    queueNewTask(task)
    scheduleProcessing()
    task.future

  end enqueueCancellable

  /** Notifies compiler to stop at nearest cancellcation check and resets it */
  def shutdownCurrentCompiler(): Unit =
    // tutaj trzeba dodac jeszcze
    _driver.set(new CachingDriver(driverSettings))

  def shutdown(): Unit =
    executor.shutdown()
    timeoutExecutor.shutdown()
    shutdownCurrentCompiler()

  private def scheduleProcessing(): Unit =
    executor.execute(() => processQueue())

  private def handleError(e: Throwable, compilationInputs: CompilationInputs): Unit = {
    val error = CompilerThrowable.trimStackTrace(e)
    val report =
      Report(
        "compiler-error",
        s"""|occurred in the presentation compiler.
            |
            |presentation compiler configuration:
            |
            |action parameters:
            |${compilationInputs.show}
            |""".stripMargin,
        error,
        path = Some(compilationInputs.uri)
      )

    val pathToReport = reportContext.unsanitized.create(report)
    pathToReport match {
      case Some(path) =>
        logger.log(
          Level.SEVERE,
          s"A severe compiler error occurred, full details of the error can be found in the error report $path"
        )
      case _ =>
        logger.log(Level.SEVERE, error.getMessage, error)
    }
    shutdownCurrentCompiler()
  }

  private def handleInfiniteCompilation(thread: Thread, compilationInputs: CompilationInputs): Unit = {
    val stacktrace =
      thread.getStackTrace().nn
        .map(_.nn.toString())
        .mkString("\n")

    val shortMessage =
        """Fatal compiler error encountered.
          |Please send this report to compiler team or create an issue at https://github.com/scala/scala3/issues""".stripMargin

    val report =
      Report(
        "fatal-compiler-error",
        s"""|occurred in the presentation compiler.
            |
            |There is very high chance that you've just discovered infinite compilation.
            |Please report this fatal error to the compiler team by uploading this report.
            |
            |You can do at https://github.com/scala/scala3/issues
            |
            |If your code is sensitive please make sure to remove code from this report.
            |
            |Stacktrace:
            |  ${stacktrace}
            |
            |action parameters:
            |${compilationInputs.show}
            |""".stripMargin,
        shortMessage,
        path = Some(compilationInputs.uri)
      )

    val pathToReport = reportContext.unsanitized.create(report)
    pathToReport match {
      case Some(path) =>
        logger.log(
          Level.SEVERE,
          s"$shortMessage. Full details of the error can be found in the error report $path"
        )
      case _ =>
        logger.log(Level.SEVERE, shortMessage + "\n" + stacktrace.indent(2))
    }
    shutdownCurrentCompiler()
  }

  private def scheduleForceShutdown(executorThread: Thread, inputs: CompilationInputs): ScheduledFuture[Unit] =
    timeoutExecutor.schedule[Unit](() => {
      // Get stack trace (if you have a way to access it)
      // logger.error(s"Task did not respond to cancellation after 10 seconds. Forcing executor shutdown.")
      executor.shutdown()
      try
        if !executor.awaitTermination(5, TimeUnit.SECONDS) then
          executor.shutdownNow()
      catch
        case _: InterruptedException => executor.shutdownNow()
      handleInfiniteCompilation(executorThread, inputs)
      ()
    }, 15, TimeUnit.SECONDS).nn


  private def processQueue(): Unit =
    try {
      var task = queue.synchronized:
        queue.removeHeadOption()

      val thisThread = Thread.currentThread().nn

      task match
        case Some(task) =>
          currentTask.set(Some(task))

          val initialTimeoutFuture = timeoutExecutor.schedule[Unit](() => {
            if !task.future.isDone() then
              // logger.warn(s"Task timeout detected. Attempting graceful cancellation.")
              task.timeout() // Attempt graceful cancellation
              // Schedule the second timeout to force shutdown if still running
          }, config.timeoutDelay(), config.timeoutUnit()).nn

          val forceShutdownFuture  = scheduleForceShutdown(thisThread, task.inputs)

          task.execute() // actual compilation happens here

          initialTimeoutFuture.cancel(false)
          forceShutdownFuture.cancel(false)

          processQueue()
        case None =>
          currentTask.set(None)
    } finally {
      currentTask.set(None)
    }

