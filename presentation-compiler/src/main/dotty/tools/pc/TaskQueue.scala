package dotty.tools.pc

import scala.meta.internal.metals.ReportContext
import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.pc.CancelToken
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import scala.meta.internal.pc.CompilerThrowable
import scala.meta.internal.metals.Report
import java.util.logging.Logger
import java.util.logging.Level
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CancellationException
import scala.jdk.CollectionConverters.*
import java.util.concurrent.atomic.AtomicInteger
import java.nio.file.Path
import java.util.concurrent.ConcurrentLinkedQueue
import dotty.tools.dotc.sbt.interfaces.ProgressCallback

class TaskQueue(profiler: Profiler, driverSettings: List[String], reportContext: PcReportContext):
  private val logger: Logger = Logger.getLogger(getClass.getName).nn

  private val currentId: AtomicInteger = AtomicInteger(0)
  private val queue: ConcurrentLinkedQueue[Task[?]] = ConcurrentLinkedQueue[Task[?]]() // Not started tasks only

  private val _driver: AtomicReference[CachingDriver] = AtomicReference(newCompiler())
  private def newCompiler(): CachingDriver = new CachingDriver(driverSettings)

  def enqueue(task: Task[?]): Boolean = queue.add(task)
  def dequeue(): Option[Task[?]] = Option(queue.poll().nn)

  def cancelNotStartedOfSameType(lspRequestType: LspRequest) =
    import scala.jdk.CollectionConverters.*
    import scala.language.unsafeNulls

    val cancelledTasks = queue.iterator().asScala.toList.collect:
      case t: Cancellable[?] if t.inputs.taskName == lspRequestType => t

    cancelledTasks.foreach(_.cancelNotStartedTask())
    queue.removeAll(cancelledTasks.asJava)

  def shutdownCurrentCompiler(): Unit =
    // tutaj trzeba dodac jeszcze logging i tracing
    _driver.set(new CachingDriver(driverSettings))

  private def getDriver(cleanDriver: Boolean): CachingDriver =
    if cleanDriver then new CachingDriver(driverSettings)
    else _driver.get().nn

  sealed trait Task[T]:
    private val _interrupted = AtomicBoolean(false)

    val inputs: CompilationInputs
    val future: CompletableFuture[T] = new CompletableFuture()
    val lspRequestType: LspRequest
    val runId = currentId.incrementAndGet()
    def execute(): Unit
    def compile(): T

    final def interrupt(): Unit =
      recordAction(TraceName.TaskInterruptionRequested)
      _interrupted.set(true)

    final def isInterrupted: Boolean = _interrupted.get()

    final def recordAction[T](name: TraceName): Unit =
      profiler.reportEvent(name, lspRequestType.toString, EventPhases.Instant, System.nanoTime(), 0, runId)


  /**
   * This task can be cancelled by the user. It will cancel any tasks that did not start yet.
   * If there is currently evaluated task, we will kindly ask it to cancel itself.
   */
  case class Cancellable[T](token: CancelToken, val inputs: CompilationInputs)(f: InteractiveDriver => T) extends Task[T]:
    val lspRequestType: LspRequest = inputs.taskName
    recordAction(TraceName.CancellableTask)

    val callback = new ProgressCallback:
      override def isCancelled(): Boolean =
        if token.nn.isCanceled() then
          recordAction(TraceName.ClientCancelTriggered)
          true
        else if isInterrupted || Thread.interrupted() then
          recordAction(TraceName.InterruptCancelTriggered)
          true
        else false

    def cancelNotStartedTask(): Unit =
      recordAction(TraceName.ServerCancelled)
      future.cancel(true)

    override def compile(): T =
      import inputs.*
      val driver = getDriver(cleanDriver)
      driver.runWithProgressCallback(uri, code, callback)
      f(driver)

    override def execute(): Unit = this.synchronized:
      try
        if callback.isCancelled then
          future.cancel(true)
        else
          profiler.traced(lspRequestType.toString, runId, TraceName.Compilation):
            val res = compile()
            future.complete(res)
      catch case ex: Exception =>
        if token.nn.isCanceled then
          recordAction(TraceName.ClientCancelled)
          future.completeExceptionally(new CancellationException())
        else if isInterrupted then
          recordAction(TraceName.TaskInterrupted)
          future.completeExceptionally(new InterruptedException())
        else
          recordAction(TraceName.CompletedExceptionally)
          handleError(ex, (inputs))
          future.completeExceptionally(ex)

  case class NonCancellable[T](val inputs: CompilationInputs)(f: InteractiveDriver => T) extends Task[T]:
    val lspRequestType = inputs.taskName
    recordAction(TraceName.UncancellableTask)

    val callback = new ProgressCallback:
      override def isCancelled(): Boolean = isInterrupted

    override def compile(): T =
      import inputs.*
      val driver = getDriver(cleanDriver)
      driver.runWithProgressCallback(uri, code, callback)
      f(driver)

    override def execute(): Unit =
      try
        profiler.traced(lspRequestType.toString, runId, TraceName.Compilation):
          future.complete(compile())
      catch case ex: Exception =>
        // cleanup memory, if this happened the execution failed and will not run in the background
        recordAction(TraceName.CompletedExceptionally)
        // shutdownCurrentCompiler()
        handleError(ex, (inputs))
        future.completeExceptionally(ex)

  case class LookupTask[T](lspRequestType: LspRequest)(f: InteractiveDriver => T) extends Task[T]:
    // traces += UncancellableTask()

    override val inputs: CompilationInputs = CompilationInputs.empty

    override def compile(): T =
      val driver = _driver.get().nn
      f(driver)

    override def execute(): Unit =
      try
        profiler.traced(lspRequestType.toString, runId, TraceName.Compilation):
          future.complete(compile())
      catch case ex: Exception =>
        // cleanup memory, if this happened the execution failed and will not run in the background
        recordAction(TraceName.CompletedExceptionally)
        // handle
        // shutdownCurrentCompiler()
        future.completeExceptionally(ex)

  private def handleError(e: Throwable, compilationInputs: CompilationInputs): Unit =
    val error = CompilerThrowable.trimStackTrace(e)
    val report =
      Report(
        "compiler-error",
        s"""|occurred in the Presentation Compiler.
            |
            |Presentation Compiler configuration:
            |
            |action parameters:
            |${compilationInputs.show}
            |
            |Additional data:
            |  ${reportContext.additionalData}
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

  def shutdown() = shutdownCurrentCompiler()
