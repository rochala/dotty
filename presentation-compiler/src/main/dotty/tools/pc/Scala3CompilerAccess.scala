package dotty.tools.pc


import scala.meta.internal.metals.ReportContext
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.dotc.interactive.InteractiveDriver
import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams
import java.util.concurrent.CompletableFuture
import java.util.logging.Logger
import scala.meta.internal.metals.EmptyCancelToken
import java.net.URI
import scala.meta.pc.OffsetParams
import scala.jdk.CollectionConverters.*

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
  def fromParams(params: VirtualFileParams | OffsetParams, lspRequestKind: LspRequest, cleanDriver: Boolean = false): CompilationInputs =
    CompilationInputs(params.uri().nn, params.text().nn, lspRequestKind, params.token().nn, cleanDriver)

class DriverAccess(
  config: PresentationCompilerConfig,
  driverSettings: List[String],
  reportContext: ReportContext,
  profiler: Profiler
):
  private val logger: Logger = Logger.getLogger(getClass.getName).nn
  private val taskQueue = TaskQueue(profiler, driverSettings, reportContext)
  private val worker = Worker(taskQueue, config, reportContext)

  def enqueueUncancellable[T](
    inputs: CompilationInputs
  )(f: InteractiveDriver => T): CompletableFuture[T] =
    enqueueTask(taskQueue.NonCancellable(inputs)(f))

  def lookup[T](taskName: LspRequest)(f: InteractiveDriver => T): CompletableFuture[T] =
    enqueueTask(taskQueue.LookupTask(taskName)(f))

  def enqueueCancellable[T](inputs: CompilationInputs)
    (f: CancelToken ?=> InteractiveDriver => T): CompletableFuture[T] =

    given token: CancelToken = inputs.cancelToken
    val task = taskQueue.Cancellable(token, inputs)(f)

    // SERVER CANCELLATION
    worker.cancelCurrentCancellable(inputs.taskName)
    taskQueue.cancelNotStartedOfSameType(inputs.taskName)

    enqueueTask(task)

  end enqueueCancellable

  private def enqueueTask[T](task: taskQueue.Task[T]): CompletableFuture[T] =
    taskQueue.enqueue(task)
    worker.scheduleProcessing()
    task.future

  def shutdown(): Unit =
    worker.shutdown()
    taskQueue.shutdown()

  def restart() =
    taskQueue.shutdown()

