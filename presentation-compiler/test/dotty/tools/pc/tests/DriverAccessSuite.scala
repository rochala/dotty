package dotty.tools.pc.tests

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.ScalaPresentationCompiler
import org.junit.{Before, Test}

import scala.language.unsafeNulls
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.OffsetParams
import scala.concurrent.Future
import scala.concurrent.Await
import scala.meta.pc.VirtualFileParams
import scala.concurrent.duration.*

import java.util.Collections
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import dotty.tools.pc.DriverAccess
import dotty.tools.pc.CachingDriver
import dotty.tools.dotc.CompilationUnit
import dotty.tools.pc.base.TestResources
import java.io.File
import dotty.tools.pc.CompilationInputs
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.CancelToken
import scala.meta.internal.metals.EmptyReportContext
import dotty.tools.pc.Profiler
import dotty.tools.pc.util.TraceEvent
import dotty.tools.pc.LspRequest


class EmptyProfiler() extends Profiler:
  def reportEvent(event: TraceEvent): Unit = ()

class DriverAccessSuite extends BasePCSuite:

  val timeout = 5.seconds


  val defaultFlags = List("-color:never", "-classpath" /*, "-Yplain-printer","-Yprint-pos"*/) :+
    TestResources.classpath.mkString(File.pathSeparator)

    def inputs(source: String, fileName: String = "Test.scala"): CompilationInputs =
      val uri = Paths.get(fileName).toUri()
      CompilationInputs(uri, source, LspRequest.Unknown, EmptyCancelToken, false)



  @Test def simple: Unit =
    given CancelToken = EmptyCancelToken
    val driver = DriverAccess(config, defaultFlags, EmptyReportContext, EmptyProfiler())
    val result = driver.enqueueCancellable(inputs("object A"))(_ => ())
    result.get().nn
    println(result)

  @Test def fff: Unit =
    given CancelToken = EmptyCancelToken
    val driver = DriverAccess(config, defaultFlags, EmptyReportContext, EmptyProfiler())
    val result = driver.enqueueCancellable(inputs("object A"))(_ => ())
    val result1 = driver.enqueueCancellable(inputs("object A"))(_ => ())
    val result2 = driver.enqueueCancellable(inputs("object A"))(_ => ())
    val result3 = driver.enqueueCancellable(inputs("object A"))(_ => ())
    val result4 = driver.enqueueCancellable(inputs("object A"))(_ => ())
    result4.get().nn
    println(result)
    println(result1)
    println(result2)
    println(result3)
    println(result4)

  @Test def fff2: Unit =
    given CancelToken = EmptyCancelToken
    (1 to 100).foreach { _ =>

      val driver = DriverAccess(config, defaultFlags, EmptyReportContext, EmptyProfiler())
      val result = driver.enqueueCancellable(inputs("object A"))(_ => ())
      val result1 = driver.enqueueCancellable(inputs("object A1"))(_ => ())
      val result2 = driver.enqueueCancellable(inputs("object A2"))(_ => ())
      val result3 = driver.enqueueCancellable(inputs("object A3"))(_ => ())
      val result4 = driver.enqueueCancellable(inputs("object A4"))(_ => ())
      result4.get().nn
      println(result)
      println(result1)
      println(result2)
      println(result3)
      println(result4)

      Thread.sleep(100)
    }


//   private def checkCompilationCount(expected: Int): Unit =
//     presentationCompiler match
//       case pc: ScalaPresentationCompiler =>
//         val compilations = pc.compilerAccess.withInterruptableCompiler(None)(-1, EmptyCancelToken) { driver =>
//           driver.currentCtx.runId
//         }.get(timeout.length, timeout.unit)
//         assertEquals(expected, compilations, s"Expected $expected compilations but got $compilations")
//       case _ => throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

//   private def getContext(): Context =
//     presentationCompiler match
//       case pc: ScalaPresentationCompiler =>
//         pc.compilerAccess.withInterruptableCompiler(None)(null, EmptyCancelToken) { driver =>
//           driver.currentCtx
//         }.get(timeout.length, timeout.unit)
//       case _ => throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

//   @Before
//   def beforeEach: Unit =
//     presentationCompiler.restart()

//     // We want to run at least one compilation, so runId points at 3.
//     // This will ensure that we use the same driver, not recreate fresh one on each call
//     val dryRunParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "dryRun", 1, EmptyCancelToken)
//     checkCompilationCount(2)
//     val freshContext = getContext()
//     presentationCompiler.complete(dryRunParams).get(timeout.length, timeout.unit)
//     checkCompilationCount(3)
//     val dryRunContext = getContext()
//     assert(freshContext != dryRunContext)


//   @Test
//   def `cursor-compilation-does-not-corrupt-cache`: Unit =
//     val contextPreCompilation = getContext()

//     val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
//     val contextPostFirst = getContext()
//     assert(contextPreCompilation != contextPostFirst)
//     checkCompilationCount(4)

//     val fakeParamsCursor = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = new", 15, EmptyCancelToken)
//     presentationCompiler.complete(fakeParamsCursor).get(timeout.length, timeout.unit)
//     val contextPostCursor = getContext()
//     assert(contextPreCompilation != contextPostCursor)
//     assert(contextPostFirst == contextPostCursor)
//     checkCompilationCount(4)

//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
//     val contextPostSecond = getContext()
//     assert(contextPreCompilation != contextPostSecond)
//     assert(contextPostFirst == contextPostCursor)
//     assert(contextPostCursor == contextPostSecond)
//     checkCompilationCount(4)

//   @Test
//   def `compilation-for-same-snippet-is-cached`: Unit =
//     val contextPreCompilation = getContext()

//     val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
//     val contextPostFirst = getContext()
//     assert(contextPreCompilation != contextPostFirst)
//     checkCompilationCount(4)

//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
//     val contextPostSecond = getContext()
//     assert(contextPreCompilation != contextPostFirst)
//     assert(contextPostSecond == contextPostFirst)
//     checkCompilationCount(4)

//   @Test
//   def `compilation-for-different-snippet-is-not-cached`: Unit =


//     checkCompilationCount(3)
//     val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
//     checkCompilationCount(4)

//     val fakeParams2 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams2).get(timeout.length, timeout.unit)
//     checkCompilationCount(5)

//     val fakeParams3 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = print", 17, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams3).get(timeout.length, timeout.unit)
//     checkCompilationCount(6)


//   private val testFunctions: List[OffsetParams => CompletableFuture[_]] = List(
//     presentationCompiler.complete(_),
//     presentationCompiler.convertToNamedArguments(_, Collections.emptyList()),
//     presentationCompiler.autoImports("a", _, false),
//     presentationCompiler.definition(_),
//     presentationCompiler.didChange(_),
//     presentationCompiler.documentHighlight(_),
//     presentationCompiler.hover(_),
//     presentationCompiler.implementAbstractMembers(_),
//     presentationCompiler.insertInferredType(_),
//     presentationCompiler.semanticTokens(_),
//     presentationCompiler.prepareRename(_),
//     presentationCompiler.rename(_, "a"),
//     presentationCompiler.signatureHelp(_),
//     presentationCompiler.typeDefinition(_)
//   )


//   @Test
//   def `different-api-calls-reuse-cache`: Unit =
//     val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 13, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)

//     val contextBefore = getContext()

//     val differentContexts = testFunctions.map: f =>
//       f(fakeParams).get(timeout.length, timeout.unit)
//       checkCompilationCount(4)
//       getContext()
//     .toSet

//     assert(differentContexts == Set(contextBefore))

//   @Test
//   def `different-api-calls-reuse-cache-parallel`: Unit =
//     import scala.jdk.FutureConverters.*
//     import scala.concurrent.ExecutionContext.Implicits.global

//     val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 13, EmptyCancelToken)
//     presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)

//     val contextBefore = getContext()

//     val futures = testFunctions.map: f =>
//       f(fakeParams).asScala.map(_ => getContext())

//     val res = Await.result(Future.sequence(futures), timeout).toSet
//     assert(res == Set(contextBefore))
