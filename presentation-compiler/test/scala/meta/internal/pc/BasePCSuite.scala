package scala.meta.internal.pc

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService

import scala.util.control.NonFatal

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.PresentationCompilerConfig

import coursierapi.Dependency
import coursierapi.Fetch
import coursierapi.MavenRepository
import coursierapi.Repository
import org.eclipse.lsp4j.MarkupContent
import org.junit.Assert._
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither}
import scala.meta.internal.semver.SemVer
import dotty.tools.pc.util.BuildInfo
import java.util.Comparator
import org.junit.AfterClass
import org.junit.runner.notification.RunListener
import org.junit.runner.Description


abstract class BasePCSuite extends RunListener with PcAssertions {

  val executorService: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor()
  // val scalaVersion = BuildInfo.scalaVersion
  val tmp: Path = Files.createTempDirectory("metals")

  protected lazy val presentationCompiler: PresentationCompiler = {
    val scalaLibrary = BuildInfo.ideTestsDependencyClasspath.map(_.toPath).toSeq

    val myclasspath: Seq[Path] = scalaLibrary

    val scalacOpts = scalacOptions(myclasspath)

    new ScalaPresentationCompiler()
      .withConfiguration(config)
      .withExecutorService(executorService)
      .withScheduledExecutorService(executorService)
      .newInstance("", myclasspath.asJava, scalacOpts.asJava)
  }

  protected def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl().copy(
      snippetAutoIndent = false
    )

  protected def extraDependencies(scalaVersion: String): Seq[Dependency] =
    Seq.empty

  protected def scalacOptions(classpath: Seq[Path]): Seq[String] = Seq.empty

  protected def requiresJdkSources: Boolean = false

  protected def requiresScalaLibrarySources: Boolean = false

  protected def isScala3Version(scalaVersion: String): Boolean = {
    scalaVersion.startsWith("3.")
  }

  private def recursivelyDelete(path: Path): Unit = {
    Files.walk(path)
      .sorted(Comparator.reverseOrder)
      .map(_.toFile)
      .forEach(_.delete)
  }

  override def testSuiteFinished(description: Description): Unit =
    presentationCompiler.shutdown()
    recursivelyDelete(tmp)
    executorService.shutdown()

  def params(code: String, filename: String = "test.scala"): (String, Int) = {
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }
    (code2, offset)
  }

  def hoverParams(
      code: String,
      filename: String = "test.scala",
  ): (String, Int, Int) = {
    val code2 = code.replace("@@", "").replace("%<%", "").replace("%>%", "")
    val positionOffset =
      code.replace("%<%", "").replace("%>%", "").indexOf("@@")
    val startOffset = code.replace("@@", "").indexOf("%<%")
    val endOffset = code.replace("@@", "").replace("%<%", "").indexOf("%>%")
    (positionOffset, startOffset, endOffset) match {
      case (po, so, eo) if po < 0 && so < 0 && eo < 0 =>
        fail("missing @@ and (%<% and %>%)")
        (code2, so, eo)
      case (_, so, eo) if so >= 0 && eo >= 0 =>
        (code2, so, eo)
      case (po, _, _) =>
        (code2, po, po)
    }
  }

  def doc(e: JEither[String, MarkupContent]): String = {
    if (e == null) ""
    else if (e.isLeft) {
      " " + e.getLeft
    } else {
      " " + e.getRight.getValue
    }
  }.trim

  def sortLines(stableOrder: Boolean, string: String): String = {
    val strippedString = string.linesIterator.toList.filter(_.nonEmpty)
    if (stableOrder) strippedString.mkString("\n")
    else strippedString.sorted.mkString("\n")
  }

  extension (s: String) {
    def triplequoted: String = s.replace("'''", "\"\"\"")

    def removeRanges: String =
      s.replace("<<", "")
        .replace(">>", "")
        .replaceAll("/\\*.+\\*/", "")

    def removePos: String = s.replace("@@", "")
  }
}

