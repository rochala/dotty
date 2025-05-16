package dotty.tools.pc.tests

import scala.language.unsafeNulls

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.utils.RangeReplace

import java.net.URI
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken

import org.junit.Test
import org.eclipse.lsp4j.DiagnosticSeverity
import dotty.tools.pc.utils.TestExtensions.getOffset

class DiagnosticProviderSuite extends BasePCSuite with RangeReplace {
  case class TestDiagnostic(startIndex: Int, endIndex: Int, msg: String, severity: DiagnosticSeverity)

  def check(
    text: String,
    expected: List[TestDiagnostic]
  ): Unit =
    val diagnostics = presentationCompiler
      .didChange(CompilerVirtualFileParams(URI.create("file:/Diagnostic.scala"), text, EmptyCancelToken))
      .get()
      .asScala

    val actual = diagnostics.map(d => TestDiagnostic(d.getRange().getStart().getOffset(text), d.getRange().getEnd().getOffset(text), d.getMessage(), d.getSeverity()))
    assertEquals(expected, actual, s"Expected [${expected.mkString(", ")}] but got [${actual.mkString(", ")}]")

  @Test def error =
    check(
      """|type VeryHardType = Int
         |class Bar(i: VeryHardTyp)
         |""".stripMargin,
      List(TestDiagnostic(37, 48, "Not found: type VeryHardTyp", DiagnosticSeverity.Error))
    )

  @Test def warning =
    check(
      """|object M:
         |  ""
         |""".stripMargin,
      List(TestDiagnostic(12, 14, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning))
    )

  @Test def mixed =
    check(
      """|type VeryHardType = Int
         |class Bar(i: VeryHardTyp)
         |object M:
         |  ""
         |""".stripMargin,
      List(
        TestDiagnostic(37 ,48, "Not found: type VeryHardTyp", DiagnosticSeverity.Error),
        TestDiagnostic(62, 64, "A pure expression does nothing in statement position", DiagnosticSeverity.Warning)
      )
    )

}
