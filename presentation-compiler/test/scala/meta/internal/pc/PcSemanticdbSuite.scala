package scala.meta.internal.pc

import java.net.URI
import dotty.tools.dotc.semanticdb.TextDocument
import org.junit.Test


class PcSemanticdbSuite extends BasePCSuite {

  override def requiresJdkSources: Boolean = true

  override def requiresScalaLibrarySources: Boolean = true

  @Test def `simple` =
    check(
      """|package a
         |
         |object O {
         |  val a = 123
         |  val b = a + 1
         |}""".stripMargin,
      """|package a
         |
         |object O/*a.O.*/ {
         |  val a/*a.O.a.*/ = 123
         |  val b/*a.O.b.*/ = a/*a.O.a.*/ +/*scala.Int#`+`(+4).*/ 1
         |}
         |""".stripMargin,
    )

  @Test def `worksheet` =
    check(
      """|import $ivy.`org.kohsuke:github-api:1.114`
         |
         |object O {
         |  val a = 123
         |  val b = a + 1
         |}""".stripMargin,
      // local0 comes most likely from the script object use to wrap ScriptSource
      """|import $ivy.`org.kohsuke:github-api:1.114`
         |
         |object O/*_empty_.O.*/ {
         |  val a/*_empty_.O.a.*/ = 123
         |  val b/*_empty_.O.b.*/ = a/*_empty_.O.a.*/ +/*scala.Int#`+`(+4).*/ 1
         |}
         |""".stripMargin,
      filename = "A.worksheet.sc",
    )

  def check(
      original: String,
      expected: String,
      filename: String = "A.scala",
  ): Unit = {
    val uri = new URI(s"file:///$filename")
    val doc = presentationCompiler.semanticdbTextDocument(uri, original)

    val document = TextDocument.parseFrom(doc.get())
    val obtained = document.withText(original).toString
    assertNoDiff(expected, obtained)
  }
}
