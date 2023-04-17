package scala.meta.internal.pc

import org.junit.Test

class CompletionBacktickSuite extends BaseCompletionSuite {

  @Test def `keyword` =
    check(
      """|object Main {
         |  val `type` = 42
         |  Main.typ@@
         |}
         |""".stripMargin,
      "type: Int",
      filterText = "type",
    )

  @Test def `keyword-edit` =
    checkEdit(
      """|object Main {
         |  val `type` = 42
         |  Main.typ@@
         |}
         |""".stripMargin,
      """|object Main {
         |  val `type` = 42
         |  Main.`type`
         |}
         |""".stripMargin,
      filterText = "type",
    )

  @Test def `space` =
    check(
      s"""|object Main {
          |  val `hello world` = 42
          |  Main.hello@@
          |}
          |""".stripMargin,
         "hello world: Int",
      filterText = "hello world",
    )

  @Test def `comment` =
    check(
      s"""|object Main {
          |  val `///` = 42
          |  Main./@@
          |}
          |""".stripMargin,
        "///: Int",
      filterText = "///",
    )

  @Test def `named-arg` =
    check(
      """|object Main {
         |  def foo(`type`: Int) = 42
         |  foo(type@@)
         |}
         |""".stripMargin,
      "",
      filterText = "type",
    )

  @Test def `normal` =
    check(
      """|object Main {
         |  val `spaced` = 42
         |  spaced@@
         |}
         |""".stripMargin,
      // NOTE(olafur) expected output is not backticked because the compiler symbol does not
      // distinguish if the symbol was defined with backticks in source.
      """spaced: Int
        |""".stripMargin,
      filterText = "",
    )

  @Test def `negative` =
    check(
      """|object Main {
         |  val `type` = 42
         |  Main.`typ@@
         |}
         |""".stripMargin,
      // NOTE(olafur) expected output is empty because the source does not tokenize due to unclosed identifier.
      // It would be nice to fix this limitation down the road.
      "",
      filter = _.contains("`type`"),
    )

  // https://dotty.epfl.ch/docs/internals/syntax.html#soft-keywords
  List("infix", "inline", "opaque", "open", "transparent", "as", "derives",
    "end", "extension", "throws", "using").foreach(softKeywordCheck)

  @Test
  private def softKeywordCheck(keyword: String) =
    checkEdit(
      s"'$keyword'-keyword-named-method-edit",
      s"""|object Main {
          |  def $keyword(a: String) = a
          |  ${keyword}@@
          |}
          |""".stripMargin,
      s"""|object Main {
          |  def $keyword(a: String) = a
          |  `$keyword`($$0)
          |}
          |""".stripMargin,
      filter = _.contains("a: String"),
    )

}
