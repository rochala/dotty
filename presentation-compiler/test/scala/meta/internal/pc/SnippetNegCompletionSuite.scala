package scala.meta.internal.pc

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig
import org.junit.Test

class CompletionSnippetNegSuite extends BaseCompletionSuite {

  override def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl(
      isCompletionSnippetsEnabled = false
    )

  @Test def `member` =
    checkSnippet(
      """
        |object Main {
        |  List.appl@@
        |}
        |""".stripMargin,
      "apply",
    )

  @Test def `scope` =
    checkSnippet(
      """
        |object Main {
        |  printl@@
        |
        |}
        |""".stripMargin,
      """|println()
         |println
         |""".stripMargin,
    )

  @Test def `java-nullary` =
    checkSnippet(
      """
        |class Foo {
        |  override def toString = "Foo"
        |}
        |object Main {
        |  new Foo().toStrin@@
        |
        |}
        |""".stripMargin,
      // even if `Foo.toString` is nullary, it overrides `Object.toString()`
      // which is a Java non-nullary method with an empty parameter list.
      "toString",
    )

  @Test def `type` =
    checkSnippet(
      s"""|object Main {
          |  val x: scala.IndexedSe@@
          |}
          |""".stripMargin,
      // It's expected to have two separate results, one for `object IndexedSeq` and one for `type IndexedSeq[T]`.
      """|IndexedSeq
         |IndexedSeq
         |""".stripMargin,
    )

}
