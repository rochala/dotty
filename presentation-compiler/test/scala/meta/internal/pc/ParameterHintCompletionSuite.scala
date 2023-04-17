package scala.meta.internal.pc


import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig
import org.junit.Test

class ParameterHintCompletionSuite extends BaseCompletionSuite {

  override def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl(
      _parameterHintsCommand = Some("hello")
    )

  @Test def `command` =
    checkItems(
      """
        |object Main {
        |  "".stripSuffi@@
        |}
      """.stripMargin,
      { case Seq(item) =>
        item.getCommand.getCommand == "hello"
      },
    )

  @Test def `command2` =
    checkItems(
      """
        |object Main {
        |  println@@
        |}
      """.stripMargin,
      { case Seq(item1, item2) =>
        item1.getCommand == null &&
        item2.getCommand.getCommand == "hello"
      },
    )
}
