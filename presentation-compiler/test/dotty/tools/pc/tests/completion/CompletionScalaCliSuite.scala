package dotty.tools.pc.tests.completion

import org.junit.Test
import dotty.tools.pc.base.BaseCompletionSuite

class CompletionScalaCliSuite extends BaseCompletionSuite:

  @Test def `simple` =
    check(
      """|//> using lib "io.cir@@
         |package A
         |""".stripMargin,
      "io.circe"
    )

  @Test def `multiple-deps` =
    checkEdit(
      """|// Multiple using lib
         |//> using lib ???
         |// //> using lib ???
         |//> using lib io.circe::circe-core_na@@
         |package A
         |""".stripMargin,
      """|// Multiple using lib
         |//> using lib ???
         |// //> using lib ???
         |//> using lib io.circe::circe-core_native0.4
         |package A
         |""".stripMargin
    )

  @Test def `single-colon` =
    check(
      """|//> using lib "io.circe:circe-core_na@@
         |package A
         |""".stripMargin,
      """|circe-core_native0.4_2.12
         |circe-core_native0.4_2.13
         |circe-core_native0.4_3
         |""".stripMargin
    )

  @Test def `version` =
    check(
      """|//> using lib "io.circe::circe-core_sjs1:0.14.1@@"
         |package A
         |""".stripMargin,
      "0.14.1"
    )

  @Test def `multiple-libs` =
    check(
      """|//> using lib "io.circe::circe-core:0.14.0", "io.circe::circe-core_na@@"
         |package A
         |""".stripMargin,
      "circe-core_native0.4"
    )

  @Test def `script` =
    check(
      scriptWrapper(
        """|//> using lib "io.circe:circe-core_na@@
           |
           |""".stripMargin,
        "script.sc.scala"
      ),
      """|circe-core_native0.4_2.12
         |circe-core_native0.4_2.13
         |circe-core_native0.4_3
         |""".stripMargin,
      filename = "script.sc.scala",
      enablePackageWrap = false
    )

  @Test def `closing-quote` =
    check(
      """|//> using lib "io.circe::circe-core:0.14.0"@@
         |package A
         |""".stripMargin,
      ""
    )

  @Test def `whitespace` =
    check(
      """|//> using lib "io.circe::circe-co @@
         |package A
         |""".stripMargin,
      ""
    )

  @Test def `alternative-sorting` =
    checkEdit(
      """|//> using lib "co.fs2::fs2-core:@@"
         |package A
         |""".stripMargin,
      """|//> using lib "co.fs2::fs2-core:3.4.0"
         |package A
         |""".stripMargin,
      filter = _.startsWith("3.4")
    )

  @Test def `dep` =
    check(
      """|//> using dep "io.cir@@
         |package A
         |""".stripMargin,
      "io.circe"
    )

  @Test def `multiple-deps2` =
    check(
      """|//> using libs "io.circe::circe-core:0.14.0", "io.circe::circe-core_na@@"
         |package A
         |""".stripMargin,
      "circe-core_native0.4"
    )

  private def scriptWrapper(code: String, filename: String): String =
    // Vaguely looks like a scala file that ScalaCLI generates
    // from a sc file.
    s"""|
        |object ${filename.stripSuffix(".sc.scala")} {
        |/*<script>*/${code}
        |}
        |""".stripMargin
