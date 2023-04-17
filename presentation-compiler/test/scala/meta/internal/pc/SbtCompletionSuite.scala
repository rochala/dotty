package scala.meta.internal.pc

import org.junit.Test


class CompletionSbtLibSuite extends BaseCompletionSuite {

  @Test def `source` =
    check(
      """|val dependency = "io.cir@@" %
         |""".stripMargin,
      """|io.circe
         |""".stripMargin,
      filename = "A.sbt",
    )

  @Test def `single-percent` =
    check(
      """|val dependency = "io.circe" % "circe-core_na@@"
         |""".stripMargin,
      """|circe-core_native0.4_2.12
         |circe-core_native0.4_2.13
         |circe-core_native0.4_3
         |""".stripMargin,
      filename = "A.sbt",
    )

  @Test def `double-percent` =
    check(
      """|val dependency = "io.circe" %% "circe-core@@"
         |""".stripMargin,
      """|circe-core
         |circe-core_native0.4
         |circe-core_sjs0.6
         |circe-core_sjs1
         |circe-core_sjs1.0-RC2
         |""".stripMargin,
      filename = "A.sbt",
    )

  @Test def `version` =
    check(
      """|val dependency = "io.circe" %% "circe-core_sjs1" % "0.13@@"
         |""".stripMargin,
      """|0.13.0
         |""".stripMargin,
      filename = "A.sbt",
    )

  @Test def `double-percent-edit` =
    checkEdit(
      """|val dependency = "io.circe" %% "circe-core_n@@"
         |""".stripMargin,
      """|val dependency = "io.circe" %% "circe-core_native0.4"
         |""".stripMargin,
      filename = "A.sbt",
    )
}
