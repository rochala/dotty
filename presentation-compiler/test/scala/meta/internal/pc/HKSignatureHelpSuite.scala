package scala.meta.internal.pc

import coursierapi._
import org.junit.Test

class HKSignatureHelpSuite extends BaseSignatureHelpSuite {

  // override def extraDependencies(scalaVersion: String): Seq[Dependency] = {
  //   val binaryVersion = createBinaryVersion(scalaVersion)
  //   Seq(Dependency.of("org.typelevel", s"cats-core_$binaryVersion", "2.8.0"))

  // }

  @Test def `foldmap` =
    check(
      """import cats.implicits._
        |import cats._
        |object a {
        |  Foldable[Option].foldMap(a @@)
        |}
        |""".stripMargin,
     """|foldMap[A, B](fa: Option[A])(f: A => B)(using B: cats.kernel.Monoid[B]): B
        |              ^^^^^^^^^^^^^
        |""".stripMargin
    )

  // https://github.com/scalameta/metals/issues/5055
  @Test def `named` =
    check(
      """|
         |object demo {
         |  val f: Int = fun(
         |    logHeaders = true,
         |    logBody = true,
         |    logAction = None,
         |    @@
         |  )
         |  def defaultRedactHeadersWhen(name: String): Boolean = false
         |
         |  /**
         |    *
         |    *
         |    * @param logHeaders
         |    * @param logBody
         |    * @param redactHeadersWhen test description
         |    * @param logAction
         |    * @return
         |    */
         |  def fun(
         |    logHeaders: Boolean,
         |    logBody: Boolean,
         |    redactHeadersWhen: String => Boolean = defaultRedactHeadersWhen,
         |    logAction: Option[String => Unit] = None,
         |  ): Int = ???
         |}
         |""".stripMargin,
      """|**Parameters**
         |- `redactHeadersWhen`: test description
         |fun(<logHeaders: Boolean>, <logBody: Boolean>, <logAction: Option[String => Unit] = None>, <redactHeadersWhen: String => Boolean = defaultRedactHeadersWhen>): Int
         |                                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         |  @param <redactHeadersWhen test description
         |""".stripMargin,
      includeDocs = true,
    )

}
