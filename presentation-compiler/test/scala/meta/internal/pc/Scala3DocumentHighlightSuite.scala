package scala.meta.internal.pc

import org.junit.Test

class Scala3DocumentHighlightSuite extends BaseDocumentHighlightSuite:

  @Test def `enum1` =
    check(
      """|enum FooEnum:
         |  case <<Ba@@r>>, Baz
         |val bar = FooEnum.<<Bar>>
         |""".stripMargin
    )

  @Test def `enum2` =
    check(
      """|enum FooEnum:
         |  case <<Bar>>, Baz
         |val bar = FooEnum.<<Ba@@r>>
         |""".stripMargin
    )

  @Test def `transparent1` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def <<foo>>(i: Int): Foo = new Bar
         |val iii = 123
         |val bar = <<f@@oo>>(iii)
         |""".stripMargin
    )

  @Test def `transparent2` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def <<f@@oo>>(i: Int): Foo = new Bar
         |val iii = 123
         |val bar = <<foo>>(iii)
         |""".stripMargin
    )

  @Test def `transparent3` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def foo(i: Int): Foo = new Bar
         |val <<ii@@i>> = 123
         |val bar = foo(<<iii>>)
         |""".stripMargin
    )

  @Test def `transparent4` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def foo(i: Int): Foo = new Bar
         |val <<iii>> = 123
         |val bar = foo(<<i@@ii>>)
         |""".stripMargin
    )

  @Test def `recursive-inline1` =
    check(
      """|inline def <<po@@wer>>(x: Double, n: Int): Double =
         |  if n == 0 then 1.0
         |  else if n == 1 then x
         |  else
         |    val y = <<power>>(x, n / 2)
         |    if n % 2 == 0 then y * y else y * y * x
         |""".stripMargin
    )

  @Test def `recursive-inline2` =
    check(
      """|inline def <<power>>(x: Double, n: Int): Double =
         |  if n == 0 then 1.0
         |  else if n == 1 then x
         |  else
         |    val y = <<po@@wer>>(x, n / 2)
         |    if n % 2 == 0 then y * y else y * y * x
         |""".stripMargin
    )

  @Test def `extension-params` =
    check(
      """|extension (<<sb@@d>>: String)
         |  def double = <<sbd>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-params-ref` =
    check(
      """|extension (<<sbd>>: String)
         |  def double = <<sb@@d>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param` =
    check(
      """|extension [T](<<x@@s>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<xs>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param-ref` =
    check(
      """|extension [T](<<xs>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<x@@s>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-with-type` =
    check(
      """|object Mincase:
         |  extension [X](x: X)
         |    def <<foobar>>(): Unit = ???
         |
         |  val x = 1.<<foo@@bar>>()
         |  val y = (1: Int).<<foobar>>()
         |""".stripMargin
    )

  @Test def `extension-complex` =
    check(
      """|object Extensions:
         |
         |  extension [A, B](<<eit@@hers>>: Seq[Either[A, B]])
         |    def sequence = <<eithers>>.partitionMap(identity) match
         |      case (Nil, rights)       => Right(rights)
         |      case (firstLeft :: _, _) => Left(firstLeft)
         |    def sequence2 = <<eithers>>.partitionMap(identity) match
         |      case (Nil, rights)       => Right(rights)
         |      case (firstLeft :: _, _) => Left(firstLeft)
         |
         |  extension (map: Map[String, String])
         |    def getOrLeft(key: String): Either[String, String] =
         |      map.get(key) match
         |        case None        => Left(s"Missing ${key} in }")
         |        case Some(value) => Right(value)
         |""".stripMargin
    )

  @Test def `given-synthetic1` =
    check(
      """|given (usi@@ng i: Int): Double = 4.0
         |val a = given_Double""".stripMargin
    )

  @Test def `given-synthetic2` =
    check(
      """|given (using i: Int): Double = 4.0
         |val a = <<given_Doub@@le>>""".stripMargin
    )

  @Test def `given-synthetic3` =
    check(
      """|given Int = 10
         |val a = <<giv@@en_Int>>""".stripMargin
    )

  @Test def `given-synthetic4` =
    check(
      """|given <<I@@nt>> = 10
         |val a = given_Int""".stripMargin
    )

  @Test def `given-not-synthetic1` =
    check(
      """|given <<`giv@@en_D`>>: Double = 4.0
         |val a = <<`given_D`>>""".stripMargin
    )

  @Test def `given-not-synthetic2` =
    check(
      """|given <<`given_D`>>:Double = 4.0
         |val a = <<`giv@@en_D`>>""".stripMargin
    )
