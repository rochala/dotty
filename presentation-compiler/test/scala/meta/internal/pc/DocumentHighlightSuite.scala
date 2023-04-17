package scala.meta.internal.pc

import org.junit.Test


class DocumentHighlightSuite extends BaseDocumentHighlightSuite {

  @Test def `single` =
    check(
      """
        |object Main {
        |  Option(1).<<he@@ad>>
        |}""".stripMargin,
    )

  @Test def `multiple` =
    check(
      """
        |object Main {
        |  val <<abc>> = 123
        |  <<abc>>.toInt
        |  println(<<ab@@c>>)
        |}""".stripMargin,
    )

  @Test def `multiple2` =
    check(
      """
        |object Main {
        |  val <<a@@bc>> = 123
        |  <<abc>>.toInt
        |  println(<<abc>>)
        |}""".stripMargin,
    )

  @Test def `multiple3` =
    check(
      """
        |object Main {
        |  val <<abc>> = 123
        |  <<ab@@c>>.toInt
        |  println(<<abc>>)
        |}""".stripMargin,
    )

  @Test def `different-symbols` =
    check(
      """
        |object Main {
        |  val abc = 123
        |  abc.<<to@@Int>>
        |  134l.toInt
        |}""".stripMargin,
    )

  @Test def `scopes` =
    check(
      """
        |object Main {
        |  val <<@@a>> = 123
        |  val f = (a: Int) => a + 1
        |  println(<<a>>)
        |}""".stripMargin,
    )

  @Test def `scopes2` =
    check(
      """
        |object Main {
        |  val <<a>> = 123
        |  val f = (a: Int) => a + 1
        |  println(<<@@a>>)
        |}""".stripMargin,
    )

  @Test def `params` =
    check(
      """
        |case class User(<<n@@ame>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `params2` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<na@@me>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `params3` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<n@@ame>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `params4` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<na@@me>> = "John")
        |}""".stripMargin,
    )

  @Test def `object` =
    check(
      """
        |case class <<U@@ser>>(name: String)
        |object <<User>>
        |object Main {
        |  val user = <<User>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin,
    )

  @Test def `object2` =
    check(
      """
        |case class <<User>>(name: String)
        |object <<Us@@er>>
        |object Main {
        |  val user = <<User>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin,
    )

  @Test def `object3` =
    check(
      """
        |case class <<User>>(name: String)
        |object <<User>>
        |object Main {
        |  val user = <<U@@ser>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin,
    )

  @Test def `case-class-var` =
    check(
      """
        |case class User(var <<na@@me>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `case-class-var2` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<na@@me>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `case-class-var3` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<n@@ame>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `case-class-var4` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<na@@me>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin,
    )

  @Test def `case-class-var5` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<na@@me>> = "John")
        |}""".stripMargin,
    )

  @Test def `var` =
    check(
      """
        |object Main {
        |  var <<ab@@d>> = 123
        |  <<abd>> = 344
        |  <<abd>> +=1
        |  println(<<abd>>)
        |}""".stripMargin,
    )

  @Test def `var2` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<ab@@d>> = 344
        |  <<abd>> +=1
        |  println(<<abd>>)
        |}""".stripMargin,
    )

  @Test def `var3` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<abd>> = 344
        |  <<ab@@d>> +=1
        |  println(<<abd>>)
        |}""".stripMargin,
    )

  @Test def `var4` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<abd>> = 344
        |  <<abd>> +=1
        |  println(<<a@@bd>>)
        |}""".stripMargin,
    )

  @Test def `overloaded` =
    check(
      """
        |object Main {
        |  def hello() = ""
        |  def <<hel@@lo>>(a : Int) = ""
        |  def hello(a : Int, b : String) = ""
        |}""".stripMargin,
    )

  @Test def `local-var` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<a@@bc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-var2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<ab@@c>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-var3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-assign` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<ab@@c>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-assign2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<a@@bc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-assign3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-class` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<ab@@c>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-class2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<a@@bc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `local-class3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin,
    )

  @Test def `setter-getter` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<ar@@g_=>> (arg: Int) = {}
        |    def <<arg>> = 1
        |  }
        |  val t = new T1
        |  t.<<arg>> = 123
        |}""".stripMargin,
    )

  @Test def `setter-getter2` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<arg_=>> (arg: Int) = {}
        |    def <<a@@rg>> = 1
        |  }
        |  val t = new T1
        |  t.<<arg>> = 123
        |
        |}""".stripMargin,
    )

  @Test def `setter-getter3` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<arg_=>> (arg: Int) = {}
        |    def <<arg>> = 1
        |  }
        |  val t = new T1
        |  t.<<ar@@g>> = 123
        |}""".stripMargin,
    )

  @Test def `same-name` =
    check(
      """
        |object Test {
        |  def foo(name: String) = ???
        |  def bar(<<n@@ame>>: String) = ???
        |  foo(name = "123")
        |  bar(<<name>> = "123")
        |}""".stripMargin,
    )

  @Test def `same-name2` =
    check(
      """
        |object Test {
        |  def foo(name: String) = ???
        |  def bar(<<name>>: String) = ???
        |  foo(name = "123")
        |  bar(<<na@@me>> = "123")
        |}""".stripMargin,
    )

  @Test def `same-name3` =
    check(
      """
        |object Test {
        |  def foo(<<na@@me>>: String) = ???
        |  def bar(name: String) = ???
        |  foo(<<name>> = "123")
        |  bar(name = "123")
        |}""".stripMargin,
    )

  @Test def `same-name4` =
    check(
      """
        |object Test {
        |  def foo(<<name>>: String) = ???
        |  def bar(name: String) = ???
        |  foo(<<na@@me>> = "123")
        |  bar(name = "123")
        |}""".stripMargin,
    )

  @Test def `import1` =
    check(
      """
        |import scala.util.<<Tr@@y>>
        |object Test {
        |   <<Try>>(1)
        |}""".stripMargin,
    )

  @Test def `import2` =
    check(
      """
        |import scala.util.<<Try>>
        |object Test {
        |   <<Tr@@y>>(1)
        |}""".stripMargin,
    )

  @Test def `import3` =
    check(
      """
        |import scala.<<ut@@il>>.Try
        |object Test {
        |   scala.<<util>>.Try(1)
        |}""".stripMargin,
    )

  @Test def `import4` =
    check(
      """
        |import scala.<<util>>.Try
        |object Test {
        |   scala.<<ut@@il>>.Try(1)
        |}""".stripMargin,
    )

  @Test def `rename1` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATr@@y>>}
        |object Test {
        |   <<ATry>>(1)
        |}""".stripMargin,
    )

  @Test def `rename2` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATry>>}
        |object Test {
        |   <<ATr@@y>>(1)
        |}""".stripMargin,
    )

  // @note, we could try and not highlight normal Try,
  // but this might still be useful
  @Test def `rename3` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATr@@y>>}
        |object Test {
        |   scala.util.<<Try>>(1)
        |}""".stripMargin,
    )

  @Test def `rename4` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATry>>}
        |object Test {
        |   scala.util.<<Tr@@y>>(1)
        |}""".stripMargin,
    )

  @Test def `rename5` =
    check(
      """
        |import scala.util.{ <<T@@ry>> => <<ATry>>}
        |object Test {
        |   scala.util.<<Try>>(1)
        |}""".stripMargin,
    )

  @Test def `case-match1` =
    check(
      """
        |import scala.util.Try
        |import scala.util.Success
        |object Test {
        |   Try(1) match {
        |     case Success(<<va@@lue>>) =>
        |       <<value>>
        |   }
        |}""".stripMargin,
    )

  @Test def `case-match2` =
    check(
      """
        |import scala.util.Try
        |import scala.util.Success
        |object Test {
        |   Try(1) match {
        |     case Success(<<value>>) =>
        |       <<va@@lue>>
        |   }
        |}""".stripMargin,
    )

  @Test def `inner-class1` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<U@@ser>>(name: String)
         |    object <<User>>{ def nnn = ""}
         |    <<User>>.nnn
         |  }
         |}""".stripMargin,
    )

  @Test def `inner-class2` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<User>>(name: String)
         |    object <<U@@ser>>{ def nnn = ""}
         |    <<User>>.nnn
         |  }
         |}""".stripMargin,
    )

  @Test def `inner-class3` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<User>>(name: String)
         |    object <<User>>{ def nnn = ""}
         |    <<Use@@r>>.nnn
         |  }
         |}""".stripMargin,
    )

  @Test def `inner-class4` =
    check(
      """|object Main {
         |  def foo = {
         |    object O {
         |      case class <<User>>(name: String)
         |      object <<User>>{ def nnn = ""}
         |      <<Use@@r>>.nnn
         |    }
         |  }
         |}""".stripMargin,
    )

  @Test def `package-object` =
    check(
      """|package example
         |
         |package object <<nes@@ted>> {
         |
         |  class PackageObjectNestedClass
         |
         |}
         |""".stripMargin,
    )

  @Test def `named-param` =
    check(
      """|object Main {
         |  def foo = {
         |      case class User(<<name>>: String)
         |      val a = User(<<na@@me>> = "abc")
         |  }
         |}""".stripMargin,
    )

  @Test def `backtick` =
    check(
      """|object Main {
         |  val <<`hi-!`>> = 5
         |
         |  <<`hi@@-!`>> + 3
         |}""".stripMargin,
    )

  @Test def `shadowing` =
    check(
      """|object Main {
         |  val abc = {
         |    val <<abc>> = 1
         |    <<a@@bc>> + 1
         |  }
         |  val d = abc + 1
         |}""".stripMargin,
    )

  @Test def `select-parentheses` =
    check(
      """|object Main {
         |  val a = (1 + 2 + 3).<<toStr@@ing>>
         |}""".stripMargin,
    )

  @Test def `select-parentheses2` =
    check(
      """|object Main {
         |  val a = (1 + 2 + 3) <<:@@:>> Nil
         |}""".stripMargin,
    )

  @Test def `trailling-comma` =
    check(
      """
        |object Main {
        |  val a = 1
        |  val <<b>> = 2
        |  List(
        |    a,
        |    <<b@@>>,
        |  )
        |}""".stripMargin,
    )

  @Test def `trailling-comma2` =
    check(
      """
        |object Main {
        |  val a = 1
        |  val <<`ab`>> = 2
        |  List(
        |    a,
        |    <<`ab@@`>>,
        |  )
        |}""".stripMargin,
    )
}
