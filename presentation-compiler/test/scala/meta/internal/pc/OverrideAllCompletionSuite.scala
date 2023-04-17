package scala.meta.internal.pc

import org.junit.Test


class CompletionOverrideAllSuite extends BaseCompletionSuite {

  override def requiresJdkSources: Boolean = true

  @Test def `simple` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (2 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `simple-edit` =
    checkEdit(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo: Int = ${0:???}
         |
         |    def bar: Int = ${0:???}
         |
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `simple-three` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |  def car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (3 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `simple-three-edit` =
    checkEdit(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |  def car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """
        |package example
        |
        |trait Foo {
        |  def foo: Int
        |  def bar: Int
        |  def car: Int
        |}
        |object Main {
        |  val x = new Foo {
        |    def foo: Int = ${0:???}
        |
        |    def bar: Int = ${0:???}
        |
        |    def car: Int = ${0:???}
        |
        |  }
        |}
        |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `two-left` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |  def car: Int
         |}
         |object Main {
         |  val x = new Foo {
             def foo = 2
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (2 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `two-left-edit` =
    checkEdit(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |  def car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo = 2
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def bar: Int
         |  def car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo = 2
         |    def bar: Int = ${0:???}
         |
         |    def car: Int = ${0:???}
         |
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `none-with-one` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      "",
      filter = _.contains("Implement"),
    )

  @Test def `same-scope` =
    check(
      """|package a { class Foo }
         |package b { class Foo }
         |
         |trait Foo {
         |  def one: a.Foo
         |  def two: b.Foo
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (2 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `same-scope-edit` =
    checkEdit(
      """|package a { class Foo }
         |package b { class Foo }
         |
         |trait Foo {
         |  def one: a.Foo
         |  def two: b.Foo
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|package a { class Foo }
         |package b { class Foo }
         |
         |trait Foo {
         |  def one: a.Foo
         |  def two: b.Foo
         |}
         |object Main {
         |  val x = new Foo {
         |    def one: a.Foo = ${0:???}
         |
         |    def two: b.Foo = ${0:???}
         |
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `include-val-var` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  val bar: Int
         |  var car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (3 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `include-val-var-edit` =
    checkEdit(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  val bar: Int
         |  var car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  val bar: Int
         |  var car: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo: Int = ${0:???}
         |
         |    val bar: Int = ${0:???}
         |
         |    var car: Int = ${0:???}
         |
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `mixed-partial` =
    check(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def foo2: Int
         |  var bar: Int
         |  var bar2: Int
         |  val car: Int
         |  val car2: Int
         |}
         |object Main {
         |  val x = new Foo {
         |   def foo: int = 3
         |   var bar: int = 2
         |   val car: Int = 1
         |   def@@
         |  }
         |}
         |""".stripMargin,
      """|Implement all members (3 total)
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `mixed-partial-edit` =
    checkEdit(
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def foo2: Int
         |  var bar: Int
         |  var bar2: Int
         |  val car: Int
         |  val car2: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo: int = 3
         |    var bar: int = 2
         |    val car: Int = 1
         |    def@@
         |  }
         |}
         |""".stripMargin,
      """|package example
         |
         |trait Foo {
         |  def foo: Int
         |  def foo2: Int
         |  var bar: Int
         |  var bar2: Int
         |  val car: Int
         |  val car2: Int
         |}
         |object Main {
         |  val x = new Foo {
         |    def foo: int = 3
         |    var bar: int = 2
         |    val car: Int = 1
         |    def foo2: Int = ${0:???}
         |
         |    var bar2: Int = ${0:???}
         |
         |    val car2: Int = ${0:???}
         |
         |  }
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )

  @Test def `java` =
    checkEdit(
      """|package example
         |
         |import java.io.Externalizable
         |
         |object Main extends Externalizable {
         |  def@@
         |}
         |""".stripMargin,
      """|package example
         |
         |import java.io.Externalizable
         |import java.io.ObjectOutput
         |import java.io.ObjectInput
         |
         |object Main extends Externalizable {
         |  def writeExternal(out: ObjectOutput): Unit = ${0:???}
         |
         |  def readExternal(in: ObjectInput): Unit = ${0:???}
         |
         |}
         |""".stripMargin,
      filter = _.contains("Implement"),
    )
}
