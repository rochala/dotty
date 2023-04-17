package scala.meta.internal.pc

import org.junit.Test

class SignatureHelpDocSuite extends BaseSignatureHelpSuite {

  override def requiresJdkSources: Boolean = true

  override def requiresScalaLibrarySources: Boolean = true

  val foldLatestDocs: String =
    """|Returns the result of applying `f` to this [scala.Option](scala.Option)'s
       | value if the [scala.Option](scala.Option) is nonempty.  Otherwise, evaluates
       | expression `ifEmpty`.
       |
       |This is equivalent to:
       |
       |```
       |option match {
       |  case Some(x) => f(x)
       |  case None    => ifEmpty
       |}
       |```
       |This is also equivalent to:
       |
       |```
       |option map f getOrElse ifEmpty
       |```""".stripMargin

  val foldOlderDocs1: String =
    """|Returns the result of applying `f` to this [scala.Option](scala.Option)'s
       | value if the [scala.Option](scala.Option) is nonempty.  Otherwise, evaluates
       | expression `ifEmpty`.
       |
       |
       |**Notes**
       |- This is equivalent to `[scala.Option](scala.Option) map f getOrElse ifEmpty`.
       |
       |**Parameters**
       |- `ifEmpty`: the expression to evaluate if empty.
       |- `f`: the function to apply if nonempty.
       |fold[B](ifEmpty: => B)(f: Int => B): B
       |                       ^^^^^^^^^^^
       |  @param ifEmpty the expression to evaluate if empty.
       |  @param f the function to apply if nonempty.
       |""".stripMargin

  @Test def `curry` =
    checkDoc(
      """
        |object a {
        |  Option(1).fold("")(_ => @@)
        |}
      """.stripMargin,
      s"""$foldLatestDocs
         |**Parameters**
         |- `f`: the function to apply if nonempty.
         |- `ifEmpty`: the expression to evaluate if empty.
         |fold[B](ifEmpty: => B)(f: Int => B): B
         |                       ^^^^^^^^^^^
         |  @param ifEmpty the expression to evaluate if empty.
         |  @param f the function to apply if nonempty.
          """.stripMargin,
    )

  val foldOlderDocs2: String =
    """|Returns the result of applying `f` to this [scala.Option](scala.Option)'s
       | value if the [scala.Option](scala.Option) is nonempty.  Otherwise, evaluates
       | expression `ifEmpty`.
       |
       |
       |**Notes**
       |- This is equivalent to `[scala.Option](scala.Option) map f getOrElse ifEmpty`.
       |
       |**Parameters**
       |- `ifEmpty`: the expression to evaluate if empty.
       |- `f`: the function to apply if nonempty.
       |fold[B](ifEmpty: => B)(f: Int => B): B
       |        ^^^^^^^^^^^^^
       |  @param ifEmpty String the expression to evaluate if empty.
       |  @param f the function to apply if nonempty.
       |""".stripMargin

  @Test def `curry2` =
    checkDoc(
      """
        |object a {
        |  Option(1).fold("@@")
        |}
      """.stripMargin,
      s"""|$foldLatestDocs
          |**Parameters**
          |- `f`: the function to apply if nonempty.
          |- `ifEmpty`: the expression to evaluate if empty.
          |fold[B](ifEmpty: => B)(f: Int => B): B
          |        ^^^^^^^^^^^^^
          |  @param ifEmpty the expression to evaluate if empty.
          |  @param f the function to apply if nonempty.
          |""".stripMargin,
    )

  @Test def `curry3` =
    checkDoc(
      """
        |object a {
        |  List(1).foldLeft(0) {
        |   case @@
        |  }
        |}
      """.stripMargin,
    """|Applies a binary operator to a start value and all elements of this collection,
       | going left to right.
       |
       | Note: will not terminate for infinite-sized collections.
       | Note: might return different results for different runs, unless the
       |underlying collection type is ordered or the operator is associative
       |and commutative.
       |
       |
       |**Type Parameters**
       |- `B`: the result type of the binary operator.
       |
       |**Parameters**
       |- `z`: the start value.
       |- `op`: the binary operator.
       |
       |**Returns:** the result of inserting `op` between consecutive elements of this collection,
       |          going left to right with the start value `z` on the left:
       |          `op(...op(z, x), x, ..., x)` where `x, ..., x`
       |           are the elements of this collection.
       |          Returns `z` if this collection is empty.
       |foldLeft[B](z: B)(op: (B, Int) => B): B
       |                  ^^^^^^^^^^^^^^^^^
       |""".stripMargin,
    )

  @Test def `curry4` =
    checkDoc(
      """
        |object a {
        |  def curry(a: Int, b: Int)(c: Int) = a
        |  curry(1)(3@@)
        |}
      """.stripMargin,
      """|
         |curry(a: Int, b: Int)(c: Int): Int
         |                      ^^^^^^
         |""".stripMargin,
    )

  @Test def `canbuildfrom` =
    checkDoc(
      """
        |object a {
        |  List(1).map(x => @@)
        |}
      """.stripMargin,
      """|Builds a new collection by applying a function to all elements of this collection.
         |
         |
         |**Type Parameters**
         |- `B`: the element type of the returned collection.
         |
         |**Parameters**
         |- `f`: the function to apply to each element.
         |
         |**Returns:** a new collection resulting from applying the given function
         |               `f` to each element of this collection and collecting the results.
         |map[B](f: Int => B): List[B]
         |       ^^^^^^^^^^^
         |""".stripMargin,
    )

  @Test def `too-many` =
    checkDoc(
      """
        |object a {
        |  Option(1, 2, @@2)
        |}
      """.stripMargin,
          """|An Option factory which creates Some(x) if the argument is not null,
             | and None if it is null.
             |
             |
             |**Parameters**
             |- `x`: the value
             |
             |**Returns:** Some(value) if value != null, None if value == null
             |apply[A](x: A): Option[A]
             |         ^^^^
             |  @param x the value
             |""".stripMargin,
    )

  @Test def `java5` =
    checkDoc(
      """
        |object a {
        |  java.util.Collections.singleton(@@)
        |}
      """.stripMargin,
          """|Returns an immutable set containing only the specified object.
             |The returned set is serializable.
             |singleton[T](o: T): java.util.Set[T]
             |             ^^^^
             |  @param o o the sole object to be stored in the returned set.
             |""".stripMargin,
    )

  @Test def `default` =
    checkDoc(
      """
        |object A {
        |  new scala.util.control.Exception.Catch(@@)
        |}
      """.stripMargin,
          """|A container class for catch/finally logic.
             |
             | Pass a different value for rethrow if you want to probably
             | unwisely allow catching control exceptions and other throwables
             | which the rest of the world may expect to get through.
             |
             |**Type Parameters**
             |- `T`: result type of bodies used in try and catch blocks
             |
             |**Parameters**
             |- `fin`: Finally logic which if defined will be invoked after catch logic
             |- `rethrow`: Predicate on throwables determining when to rethrow a caught [Throwable](Throwable)
             |- `pf`: Partial function used when applying catch logic to determine result value
             |Catch[T](pf: scala.util.control.Exception.Catcher[T], fin: Option[scala.util.control.Exception.Finally], rethrow: Throwable => Boolean)
             |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
             |  @param pf Partial function used when applying catch logic to determine result value
             |  @param fin Finally logic which if defined will be invoked after catch logic
             |  @param rethrow Predicate on throwables determining when to rethrow a caught [Throwable](Throwable)
             |""".stripMargin,
    )

  @Test def `java` =
    check(
      """
        |object a {
        |  new java.io.File(@@)
        |}
      """.stripMargin,
     """|File(uri: java.net.URI)
        |     ^^^^^^^^^^^^^^^^^
        |File(parent: java.io.File, child: String)
        |File(parent: String, child: String)
        |File(pathname: String)
        |""".stripMargin
    )

  @Test def `java2` =
    check(
      """
        |object a {
        |  "".substring(1@@)
        |}
      """.stripMargin,
          """|substring(beginIndex: Int, endIndex: Int): String
             |substring(beginIndex: Int): String
             |          ^^^^^^^^^^^^^^^
             |""".stripMargin
    )

  @Test def `java3` =
    check(
      """
        |object a {
        |  String.valueOf(1@@)
        |}
      """.stripMargin,
          """|valueOf(d: Double): String
             |valueOf(f: Float): String
             |valueOf(l: Long): String
             |valueOf(i: Int): String
             |        ^^^^^^
             |valueOf(c: Char): String
             |valueOf(b: Boolean): String
             |valueOf(data: Array[Char], offset: Int, count: Int): String
             |valueOf(data: Array[Char]): String
             |valueOf(obj: Object): String
             |""".stripMargin
    )

  @Test def `java4` =
    check(
      """
        |object a {
        |  String.valueOf(@@)
        |}
      """.stripMargin,
          """|valueOf(d: Double): String
             |        ^^^^^^^^^
             |valueOf(f: Float): String
             |valueOf(l: Long): String
             |valueOf(i: Int): String
             |valueOf(c: Char): String
             |valueOf(b: Boolean): String
             |valueOf(data: Array[Char], offset: Int, count: Int): String
             |valueOf(data: Array[Char]): String
             |valueOf(obj: Object): String
             |""".stripMargin,
    )

  @Test def `ctor2` =
    checkDoc(
      """
        |object a {
        |  new Some(10@@)
        |}
      """.stripMargin,
          """|Class `Some[A]` represents existing values of type
             | `A`.
             |Some[A](value: A)
             |        ^^^^^^^^
             |""".stripMargin
    )

  @Test def `markdown` =
    checkDoc(
      """
        |object A {
        |  1.to(10).by(@@)
        |}
      """.stripMargin,
      // tests both @define and HTML expansion
      """|Create a new range with the `start` and `end` values of this range and
         | a new `step`.
         |
         |
         |**Returns:** a new range with a different step
         |by(step: Int): Range
         |   ^^^^^^^^^
         |""".stripMargin,
    )
}
