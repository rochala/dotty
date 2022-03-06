class CC
type Cap = {*} CC

trait LazyList[+A]:
  this: ({*} LazyList[A]) =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

final class LazyCons[+T](val x: T, val xs: {*} () => {*} LazyList[T]) extends LazyList[T]:
  this: ({*} LazyList[T]) =>

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[T] = xs()

extension [A](xs: {*} LazyList[A])
  def map[B](f: {*} A => B): {xs, f} LazyList[B] =
    if xs.isEmpty then LazyNil
    else LazyCons(f(xs.head), () => xs.tail.map(f))

def test(cap1: Cap, cap2: Cap) =
  def f(x: String): String = if cap1 == cap1 then "" else "a"
  def g(x: String): String = if cap2 == cap2 then "" else "a"

  val xs = LazyCons("", () => if f("") == f("") then LazyNil else LazyNil)
  val xsc: {cap1} LazyList[String] = xs
  val ys = xs.map(g)
  val ysc: {cap1, cap2} LazyList[String] = ys
