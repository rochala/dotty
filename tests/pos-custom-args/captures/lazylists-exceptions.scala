import language.experimental.saferExceptions
import annotation.unchecked.uncheckedVariance

trait LazyList[+A]:
  this: {*} LazyList[A] =>

  def isEmpty: Boolean
  def head: A
  def tail: {this} LazyList[A]

object LazyNil extends LazyList[Nothing]:
  def isEmpty: Boolean = true
  def head = ???
  def tail = ???

final class LazyCons[+T](val x: T, val xs: () => {*} LazyList[T]) extends LazyList[T]:
  this: {*} LazyList[T] =>

  var forced = false
  var cache: {this} LazyList[T @uncheckedVariance] = compiletime.uninitialized

  private def force =
    if !forced then
      cache = xs()
      forced = true
    cache

  def isEmpty = false
  def head = x
  def tail: {this} LazyList[T] = force
end LazyCons

extension [A](xs: {*} LazyList[A])
  def map[B](f: A => B): {xs, f} LazyList[B] =
    if xs.isEmpty then LazyNil
    else LazyCons(f(xs.head), () => xs.tail.map(f))

  def filter(p: A => Boolean): {xs, p} LazyList[A] =
    if xs.isEmpty then LazyNil
    else if p(xs.head) then LazyCons(xs.head, () => xs.tail.filter(p))
    else xs.tail.filter(p)

  def concat(ys: {*} LazyList[A]): {xs, ys} LazyList[A] =
    if xs.isEmpty then ys
    else LazyCons(xs.head, () => xs.tail.concat(ys))
end extension

class Ex1 extends Exception
class Ex2 extends Exception

def test(using cap1: CanThrow[Ex1], cap2: CanThrow[Ex2]) =
  val xs = LazyCons(1, () => LazyNil)

  def f(x: Int): Int throws Ex1 =
    if x < 0 then throw Ex1()
    x * x

  def g(x: Int): Int throws Ex1 =
    if x < 0 then throw Ex1()
    x * x

  def x1 = xs.map(f)
  def x1c: {cap1} LazyList[Int] = x1

  def x2 = x1.concat(xs.map(g).filter(_ > 0))
  def x2c: {cap1, cap2} LazyList[Int] = x2


