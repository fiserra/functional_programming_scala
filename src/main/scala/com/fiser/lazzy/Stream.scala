package com.fiser.lazzy

import com.fiser.lazzy.Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  def toList: List[A] = {
    var buff = new ListBuffer[A]()

    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) => buff += h(); go(t())
      case _ => buff.toList
    }
    go(this)
  }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (h, t) =>
      if (p(h)) cons(h, t)
      else empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def headOption(): Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) {
    (h, t) => cons(f(h), t)
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) {
    (h, t) =>
      if (f(h)) cons(h, t)
      else t
  }

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption()


  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
