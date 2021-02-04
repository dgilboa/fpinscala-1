package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise
  /**
    * This is actually not a stack safe solution - for large inputs it will stack overflow cause
    * it's not tail recursive, it's a simple recursive function,
    * to make it tail recursive it needs to be with an internal function.
  */
  def toList(): List[A] = this match {
    case Cons(h, t) => h() :: t().toList()
    case _ => List()
  }

  def toListTailRecursive: List[A] = {
    @annotation.tailrec
    def recursive(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => recursive(t(), h() :: acc)
      case _ => acc
    }
    recursive(this, List.empty[A]).reverse
  }

  // Exercise
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Exercise
  def drop(n: Int): Stream[A] = dropSol1(n)

  def dropSol1(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ =>  this
  }

  def dropSol2(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(h, t) if n == 1 => t()
    case Cons(h, t) if n == 0 => cons(h(), t())
    case _ =>  empty
  }

  // Exercise
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = headOptionSol1

  def headOptionSol1: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case _ => None
  }

  def headOptionSol2: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => Cons(() => f(h()), () => t().map(f))
    case Empty => empty[B]
  }

  def mapUsingFoldRight[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](bs: => Stream[B]): Stream[B] = foldRight(bs)((h, t) => cons(h, t))

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = {
    ???
  }
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // Exercises
  def fibs(): Stream[Int] = {
    def nextfib(h: Int ,t: Int):Stream[Int] =
      cons(h, nextfib(t, h + t))
    nextfib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    lazy val maybeA = f(z)
    maybeA match {
      case Some(a) => Cons(a._1, unfold())
      case None => empty[A]
    }
  }

}