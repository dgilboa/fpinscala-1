package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maxInt(x:Int, y: Int): Int = if (x>y) x else y
  def maxTree(t: Tree[Int]): Int = maximumGeneric(t)(maxInt)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, r) => maxInt(maximum(left), maximum(r))
  }

  def maximumGeneric[A](t: Tree[A])(fmax: (A, A) => A): A = t match {
    case Leaf(x) => x
    case Branch(l, r) => fmax(maximumGeneric(l)(fmax), maximumGeneric(r)(fmax))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + ((depth(l) max depth(r)))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeUsingFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumUsingFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthUsingFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ max _)

  def mapUsingFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def leaf[A](a: A): Tree[A] = Leaf(A)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  /** this uses the leaf() which returns Tree[A] and not Leaf[A] and therefore in the currying here
    * the compiler knows that the return of the first function is Tree[B] and not only Leaf[B] */
  def mapUsingFoldWithNoTypeAnnotation[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => leaf(f(a)))(branch(_, _))

}