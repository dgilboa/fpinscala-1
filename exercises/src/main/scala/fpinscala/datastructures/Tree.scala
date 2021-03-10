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

}