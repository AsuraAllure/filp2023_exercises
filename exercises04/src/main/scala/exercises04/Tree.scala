package exercises04

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value)         => value
    case Branch(left, right) => Math.max(max(left), max(right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => Math.max(depth(left), depth(right)) + 1
  }

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}
