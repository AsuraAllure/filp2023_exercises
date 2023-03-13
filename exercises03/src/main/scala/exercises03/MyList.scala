package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {

  def sum(list: MyList[Int]): Int = summing(list, 0)
  @tailrec
  private def summing(list: MyList[Int], result: Int): Int = list match {
    case Nil              => result
    case Cons(head, tail) => summing(tail, result + head)
  }

  def reverse[A](list: MyList[A]): MyList[A] = ???
}
