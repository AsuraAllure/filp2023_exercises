package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {

  def sum(list: MyList[Int]): Int = {
    @tailrec
    def summing(list: MyList[Int], result: Int): Int = list match {
      case Nil              => result
      case Cons(head, tail) => summing(tail, result + head)
    }
    summing(list, 0)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def reverseTailRec[A](list: MyList[A], accumulating: MyList[A]): MyList[A] = list match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (tail == Nil) {
          Cons(head, accumulating)
        } else {
          reverseTailRec(tail, Cons(head, accumulating))
        }
    }
    reverseTailRec(list, Nil)
  }

}
