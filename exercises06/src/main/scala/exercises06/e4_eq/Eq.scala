package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {
  implicit val intEq: Eq[Int]         = (a: Int, b: Int) => a == b
  implicit val booleanEq: Eq[Boolean] = (a: Boolean, b: Boolean) => a == b

  implicit def listEq[A](implicit aEq: Eq[A]): Eq[List[A]] = (a: List[A], b: List[A]) => {
    a.length == b.length &&
      a.zip(b).forall { case (a, b) => aEq.eqv(a, b) }
  }

  implicit def optionEq[A](implicit aEq: Eq[A]): Eq[Option[A]] =
    (op1: Option[A], op2: Option[A]) =>
      (op1, op2) match {
        case (None, None)             => true
        case (Some(val1), Some(val2)) => aEq.eqv(val1, val2)
        case _                        => false
      }
}

object EqSyntax {
  implicit class EqOps[A](a: A) {
    def eqv(b: A)(implicit eq: Eq[A]): Boolean = eq.eqv(a, b)

    def ===(b: A)(implicit eq: Eq[A]): Boolean = eq.eqv(a, b)

    def !==(b: A)(implicit eq: Eq[A]): Boolean = !eq.eqv(a, b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  // 1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
