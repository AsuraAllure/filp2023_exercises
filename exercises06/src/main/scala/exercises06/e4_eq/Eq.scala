package exercises06.e4_eq
trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}
object Eq {}
object EqInstances {
  implicit val intEq: Eq[Int]         = (a, b) => a == b
  implicit val booleanEq: Eq[Boolean] = (a, b) => a == b

  import EqSyntax._
  implicit def listEq[A: Eq]: Eq[List[A]] = (a, b) => {
    a.length == b.length &&
      a.zip(b).forall { case (a, b) => a.eqv(b) }
  }
  implicit def optionEq[A: Eq]: Eq[Option[A]] =
    (op1, op2) =>
      (op1, op2) match {
        case (None, None)             => true
        case (Some(val1), Some(val2)) => val1.eqv(val2)
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
