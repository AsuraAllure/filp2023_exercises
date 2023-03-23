package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Val(v1) => Success(v1)

    case Mul(left, right) =>
      (left, right) match {
        case (Val(v1), Val(v2)) => Success(v1 * v2)
        case _ =>
          (calculate(left), calculate(right)) match {
            case (Success(v1), Success(v2)) => Success(v1 * v2)
            case _ => DivisionByZero
          }

      }
    case Div(left, right) =>
      (left, right) match {
        case (Val(v1), Val(v2)) =>
          if (isZero(v2)) DivisionByZero else Success(v1 / v2)
        case _ =>
          (calculate(left), calculate(right)) match {
            case (Success(v1), Success(v2)) => Success(v1 / v2)
            case _ => DivisionByZero
          }
      }
    case Plus(left, right) =>
      (left, right) match {
        case (Val(v1), Val(v2)) => Success(v1 + v2)
        case _ =>
          (calculate(left), calculate(right)) match {
            case (Success(v1), Success(v2)) => Success(v1 + v2)
            case _ => DivisionByZero
          }

      }
    case Minus(left, right) =>
      (left, right) match {
        case (Val(v1), Val(v2)) => Success(v1 - v2)
        case _ =>
          (calculate(left), calculate(right)) match {
            case (Success(v1), Success(v2)) => Success(v1 - v2)
            case _ => DivisionByZero
          }
      }
    case If(iff, cond, left, right) =>
      if (cond match {
        case Val(v1) => iff(v1)
        case expres =>
          calculate(expres) match {
            case Success(v) => iff(v)
          }
      })
        calculate(left)
      else
        calculate(right)

  }
}
