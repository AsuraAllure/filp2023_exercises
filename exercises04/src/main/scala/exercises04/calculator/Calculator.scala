package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def evalution(r1: Result[T], r2: Result[T], f: (T, T) => Result[T]): Result[T] = (r1, r2) match {
    case (Success(v1), Success(v2)) => f(v1, v2)
    case _                          => DivisionByZero
  }
  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Val(v1) => Success(v1)

    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(value) =>
          if (iff(value))
            calculate(left)
          else
            calculate(right)
        case err => err
      }

    case Div(left, right) =>
      evalution(calculate(left), calculate(right), { (arg1, arg2) =>
        if (!isZero(arg2))
          Success(arg1 + arg2)
        else
          DivisionByZero
      })

    case Mul(left, right) =>
      evalution(calculate(left), calculate(right), { (arg1, arg2) =>
        Success(arg1 * arg2)
      })

    case Plus(left, right) =>
      evalution(calculate(left), calculate(right), { (arg1, arg2) =>
        Success(arg1 + arg2)
      })

    case Minus(left, right) =>
      evalution(calculate(left), calculate(right), { (arg1, arg2) =>
        Success(arg1 - arg2)
      })

  }
}
