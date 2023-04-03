package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def toOption(): Option[B] = this match {
      case Right(get) => Some(get)
      case _          => None
    }
    def map[C](f: B => C): Either[A, C] = this match {
      case Left(get)  => Left(get)
      case Right(get) => Right(f(get))
    }

    def flatMap[AA >: A, C](f: B => Either[AA, C]): Either[AA, C] = this match {
      case Left(get)  => Left(get)
      case Right(arg) => f(arg)
    }
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = (this, other) match {
      case (Right(get), _)       => Right(get)
      case (Left(_), Right(get)) => Right(get)
      case (Left(get), Left(_))  => Left(get)
    }

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = (this, other) match {
      case (Left(get), _)             => Left(get)
      case (_, Left(_2))              => Left(_2)
      case (Right(arg1), Right(arg2)) => Right(f(arg1, arg2))
    }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(value) => Right(value)
      case None        => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      list.foldLeft(Right(Nil): Either[E, List[B]])((holdList, newElement) =>
        (holdList, f(newElement)) match {
          case (Right(hList), Right(el)) => Right(hList.appended(el))
          case (Left(arg), _)            => Left(arg)
          case (_, Left(arg))            => Left(arg)
        }
      )
    }
    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(list)(identity)
    }
  }
}
