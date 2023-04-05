package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformerUser: Transformer[RawUser, User] = new Transformer[RawUser, User] {

    override def toOption(a: RawUser): Option[User] = toEither(a) match {
      case Left(_)      => None
      case Right(value) => Some(value)
    }

    override def toEither(rawUser: RawUser): Either[Error, User] =
      for {
        idLong <- {
          rawUser.id.toLongOption match {
            case None      => Left(InvalidId)
            case Some(arg) => Right(arg)
          }
        }
        firstName <- rawUser.firstName match {
          case None        => Left(InvalidName)
          case Some(value) => Right(value)
        }
        secondName <- rawUser.secondName match {
          case None        => Left(InvalidName)
          case Some(value) => Right(value)
        }
      } yield User(
        idLong,
        UserName(firstName, secondName, rawUser.thirdName)
      )
  }
}

object TransformerSyntax {
  implicit class RawUserOps[A](a: A) {
    def transformToOption[B](implicit transformer: Transformer[A, B]): Option[B] = {
      transformer.toOption(a)
    }

    def transformToEither[B](implicit transformer: Transformer[A, B]): Either[Error, B] = {
      transformer.toEither(a)
    }
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
