package exercises05.parser

import exercises05.either.EitherCombinators._
import exercises05.parser.Error.{Banned, InvalidId, InvalidName, InvalidPassport}

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      firstName  <- rawUser.firstName
      secondName <- rawUser.secondName
      idLong     <- rawUser.id.toLongOption
      if !(firstName.isEmpty || secondName.isEmpty || rawUser.banned)
      if rawUser.passport.isEmpty || rawUser.passport.contains("1234 567890")
    } yield User(
      idLong,
      UserName(firstName, secondName, rawUser.thirdName),
      if (rawUser.passport.isEmpty) None else Some(Passport(1234, 567890))
    )

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    transformToOption(rawUser) match {
      case Some(user) => Right(user)
      case None =>
        if (rawUser.banned)
          Left(Banned)
        else if (rawUser.id.toLongOption.isEmpty)
          Left(InvalidId)
        else if (rawUser.firstName.isEmpty || rawUser.secondName.isEmpty)
          Left(InvalidName)
        else
          Left(InvalidPassport)
    }

}
