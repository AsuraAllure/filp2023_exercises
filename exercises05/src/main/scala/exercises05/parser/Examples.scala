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
  def parsePassport(rawPassport: Option[String]): Option[Passport] = rawPassport match {
    case None => None
    case Some(str) =>
      val passportData = str.split(" ")
      if (passportData.length != 2)
        None
      else
        (passportData(0).toIntOption, passportData(1).toIntOption) match {
          case (Some(v1), Some(v2)) =>
            if (passportData(0).length == 4 && passportData(1).length == 6) Some(Passport(v1, v2))
            else None
          case _ => None
        }
  }
  def parseName(rawFirstName: Option[String], rawSecondName: Option[String]): Option[(String, String)] =
    (rawFirstName, rawSecondName) match {
      case (Some(n1), Some(n2)) => Some((n1, n2))
      case _                    => None
    }

  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      userName <- parseName(rawUser.firstName, rawUser.secondName)
      idLong   <- rawUser.id.toLongOption
      parsedPassport = parsePassport(rawUser.passport)
      if !(rawUser.passport.nonEmpty && parsedPassport.isEmpty)
      if !rawUser.banned
    } yield User(
      idLong,
      UserName(userName._1, userName._2, rawUser.thirdName),
      parsedPassport
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
    for {
      banned   <- { if (rawUser.banned) Left(Banned) else Right(rawUser.banned) }
      idLong   <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      userName <- Either.fromOption(parseName(rawUser.firstName, rawUser.secondName))(InvalidName)
      parsPas = parsePassport(rawUser.passport)
      parsedPassport <- {
        if (parsPas.isEmpty && rawUser.passport.nonEmpty)
          Left(InvalidPassport)
        else
          Right(parsPas)
      }
    } yield User(
      idLong,
      UserName(userName._1, userName._2, rawUser.thirdName),
      parsedPassport
    )
}
