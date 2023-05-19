package service

import cats.effect.IO
import cats.syntax.all._
import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain._
import twitter.domain.TwitterError._
import twitter.TwitterApi
import twitter.domain._

import scala.util.{Failure, Success, Try}

// Воспользуйтесь синтаксисом map, recover, traverse из cats.syntax.all_
class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = {
    createIO[TweetId, TweetId](api.tweet(user, text), PartialFunction.empty, identity)
  }

  def like(user: User, tweetId: TweetId): IO[Unit] = {
    createIO[Unit, Unit](api.like(user, tweetId), {
      case LikeAlreadyExistError => ()
    }, identity)
  }

  def unlike(user: User, tweetId: TweetId): IO[Unit] = {
    createIO[Unit, Unit](api.unlike(user, tweetId), {
      case LikeNotExistError => ()
    }, identity)
  }

  def getTweet(tweetId: TweetId): IO[GetTweetResponse] = {
    createIO[TweetInfo, GetTweetResponse](api.get(tweetId), PartialFunction.empty, {
      case Success(tweetInfo) => Success(Found(tweetInfo))
      case Failure(_)         => Success(NotFound(tweetId))
    })
  }

  def createIO[T1, T2](
                        f: (Try[T1] => Unit) => Unit,
                        recovered: PartialFunction[Throwable, T2],
                        converter: Try[T1] => Try[T2]
                      ): IO[T2] = { IO.async_[T2](clb => f(a => clb(converter(a).toEither))).recover(recovered) }

  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] = {
    ids
      .traverse(getTweet)
      .map(list =>
        list.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo])) { (tweets, resp) =>
          resp match {
            case Found(v)    => GetTweetsResponse(tweets.notFound, tweets.found + v)
            case NotFound(v) => GetTweetsResponse(tweets.notFound + v, tweets.found)
          }
        }
      )
  }
}
