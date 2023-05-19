package service

import service.domain.GetTweetResponse.{Found, NotFound}
import service.domain.{GetTweetResponse, GetTweetsResponse}
import twitter.TwitterApi
import twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError}
import twitter.domain.{TweetId, TweetInfo, User}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

class TwitterServiceFuture(api: TwitterApi)(implicit ec: ExecutionContext) extends TwitterService[Future] {
  def tweet(user: User, text: String): Future[TweetId] = {
    val pr = Promise[TweetId]
    Future(api.tweet(user, text)(pr.complete))
    pr.future
  }

  def like(user: User, tweetId: TweetId): Future[Unit] = {
    val pr = Promise[Unit]
    Future(api.like(user, tweetId)(pr.complete))
    pr.future.recover({
      case LikeAlreadyExistError => ()
    })
  }

  def unlike(user: User, tweetId: TweetId): Future[Unit] = {
    val pr = Promise[Unit]
    Future(api.unlike(user, tweetId)(pr.complete))
    pr.future.recover({
      case LikeNotExistError => ()
    })
  }

  def getTweet(tweetId: TweetId): Future[GetTweetResponse] = {
    val pr = Promise[GetTweetResponse]
    Future(
      api.get(tweetId)(x =>
        pr.complete(x match {
          case Success(tweetInfo) => Success(Found(tweetInfo))
          case Failure(_)         => Success(NotFound(tweetId))
        })
      )
    )
    pr.future
  }

  def getTweets(ids: List[TweetId]): Future[GetTweetsResponse] = {
    Future
      .traverse(ids)(getTweet)
      .map(_.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((tweets, resp) => {
        resp match {
          case Found(v)    => GetTweetsResponse(tweets.notFound, tweets.found + v)
          case NotFound(v) => GetTweetsResponse(tweets.notFound + v, tweets.found)
        }
      }))
  }
}
