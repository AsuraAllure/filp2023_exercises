package service

import service.domain._
import twitter.domain._

trait TwitterService[F[_]] {
  def tweet(user: User, text: String): F[TweetId]

  def like(user: User, tweetId: TweetId): F[Unit]

  def unlike(user: User, tweetId: TweetId): F[Unit]

  def getTweet(tweetId: TweetId): F[GetTweetResponse]

  def getTweets(ids: List[TweetId]): F[GetTweetsResponse]
}
