package utils

import javax.inject.Inject

import play.api.cache.CacheApi

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

class Cache @Inject() (cache: CacheApi) (implicit ec: ExecutionContext) {

  // todo: configurable duration that the stale cache is kept around for onMiss failures

  def get[K](key: String, expiration: FiniteDuration)(onMiss: => Future[K])(implicit classTag: ClassTag[K]): Future[K] = {
    val actualKey = key + ":value"

    cache.get(key).fold {
      // cache miss
      onMiss.map { value =>
        // store nothing in the cache with the actual expiration time
        cache.set(key, "DO NOT USE", expiration)
        // store the actual value in the cache with an expiration that is double the specified expiration
        cache.set(actualKey, value, expiration * 2)
        value
      } recoverWith {
        case e: Exception =>
          // onMiss failed so use the stale cache
          cache.get[K](actualKey).fold[Future[K]] {
            Future.failed(new Exception("The cache expired and could not be refreshed"))
          } (Future.successful)
      }
    } { _ =>
      // cache hit
      cache.get[K](actualKey).fold[Future[K]] {
        Future.failed(new Exception("Could not get the value from the cache"))
      } (Future.successful)
    }
  }

}
