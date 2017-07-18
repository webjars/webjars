package utils

import javax.inject.Inject

import play.api.cache.SyncCacheApi

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

class Cache @Inject() (cache: SyncCacheApi) (implicit ec: ExecutionContext) {

  // todo: configurable duration that the stale cache is kept around for onMiss failures

  private val DO_NOT_USE = "DO NOT USE"


  def get[K](key: String, expiration: FiniteDuration)(onMiss: => Future[K])(implicit classTag: ClassTag[K]): Future[K] = {
    val actualKey = key + ":value"

    def store(value: K): K = {
      cache.set(key, DO_NOT_USE, expiration)
      // store the actual value in the cache with an expiration that is double the specified expiration
      cache.set(actualKey, value, expiration * 2)
      value
    }

    cache.get[String](key).fold {
      // cache miss
      onMiss.map(store).recoverWith {
        case e: Exception =>
          // onMiss failed so use the stale cache
          cache.get[K](actualKey).fold[Future[K]] {
            Future.failed(e)
          } (Future.successful)
      }
    } { s: String =>
      if (s == DO_NOT_USE) {
        // cache hit
        cache.get[K](actualKey).fold[Future[K]](onMiss.map(store))(Future.successful)
      }
      else {
        // cache miss
        onMiss.map(store)
      }
    }
  }

}
