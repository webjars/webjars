package utils

import play.api.Application
import play.api.cache.{Cache => PlayCache}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

class Cache(implicit ec: ExecutionContext, app: Application) {

  // todo: configurable duration that the stale cache is kept around for onMiss failures

  def get[K](key: String, expiration: FiniteDuration)(onMiss: => Future[K])(implicit classTag: ClassTag[K]): Future[K] = {
    val actualKey = key + ":value"

    PlayCache.get(key).fold {
      // cache miss
      onMiss.map { value =>
        // store nothing in the cache with the actual expiration time
        PlayCache.set(key, "DO NOT USE", expiration)
        // store the actual value in the cache with an expiration that is double the specified expiration
        PlayCache.set(actualKey, value, expiration * 2)
        value
      } recoverWith {
        case e: Exception =>
          // onMiss failed so use the stale cache
          PlayCache.getAs[K](actualKey).fold[Future[K]] {
            Future.failed(new Exception("The cache expired and could not be refreshed"))
          } (Future.successful)
      }
    } { _ =>
      // cache hit
      PlayCache.getAs[K](actualKey).fold[Future[K]] {
        Future.failed(new Exception("Could not get the value from the cache"))
      } (Future.successful)
    }
  }

}


object Cache {
  def apply(implicit ec: ExecutionContext, app: Application) = new Cache()
}

