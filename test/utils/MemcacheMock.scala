package utils

import net.spy.memcached.transcoders.Transcoder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

class MemcacheMock extends Memcache {

  val cache = collection.mutable.Map.empty[String, Any]

  override def get[A](cacheKey: String)(implicit transcoder: Transcoder[A]): Future[A] = {
    cache.get(cacheKey).fold(Future.failed[A](Memcache.Miss)) { any =>
      Future.fromTry {
        Try {
          any.asInstanceOf[A]
        }
      }
    }
  }

  override def getWithMiss[A](cacheKey: String, expiration: Memcache.Expiration)(miss: => Future[A])(implicit transcoder: Transcoder[A]): Future[A] = {
    get(cacheKey).recoverWith {
      case Memcache.Miss =>
        for {
          value <- miss
          _ <- set(cacheKey, value)
        } yield value
    }
  }

  override def set[A](cacheKey: String, value: A, expiration: Memcache.Expiration)(implicit transcoder: Transcoder[A]): Future[Unit] = {
    cache.update(cacheKey, value)
    Future.unit
  }
}
