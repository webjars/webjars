package utils

import java.util.UUID

import akka.util.Timeout
import net.spy.memcached.transcoders.{SerializingTranscoder, Transcoder}
import play.api.test._

import scala.concurrent.Future
import scala.concurrent.duration._

class MemcacheSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val memcache = application.injector.instanceOf[Memcache]

  implicit val transcoderString = new SerializingTranscoder().asInstanceOf[Transcoder[String]]

  "set & get" should {
    "work" in {
      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo"))
      await(memcache.get[String](cacheKey)) must be equalTo "foo"
    }
  }

  "getWithMiss" should {
    "work on a miss" in {
      val cacheKey = UUID.randomUUID().toString
      await(memcache.getWithMiss(cacheKey)(Future.successful("foo"))) must be equalTo "foo"
    }
    "work on a hit" in {
      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo"))
      await(memcache.getWithMiss(cacheKey)(Future.successful("bar"))) must be equalTo "foo"
    }
  }

  "get miss" should {
    "work" in {
      val cacheKey = UUID.randomUUID().toString
      await(memcache.get[String](cacheKey)) must throwA[Memcache.Miss.type]
    }
  }

  // todo: cleanup

}
