package utils

import akka.util.Timeout
import net.spy.memcached.transcoders.{SerializingTranscoder, Transcoder}
import play.api.test._

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration._

class MemcacheSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  implicit val transcoderString = new SerializingTranscoder().asInstanceOf[Transcoder[String]]

  "set & get" should {
    "work" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]

      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo"))
      await(memcache.get[String](cacheKey)) must be equalTo "foo"
    }
    "work with expiration less than a second" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]
      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo", Memcache.Expiration.In(50.millis))) must throwAn[Exception]
    }
    "work with expiration more than a second" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]
      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo", Memcache.Expiration.In(2.seconds)))
      await(memcache.get[String](cacheKey)) must be equalTo "foo"
      Thread.sleep(2500)
      await(memcache.get[String](cacheKey)) must throwA[Memcache.Miss.type]
    }
  }

  "getWithMiss" should {
    "work on a miss" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]
      val cacheKey = UUID.randomUUID().toString
      await(memcache.getWithMiss(cacheKey)(Future.successful("foo"))) must be equalTo "foo"
    }
    "work on a hit" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]
      val cacheKey = UUID.randomUUID().toString
      await(memcache.set(cacheKey, "foo"))
      await(memcache.getWithMiss(cacheKey)(Future.successful("bar"))) must be equalTo "foo"
    }
  }

  "get miss" should {
    "work" in new WithApplication() {
      val memcache = app.injector.instanceOf[Memcache]
      val cacheKey = UUID.randomUUID().toString
      await(memcache.get[String](cacheKey)) must throwA[Memcache.Miss.type]
    }
  }

  // todo: cleanup

}