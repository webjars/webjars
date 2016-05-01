package utils

import java.util.UUID
import java.util.concurrent.TimeUnit

import play.api.test._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class CacheSpec extends PlaySpecification {

  val app = FakeApplication()
  val cache = Cache(ExecutionContext.global, app)

  "cache.get" should {
    "fetch a value when the cache is empty" in {
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      val futureValue = cache.get[String](key, 1.second) {
        Future.successful(value)
      }

      await(futureValue) mustEqual value
    }
    "not miss when the cache has a value" in {
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      val iterator = Iterator(value, "SHOULD NOT REACH")
      def onMiss() = Future.successful(iterator.next())
      val futureSecondGet = cache.get[String](key, 1.second)(onMiss()).flatMap { firstGet =>
        cache.get[String](key, 1.second)(onMiss())
      }

      await(futureSecondGet) mustEqual value
    }
    "miss after expiration" in {
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      val iterator = Iterator("first value", value)
      def onMiss() = Future.successful(iterator.next())
      val futureSecondGet = cache.get[String](key, 1.second)(onMiss()).flatMap { firstGet =>
        Thread.sleep(2000)
        cache.get[String](key, 1.second)(onMiss())
      }

      await(futureSecondGet) mustEqual value
    }
    "stick with the original cache value if there is a failure on expiration renewal" in {
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      val iterator = Iterator(Future.successful(value), Future.failed(new Exception("can not get a new value")))
      def onMiss() = iterator.next()
      val futureSecondGet = cache.get[String](key, 1.second)(onMiss()).flatMap { firstGet =>
        Thread.sleep(1500)
        cache.get[String](key, 1.second)(onMiss())
      }

      await(futureSecondGet) mustEqual value
    }
  }

}