package utils

import java.util.UUID

import play.api.test._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class CacheSpec extends PlaySpecification {

  "cache.get" should {
    "fetch a value when the cache is empty" in new WithApplication() {
      val cache = app.injector.instanceOf[Cache]

      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      val futureValue = cache.get[String](key, 1.second) {
        Future.successful(value)
      }

      await(futureValue) mustEqual value
    }
    "not miss when the cache has a value" in new WithApplication() {
      val cache = app.injector.instanceOf[Cache]

      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      val futureSecondGet = cache.get[UUID](key, 1.second)(Future.successful(value)).flatMap { firstGet =>
        cache.get[UUID](key, 1.second)(Future.failed(new Exception("SHOULD NOT REACH")))
      }

      await(futureSecondGet) mustEqual value
    }
    "miss after expiration" in new WithApplication() {
      val cache = app.injector.instanceOf[Cache]

      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      val futureSecondGet = cache.get[UUID](key, 1.second)(Future.successful(UUID.randomUUID())).flatMap { firstGet =>
        Thread.sleep(2000)
        cache.get[UUID](key, 1.second)(Future.successful(value))
      }

      await(futureSecondGet) mustEqual value
    }
    "stick with the original cache value if there is a failure on expiration renewal" in new WithApplication() {
      val cache = app.injector.instanceOf[Cache]

      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      val futureSecondGet = cache.get[UUID](key, 1.second)(Future.successful(value)).flatMap { firstGet =>
        Thread.sleep(1500)
        cache.get[UUID](key, 1.second)(Future.failed(new Exception("can not get a new value")))
      }

      await(futureSecondGet) mustEqual value
    }
  }

}
