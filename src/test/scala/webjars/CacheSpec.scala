package webjars

import webjars.utils.CacheLive
import zio.*
import zio.test.*

import java.util.UUID

object CacheSpec extends ZIOSpecDefault:

  def spec = suite("Cache")(
    test("fetch a value when the cache is empty") {
      val cache = CacheLive()
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID().toString
      for
        result <- ZIO.scoped(cache.get[String](key, 1.second)(ZIO.succeed(value)))
      yield assertTrue(result == value)
    },
    test("not miss when the cache has a value") {
      val cache = CacheLive()
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      for
        _ <- ZIO.scoped(cache.get[UUID](key, 1.second)(ZIO.succeed(value)))
        result <- ZIO.scoped(cache.get[UUID](key, 1.second)(ZIO.fail(new Exception("SHOULD NOT REACH"))))
      yield assertTrue(result == value)
    },
    test("miss after expiration") {
      val cache = CacheLive()
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      for
        _ <- ZIO.scoped(cache.get[UUID](key, 1.second)(ZIO.succeed(UUID.randomUUID())))
        _ <- ZIO.sleep(2.seconds)
        result <- ZIO.scoped(cache.get[UUID](key, 1.second)(ZIO.succeed(value)))
      yield assertTrue(result == value)
    } @@ TestAspect.withLiveClock,
    test("does not cache failures — next call retries") {
      val cache = CacheLive()
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      for
        attempts <- Ref.make(0)
        // First lookup fails.
        first    <- ZIO.scoped(cache.get[UUID](key, 1.hour) {
                      attempts.update(_ + 1) *> ZIO.fail(new Exception("transient"))
                    }).either
        // Second lookup with a fresh `onMiss` should re-run rather than
        // returning the cached failure, even though the TTL is 1h.
        second   <- ZIO.scoped(cache.get[UUID](key, 1.hour) {
                      attempts.update(_ + 1) *> ZIO.succeed(value)
                    })
        finalAttempts <- attempts.get
      yield assertTrue(
        first.isLeft,
        second == value,
        finalAttempts == 2,
      )
    },
    test("still caches successes after a failure-then-success sequence") {
      val cache = CacheLive()
      val key = UUID.randomUUID().toString
      val value = UUID.randomUUID()
      for
        _      <- ZIO.scoped(cache.get[UUID](key, 1.hour)(ZIO.fail(new Exception("transient")))).either
        _      <- ZIO.scoped(cache.get[UUID](key, 1.hour)(ZIO.succeed(value)))
        // Third call must hit the cached success (onMiss must not run).
        result <- ZIO.scoped(cache.get[UUID](key, 1.hour)(ZIO.fail(new Exception("SHOULD NOT REACH"))))
      yield assertTrue(result == value)
    },
  ) @@ TestAspect.timeout(30.seconds)
