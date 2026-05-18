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
  ) @@ TestAspect.timeout(30.seconds)
