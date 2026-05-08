package webjars.utils

import zio.*

trait Cache:
  def get[K](key: String, expiration: Duration)(onMiss: => ZIO[Scope, Throwable, K]): ZIO[Scope, Throwable, K]

case class CacheLive() extends Cache:

  private val cache = new java.util.concurrent.ConcurrentHashMap[String, (Any, Long, Long)]()

  private val DO_NOT_USE = "DO_NOT_USE"

  def get[K](key: String, expiration: Duration)(onMiss: => ZIO[Scope, Throwable, K]): ZIO[Scope, Throwable, K] =
    val now = java.lang.System.currentTimeMillis()
    val expirationMs = expiration.toMillis

    Option(cache.get(key)) match
      case Some((_, primaryExpiry, _)) if now < primaryExpiry =>
        Option(cache.get(key + ":value")) match
          case Some((value, _, _)) => ZIO.succeed(value.asInstanceOf[K])
          case None =>
            onMiss.tap(store(key, _, expirationMs))
      case _ =>
        onMiss.tap(store(key, _, expirationMs)).catchAll { e =>
          Option(cache.get(key + ":value")) match
            case Some((value, _, staleExpiry)) if now < staleExpiry =>
              ZIO.succeed(value.asInstanceOf[K])
            case _ => ZIO.fail(e)
        }

  private def store[K](key: String, value: K, expirationMs: Long): ZIO[Any, Nothing, Unit] =
    ZIO.succeed {
      val now = java.lang.System.currentTimeMillis()
      cache.put(key, (DO_NOT_USE, now + expirationMs, 0L))
      cache.put(key + ":value", (value, now + expirationMs, now + expirationMs * 2))
    }

object Cache:
  val live: ZLayer[Any, Nothing, Cache] = ZLayer.succeed(CacheLive())
