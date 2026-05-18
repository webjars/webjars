package webjars.utils

import zio.*
import zio.direct.*
import zio.redis.*
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.schema.Schema

import java.net.URI
import java.util.concurrent.TimeoutException

trait Valkey:
  def layer: ZLayer[Any, Nothing, Redis]
  def codecLayer: ZLayer[Any, Nothing, CodecSupplier]
  def close(): Unit

case class ValkeyLive() extends Valkey:

  object ProtobufCodecSupplier extends CodecSupplier:
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

  val codecLayer: ZLayer[Any, Nothing, CodecSupplier] =
    ZLayer.succeed(ProtobufCodecSupplier)

  private val redisUri: ZIO[Any, Throwable, URI] =
    ZIO.systemWith: system =>
      system.env("REDIS_URL")
        .someOrFail(new RuntimeException("REDIS_URL env var not set"))
        .map(redisUrl => URI(redisUrl))

  private val redisConfigLayer: ZLayer[Any, Throwable, RedisConfig] =
    ZLayer.fromZIO:
      redisUri.map: uri =>
        // `rediss://...` → TLS (Heroku, managed Redis). `redis://...` → plaintext (local dev).
        val ssl = uri.getScheme == "rediss"
        RedisConfig(uri.getHost, uri.getPort, ssl = ssl, verifyCertificate = false)

  private val redisAuthLayer: ZLayer[CodecSupplier & RedisConfig, Throwable, Redis] =
    Redis.singleNode.flatMap: env =>
      ZLayer.fromZIO:
        defer:
          val uri = redisUri.run
          val redis = env.get[Redis]
          // REDIS_URL on Heroku is `rediss://:password@host:port` (empty
          // username + ':' separator + password), so we drop the leading ':'.
          // Local redis URLs without auth produce null here — treat that as "no password".
          val maybePassword = Option(uri.getUserInfo).map(_.drop(1)).filter(_.nonEmpty)

          // zio-redis silently hangs forever if the TLS handshake never
          // completes (e.g. ssl=true vs plain redis://). A short timeout on
          // the first command turns that into a fast-fail at boot.
          val connectionCheckTimeout = 5.seconds
          val timeoutMsg = s"Redis connection check timed out after $connectionCheckTimeout — check that REDIS_URL scheme (redis:// vs rediss://) matches the server."

          maybePassword match
            case Some(password) =>
              val authIfNeeded =
                redis.ping().catchAll:
                  case e: RedisError if e.getMessage.contains("NOAUTH") =>
                    ZIO.logInfo("Redis NOAUTH detected, authenticating...") *> redis.auth(password)
                  case e =>
                    ZIO.fail(e)
              redis.auth(password).timeoutFail(TimeoutException(timeoutMsg))(connectionCheckTimeout).run
              authIfNeeded.repeat(Schedule.spaced(5.seconds)).forkDaemon.run
            case None =>
              redis.ping().timeoutFail(TimeoutException(timeoutMsg))(connectionCheckTimeout).run

          redis
      .orElse:
        ZLayer.succeed(env.get[Redis])

  private val runtime = Runtime.default

  private val scopeCloseable: Scope.Closeable =
    Unsafe.unsafe { implicit u =>
      runtime.unsafe.run(Scope.make).getOrThrow()
    }

  private lazy val redisEnv: ZEnvironment[Redis] =
    Unsafe.unsafe { implicit u =>
      runtime.unsafe
        .run(
          ((redisConfigLayer ++ codecLayer) >>> redisAuthLayer)
            .build
            .provideEnvironment(ZEnvironment(scopeCloseable))
        )
        .getOrThrow()
    }

  def close(): Unit =
    Unsafe.unsafe { implicit u =>
      runtime.unsafe.run(scopeCloseable.close(Exit.unit)).getOrThrow()
    }

  val layer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeedEnvironment(redisEnv)

object Valkey:
  val live: ZLayer[Any, Nothing, Valkey] = ZLayer.succeed(ValkeyLive())
