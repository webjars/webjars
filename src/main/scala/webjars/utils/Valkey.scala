package webjars.utils

import zio.*
import zio.direct.*
import zio.redis.*
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.schema.Schema

import java.net.URI
import java.util.concurrent.TimeoutException

/** Production Redis layer. Reads `REDIS_URL` from the environment, builds
 *  an authenticated [[Redis]] client, and exposes it as a `ZLayer`. Provided
 *  once at the application edge ([[webjars.Main]]); services depend on
 *  `Redis` directly. Tests bypass this and provide their own Redis layer
 *  pointing at a testcontainer. */
object Valkey:

  object ProtobufCodecSupplier extends CodecSupplier:
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

  val codecSupplierLayer: ULayer[CodecSupplier] =
    ZLayer.succeed(ProtobufCodecSupplier)

  private val redisUri: ZIO[Any, Throwable, URI] =
    ZIO.systemWith: system =>
      system.env("REDIS_URL")
        .someOrFail(new RuntimeException("REDIS_URL env var not set"))
        .map(URI(_))

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
              authIfNeeded
                .repeat(Schedule.spaced(5.seconds))
                .tapErrorCause(c => ZIO.logErrorCause("Redis auth retry loop crashed", c))
                .forkDaemon
                .run
            case None =>
              redis.ping().timeoutFail(TimeoutException(timeoutMsg))(connectionCheckTimeout).run

          redis
      .orElse:
        ZLayer.succeed(env.get[Redis])

  val live: ZLayer[Any, Throwable, Redis] =
    (redisConfigLayer ++ codecSupplierLayer) >>> redisAuthLayer
