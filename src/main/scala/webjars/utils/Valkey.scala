package webjars.utils

import zio.*
import zio.direct.*
import zio.redis.*
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.schema.Schema

import java.net.URI

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
        RedisConfig(uri.getHost, uri.getPort, ssl = true, verifyCertificate = false)
      .orElseSucceed:
        RedisConfig("localhost", 6379)

  private val redisAuthLayer: ZLayer[CodecSupplier & RedisConfig, Throwable, Redis] =
    Redis.singleNode.flatMap: env =>
      ZLayer.fromZIO:
        defer:
          val uri = redisUri.run
          val redis = env.get[Redis]
          val password = uri.getUserInfo.drop(1) // REDIS_URL has an empty username

          val authIfNeeded =
            redis.ping().catchAll:
              case e: RedisError if e.getMessage.contains("NOAUTH") =>
                ZIO.logInfo("Redis NOAUTH detected, authenticating...") *> redis.auth(password)
              case e =>
                ZIO.fail(e)

          redis.auth(password).run
          authIfNeeded.repeat(Schedule.spaced(5.seconds)).forkDaemon.run
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
