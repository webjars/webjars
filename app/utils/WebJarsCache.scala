package utils

import com.google.inject.ImplementedBy
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupArtifact
import models.WebJarVersion
import play.api.inject.ApplicationLifecycle
import zio.*
import zio.redis.*
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.schema.{Schema, derived}

import java.net.URI
import java.time.ZonedDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@ImplementedBy(classOf[ValkeyLive])
trait Valkey:
  val layer: ZLayer[Any, Nothing, Redis]

  object ProtobufCodecSupplier extends CodecSupplier:
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec

  val codecLayer: ZLayer[Any, Nothing, CodecSupplier] =
    ZLayer.succeed(ProtobufCodecSupplier)

@Singleton
class ValkeyLive @Inject() (lifecycle: ApplicationLifecycle) extends Valkey:
  private val redisUri: ZIO[Any, Throwable, URI] =
    ZIO.systemWith:
      system =>
        system.env("REDIS_URL")
          .someOrFail(new RuntimeException("REDIS_URL env var not set"))
          .map:
            redisUrl =>
              URI(redisUrl)

  private val redisConfigLayer: ZLayer[Any, Throwable, RedisConfig] =
    ZLayer.fromZIO:
      redisUri.map: uri =>
        RedisConfig(uri.getHost, uri.getPort, ssl = true, verifyCertificate = false)
      .orElseSucceed:
        RedisConfig("localhost", 6379)

  private val redisAuthLayer: ZLayer[CodecSupplier & RedisConfig, Throwable, Redis] =
    Redis.singleNode.flatMap: env =>
      ZLayer.fromZIO:
        for
          uri <- redisUri
          redis = env.get[Redis]
          password = uri.getUserInfo.drop(1) // REDIS_URL has an empty username

          authIfNeeded =
            redis.ping().catchAll:
              case e: RedisError if e.getMessage.contains("NOAUTH") =>
                ZIO.logInfo("Redis NOAUTH detected, authenticating...") *> redis.auth(password)
              case e =>
                ZIO.fail(e)

          _ <- redis.auth(password)

          _ <- authIfNeeded.repeat(Schedule.spaced(5.seconds)).forkDaemon
        yield
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

  lifecycle.addStopHook { () =>
    Future.successful {
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run(scopeCloseable.close(Exit.unit)).getOrThrow()
      }
    }
  }

  val layer: ZLayer[Any, Nothing, Redis] =
    ZLayer.succeedEnvironment(redisEnv)


object WebJarsCache:

  case class WebJarMeta(name: String, sourceUrl: String, versions: List[WebJarVersion], lastModified: ZonedDateTime) derives Schema

  def getArtifact(groupArtifact: GroupArtifact): ZIO[Redis, RedisError, Option[WebJarMeta]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      redis.hGet(groupArtifact.groupId, groupArtifact.artifactId).returning[WebJarMeta]

  def getArtifacts(groupId: MavenCentral.GroupId, limit: Option[Int] = None, query: Option[String] = None): ZIO[Redis, RedisError, Map[MavenCentral.ArtifactId, WebJarMeta]] =
    ZIO.serviceWithZIO[Redis]: redis =>
      val filtered = redis.hGetAll(groupId).returning[MavenCentral.ArtifactId, WebJarMeta].map: webJars =>
        query.fold(webJars): q =>
          webJars.filter: (artifactId, meta) =>
            artifactId.toString.toLowerCase.contains(q.toLowerCase) ||
              meta.name.toLowerCase.contains(q.toLowerCase)

      filtered.map: webJars =>
        limit.fold(webJars)(webJars.take)

      // hScan is broken in zio-redis: cursor isn't correct
      /*
      val found = ZStream.paginateZIO(0L): cursor =>
        redis.hScan(groupId, cursor).returning[MavenCentral.ArtifactId, WebJarMeta].debug.map:
          (nextCursor, webJarMetas) =>
            val next = if nextCursor == 0L then None else Some(nextCursor)
            val matchingMetas = query.fold(webJarMetas): q =>
              webJarMetas.filter:
                (artifactId, meta) =>
                  artifactId.toString.toLowerCase.contains(q.toLowerCase) ||
                    meta.name.toLowerCase.contains(q.toLowerCase)
            matchingMetas -> None

      found.runCollect.map:
        chunkChunks =>
          val chunks = chunkChunks.flatten
          limit.fold(chunks)(chunks.take).toMap
      */

  def setArtifactDetails(groupArtifact: GroupArtifact, webJarMeta: WebJarMeta): ZIO[Redis, Throwable, Unit] =
    ZIO.serviceWithZIO[Redis]: redis =>
      redis.hSet(groupArtifact.groupId, groupArtifact.artifactId -> webJarMeta).unit

  def updateVersion(groupArtifact: GroupArtifact, version: String, numFiles: Int): ZIO[Redis, Throwable, Unit] =
    for
      artifact <- getArtifact(groupArtifact).someOrFail(RuntimeException(s"Artifact $groupArtifact not found in cache"))
      updatedVersions = artifact.versions.map: v =>
        if v.number == version then
          v.copy(numFiles = Some(numFiles))
        else
          v
      updated = artifact.copy(versions = updatedVersions)
      _ <- setArtifactDetails(groupArtifact, updated)
    yield
      ()
