package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import zio.*
import zio.redis.{Input, Output, Redis, Update}

object GavPublicationGuard:

  private given Input[String] = Input.StringInput
  private given Output[Long] = Output.LongOutput

  enum Outcome:
    case Published
    case AlreadyPublished
    case Joined

  final case class Config private (
    uncertaintyTtl: Duration,
    heartbeatEvery: Duration,
    completionTtl: Duration,
    pollEvery: Duration,
    contentionTimeout: Duration,
  )

  object Config:
    def make(
      uncertaintyTtl: Duration,
      heartbeatEvery: Duration,
      completionTtl: Duration,
      pollEvery: Duration,
      contentionTimeout: Duration,
    ): Either[IllegalArgumentException, Config] =
      if uncertaintyTtl.toMillis <= 0 then Left(IllegalArgumentException("uncertaintyTtl must be positive"))
      else if heartbeatEvery.toMillis <= 0 || heartbeatEvery >= uncertaintyTtl then
        Left(IllegalArgumentException("heartbeatEvery must be positive and shorter than uncertaintyTtl"))
      else if completionTtl.toMillis <= 0 then Left(IllegalArgumentException("completionTtl must be positive"))
      else if pollEvery.toMillis <= 0 then Left(IllegalArgumentException("pollEvery must be positive"))
      else if contentionTimeout.toMillis <= 0 then Left(IllegalArgumentException("contentionTimeout must be positive"))
      else Right(Config(uncertaintyTtl, heartbeatEvery, completionTtl, pollEvery, contentionTimeout))

    val default: Config = make(
      uncertaintyTtl = 2.hours,
      heartbeatEvery = 1.minute,
      completionTtl = 6.hours,
      pollEvery = 1.second,
      contentionTimeout = 15.minutes,
    ).toOption.get

  final case class LeaseLost(gav: MavenCentral.GroupArtifactVersion)
    extends Exception(s"Lost distributed Maven publication ownership for $gav")

  final case class PublicationInProgress(gav: MavenCentral.GroupArtifactVersion)
    extends Exception(s"Another deployment is still publishing $gav")

  final case class PreviousAttemptFailed(gav: MavenCentral.GroupArtifactVersion)
    extends Exception(s"A recent Maven publication attempt for $gav failed or had an uncertain outcome; retry after the safety window")

  private enum Claim:
    case Owner(token: String)
    case Joined

  private val transitionScript =
    """if redis.call('get', KEYS[1]) == ARGV[1] then
      |  redis.call('set', KEYS[1], ARGV[2], 'PX', ARGV[3])
      |  return 1
      |else
      |  return 0
      |end""".stripMargin

  private val renewScript =
    """if redis.call('get', KEYS[1]) == ARGV[1] then
      |  return redis.call('pexpire', KEYS[1], ARGV[2])
      |else
      |  return 0
      |end""".stripMargin

  private val releaseScript =
    """if redis.call('get', KEYS[1]) == ARGV[1] then
      |  return redis.call('del', KEYS[1])
      |else
      |  return 0
      |end""".stripMargin

  private[webjars] def stateKey(gav: MavenCentral.GroupArtifactVersion): String =
    s"webjars:maven-publish:state:${gav.groupId}:${gav.artifactId}:${gav.version}"

  private def publishingValue(token: String): String = s"publishing:$token"
  private def completeValue(token: String): String = s"complete:$token"
  private def failedValue(token: String): String = s"failed:$token"

  private[webjars] def release(redis: Redis, key: String, expectedValue: String): IO[Throwable, Boolean] =
    redis.eval(releaseScript, Chunk(key), Chunk(expectedValue)).returning[Long].map(_ == 1L)

  private[webjars] def transition(
    redis: Redis,
    key: String,
    expectedValue: String,
    newValue: String,
    ttl: Duration,
  ): IO[Throwable, Boolean] =
    redis.eval(
      transitionScript,
      Chunk(key),
      Chunk(expectedValue, newValue, ttl.toMillis.toString),
    ).returning[Long].map(_ == 1L)

  private def renew(
    redis: Redis,
    gav: MavenCentral.GroupArtifactVersion,
    key: String,
    expectedValue: String,
    ttl: Duration,
  ): IO[Throwable, Unit] =
    redis.eval(renewScript, Chunk(key), Chunk(expectedValue, ttl.toMillis.toString)).returning[Long].flatMap { renewed =>
      if renewed == 1L then ZIO.unit
      else ZIO.fail(LeaseLost(gav))
    }

  private def state(redis: Redis, key: String): IO[Throwable, Option[String]] =
    redis.get(key).returning[String]

  def publish[R](
    gav: MavenCentral.GroupArtifactVersion,
    alreadyPublished: ZIO[R, Throwable, Boolean],
    config: Config = Config.default,
  )(
    publishEffect: ZIO[R, Throwable, Unit]
  ): ZIO[R & Redis, Throwable, Outcome] =
    ZIO.serviceWithZIO[Redis] { redis =>
      Random.nextUUID.map(_.toString).flatMap { token =>
        val key = stateKey(gav)
        val publishing = publishingValue(token)

        def acquire: IO[Throwable, Claim] =
          state(redis, key).flatMap {
            case Some(value) if value.startsWith("complete:") =>
              ZIO.succeed(Claim.Joined)
            case Some(value) if value.startsWith("failed:") =>
              ZIO.fail(PreviousAttemptFailed(gav))
            case Some(_) =>
              acquire.delay(config.pollEvery)
            case None =>
              redis.set(
                key,
                publishing,
                expireTime = Some(config.uncertaintyTtl),
                update = Some(Update.SetNew),
              ).flatMap {
                case true => ZIO.succeed(Claim.Owner(token))
                case false => acquire.delay(config.pollEvery)
              }
          }

        def moveTo(value: String, ttl: Duration): IO[Throwable, Unit] =
          transition(redis, key, publishing, value, ttl).flatMap { changed =>
            if changed then ZIO.unit
            else ZIO.fail(LeaseLost(gav))
          }

        def runAsOwner: ZIO[R, Throwable, Outcome] =
          ZIO.scoped {
            for
              ownershipLost <- Promise.make[Throwable, Nothing]
              heartbeat = (ZIO.sleep(config.heartbeatEvery) *>
                renew(redis, gav, key, publishing, config.uncertaintyTtl).foldZIO(
                  error => ownershipLost.fail(error).unit,
                  _ => ZIO.unit,
                )).forever
              heartbeatFiber <- heartbeat.forkScoped
              stopHeartbeat = heartbeatFiber.interrupt.unit
              outcome <- alreadyPublished.foldCauseZIO(
                cause => stopHeartbeat *> release(redis, key, publishing).ignore *> ZIO.refailCause(cause),
                {
                  case true =>
                    stopHeartbeat *>
                      moveTo(completeValue(token), config.completionTtl).as(Outcome.AlreadyPublished)
                  case false =>
                    publishEffect
                      .onExit {
                        case Exit.Failure(_) =>
                          stopHeartbeat *>
                            transition(redis, key, publishing, failedValue(token), config.uncertaintyTtl).ignore
                        case Exit.Success(_) =>
                          ZIO.unit
                      } *>
                      stopHeartbeat *>
                      moveTo(completeValue(token), config.completionTtl).as(Outcome.Published)
                },
              ).raceFirst(ownershipLost.await)
            yield outcome
          }

        acquire
          .timeoutFail(PublicationInProgress(gav))(config.contentionTimeout)
          .flatMap {
            case Claim.Owner(_) => runAsOwner
            case Claim.Joined => ZIO.succeed(Outcome.Joined)
          }
      }
    }
