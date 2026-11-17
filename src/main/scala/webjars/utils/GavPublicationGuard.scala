package webjars.utils

import com.augustnagro.magnum.*
import com.augustnagro.magnum.ziomagnum.*
import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.Db.given
import zio.*

import java.util.UUID
import javax.sql.DataSource

/** Keeps two deploys (on any instances) from publishing the same GAV to
 *  Maven Central. One row in `gav_publication` per GAV is a lease: the owner
 *  claims it, heartbeats it while publishing, then marks it complete (later
 *  deploys join) or failed (later deploys back off until it expires). An
 *  expired row counts as absent. Expiry uses the database clock (`now()`),
 *  so instances with skewed clocks agree on it. */
object GavPublicationGuard:

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

  /** The lease's phase, stored in `gav_publication.state`. */
  enum Phase(val sqlName: String) derives CanEqual:
    case Publishing extends Phase("publishing")
    case Complete   extends Phase("complete")
    case Failed     extends Phase("failed")

  object Phase:
    given DbCodec[Phase] = DbCodec[String].biMap(
      name => Phase.values.find(_.sqlName == name).getOrElse(throw IllegalStateException(s"unknown gav_publication.state: $name")),
      _.sqlName,
    )

  final case class State(phase: Phase, token: UUID) derives DbCodec, CanEqual

  private enum Claim:
    case Owner(token: UUID)
    case Joined

  private def key(gav: MavenCentral.GroupArtifactVersion): Frag =
    sql"group_id = ${gav.groupId.toString} and artifact_id = ${gav.artifactId.toString} and version = ${gav.version.toString}"

  private[webjars] def state(gav: MavenCentral.GroupArtifactVersion): RIO[DataSource, Option[State]] =
    Db.withDataSource:
      sql"select state, token from gav_publication where ${key(gav)} and expires_at > now()".zQuery[State].map(_.headOption)

  /** Takes the lease unless an unexpired one exists (`SET NX PX` in the Redis version). */
  private[webjars] def claim(gav: MavenCentral.GroupArtifactVersion, token: UUID, ttl: Duration): RIO[DataSource, Boolean] =
    Db.withDataSource:
      sql"""insert into gav_publication (group_id, artifact_id, version, state, token, expires_at)
            values (${gav.groupId.toString}, ${gav.artifactId.toString}, ${gav.version.toString}, ${Phase.Publishing}, $token, now() + ${Db.interval(ttl)})
            on conflict (group_id, artifact_id, version) do update
              set state = excluded.state, token = excluded.token, expires_at = excluded.expires_at
              where gav_publication.expires_at <= now()""".zUpdate.map(_ == 1)

  /** Moves the owner's unexpired `publishing` lease to `phase`, expiring after `ttl`. False when `token` no longer owns it. */
  private[webjars] def transition(gav: MavenCentral.GroupArtifactVersion, token: UUID, phase: Phase, ttl: Duration): RIO[DataSource, Boolean] =
    Db.withDataSource:
      sql"""update gav_publication set state = $phase, expires_at = now() + ${Db.interval(ttl)}
            where ${key(gav)} and token = $token and state = ${Phase.Publishing} and expires_at > now()""".zUpdate.map(_ == 1)

  private[webjars] def release(gav: MavenCentral.GroupArtifactVersion, token: UUID): RIO[DataSource, Boolean] =
    Db.withDataSource:
      sql"delete from gav_publication where ${key(gav)} and token = $token and state = ${Phase.Publishing}".zUpdate.map(_ == 1)

  private def renew(gav: MavenCentral.GroupArtifactVersion, token: UUID, ttl: Duration): RIO[DataSource, Unit] =
    transition(gav, token, Phase.Publishing, ttl).flatMap: renewed =>
      if renewed then ZIO.unit
      else ZIO.fail(LeaseLost(gav))

  def publish[R](
    gav: MavenCentral.GroupArtifactVersion,
    alreadyPublished: ZIO[R, Throwable, Boolean],
    config: Config = Config.default,
  )(
    publishEffect: ZIO[R, Throwable, Unit]
  ): ZIO[R & DataSource, Throwable, Outcome] =
    Random.nextUUID.flatMap { token =>

      def acquire: RIO[DataSource, Claim] =
        state(gav).flatMap {
          case Some(State(Phase.Complete, _)) =>
            ZIO.succeed(Claim.Joined)
          case Some(State(Phase.Failed, _)) =>
            ZIO.fail(PreviousAttemptFailed(gav))
          case Some(State(Phase.Publishing, _)) =>
            acquire.delay(config.pollEvery)
          case None =>
            claim(gav, token, config.uncertaintyTtl).flatMap {
              case true => ZIO.succeed(Claim.Owner(token))
              case false => acquire.delay(config.pollEvery)
            }
        }

      def moveTo(phase: Phase, ttl: Duration): RIO[DataSource, Unit] =
        transition(gav, token, phase, ttl).flatMap { changed =>
          if changed then ZIO.unit
          else ZIO.fail(LeaseLost(gav))
        }

      def runAsOwner: ZIO[R & DataSource, Throwable, Outcome] =
        ZIO.scoped {
          for
            ownershipLost <- Promise.make[Throwable, Nothing]
            heartbeat = (ZIO.sleep(config.heartbeatEvery) *>
              renew(gav, token, config.uncertaintyTtl).foldZIO(
                error => ownershipLost.fail(error).unit,
                _ => ZIO.unit,
              )).forever
            heartbeatFiber <- heartbeat.forkScoped
            stopHeartbeat = heartbeatFiber.interrupt.unit
            outcome <- alreadyPublished.foldCauseZIO(
              cause => stopHeartbeat *> release(gav, token).ignore *> ZIO.refailCause(cause),
              {
                case true =>
                  stopHeartbeat *>
                    moveTo(Phase.Complete, config.completionTtl).as(Outcome.AlreadyPublished)
                case false =>
                  publishEffect
                    .onExit {
                      case Exit.Failure(_) =>
                        stopHeartbeat *>
                          transition(gav, token, Phase.Failed, config.uncertaintyTtl).ignore
                      case Exit.Success(_) =>
                        ZIO.unit
                    } *>
                    stopHeartbeat *>
                    moveTo(Phase.Complete, config.completionTtl).as(Outcome.Published)
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
