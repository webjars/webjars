package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.GavPublicationGuard
import zio.*
import zio.test.*

import java.util.UUID
import javax.sql.DataSource

object GavPublicationGuardSpec extends ZIOSpecDefault:

  private val config = GavPublicationGuard.Config.make(
    uncertaintyTtl = 500.millis,
    heartbeatEvery = 50.millis,
    completionTtl = 10.seconds,
    pollEvery = 20.millis,
    contentionTimeout = 5.seconds,
  ).toOption.get

  private def withGav[A](test: MavenCentral.GroupArtifactVersion => ZIO[DataSource, Throwable, A]): ZIO[DataSource, Throwable, A] =
    Random.nextUUID.flatMap { id =>
      test(MavenCentral.gav("org.webjars.test", s"guard-$id", "1.0.0"))
    }

  def spec = suite("GavPublicationGuard")(
    test("concurrent instances publish once while the lease heartbeat keeps ownership") {
      withGav { gav =>
        for
          ownerStarted <- Promise.make[Nothing, Unit]
          finishOwner <- Promise.make[Nothing, Unit]
          publishCalls <- Ref.make(0)
          owner <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config) {
            publishCalls.update(_ + 1) *> ownerStarted.succeed(()).unit *> finishOwner.await
          }.fork
          _ <- ownerStarted.await
          contender <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config) {
            publishCalls.update(_ + 1)
          }.fork
          _ <- ZIO.sleep(config.uncertaintyTtl * 2)
          callsWhileOwnerRunning <- publishCalls.get
          _ <- finishOwner.succeed(())
          ownerOutcome <- owner.join
          contenderOutcome <- contender.join
          totalCalls <- publishCalls.get
        yield assertTrue(
          callsWhileOwnerRunning == 1,
          totalCalls == 1,
          ownerOutcome == GavPublicationGuard.Outcome.Published,
          contenderOutcome == GavPublicationGuard.Outcome.Joined,
        )
      }
    },
    test("completion marker makes later instances join without rechecking or publishing") {
      withGav { gav =>
        for
          publishCalls <- Ref.make(0)
          first <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config)(publishCalls.update(_ + 1))
          second <- GavPublicationGuard.publish(gav, ZIO.dieMessage("completion marker should skip Maven recheck"), config) {
            ZIO.dieMessage("completion marker should skip publish")
          }
          totalCalls <- publishCalls.get
        yield assertTrue(
          first == GavPublicationGuard.Outcome.Published,
          second == GavPublicationGuard.Outcome.Joined,
          totalCalls == 1,
        )
      }
    },
    test("failed owner leaves an uncertainty window before a later attempt can publish") {
      withGav { gav =>
        for
          publishCalls <- Ref.make(0)
          first <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config) {
            publishCalls.update(_ + 1) *> ZIO.fail(RuntimeException("simulated publish failure"))
          }.exit
          blocked <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config)(publishCalls.update(_ + 1)).exit
          _ <- ZIO.sleep(config.uncertaintyTtl + 100.millis)
          retry <- GavPublicationGuard.publish(gav, ZIO.succeed(false), config)(publishCalls.update(_ + 1))
          totalCalls <- publishCalls.get
        yield assertTrue(
          first.isFailure,
          blocked.is(_.failure).isInstanceOf[GavPublicationGuard.PreviousAttemptFailed],
          retry == GavPublicationGuard.Outcome.Published,
          totalCalls == 2,
        )
      }
    },
    test("expired lease is recovered and Maven is rechecked before publishing") {
      withGav { gav =>
        for
          _ <- GavPublicationGuard.claim(gav, UUID.randomUUID(), 100.millis)
          publishCalls <- Ref.make(0)
          outcome <- GavPublicationGuard.publish(gav, ZIO.succeed(true), config)(publishCalls.update(_ + 1))
          totalCalls <- publishCalls.get
        yield assertTrue(
          outcome == GavPublicationGuard.Outcome.AlreadyPublished,
          totalCalls == 0,
        )
      }
    },
    test("rapid heartbeats cannot turn successful completion into lease loss") {
      val rapid = GavPublicationGuard.Config.make(
        uncertaintyTtl = 500.millis,
        heartbeatEvery = 5.millis,
        completionTtl = 5.seconds,
        pollEvery = 5.millis,
        contentionTimeout = 1.second,
      ).toOption.get

      ZIO.foreach(1 to 20) { _ =>
        withGav { gav =>
          for
            outcome <- GavPublicationGuard.publish(gav, ZIO.succeed(false), rapid)(ZIO.sleep(10.millis))
            state <- GavPublicationGuard.state(gav)
          yield outcome == GavPublicationGuard.Outcome.Published && state.exists(_.phase == GavPublicationGuard.Phase.Complete)
        }
      }.map(results => assertTrue(results.forall(identity)))
    },
    test("contention times out without publishing") {
      val shortWait = GavPublicationGuard.Config.make(
        uncertaintyTtl = 2.seconds,
        heartbeatEvery = 100.millis,
        completionTtl = 5.seconds,
        pollEvery = 20.millis,
        contentionTimeout = 100.millis,
      ).toOption.get

      withGav { gav =>
        for
          _ <- GavPublicationGuard.claim(gav, UUID.randomUUID(), 2.seconds)
          publishCalls <- Ref.make(0)
          result <- GavPublicationGuard.publish(gav, ZIO.succeed(false), shortWait)(publishCalls.update(_ + 1)).exit
          totalCalls <- publishCalls.get
        yield assertTrue(
          result.is(_.failure).isInstanceOf[GavPublicationGuard.PublicationInProgress],
          totalCalls == 0,
        )
      }
    },
    test("configuration rejects unsafe timing relationships") {
      val heartbeatTooSlow = GavPublicationGuard.Config.make(
        uncertaintyTtl = 1.second,
        heartbeatEvery = 1.second,
        completionTtl = 1.second,
        pollEvery = 10.millis,
        contentionTimeout = 1.second,
      )
      val zeroPoll = GavPublicationGuard.Config.make(
        uncertaintyTtl = 1.second,
        heartbeatEvery = 100.millis,
        completionTtl = 1.second,
        pollEvery = Duration.Zero,
        contentionTimeout = 1.second,
      )
      assertTrue(heartbeatTooSlow.isLeft, zeroPoll.isLeft)
    },
    test("an old owner cannot release a replacement owner's lease") {
      withGav { gav =>
        for
          replacement = UUID.randomUUID()
          oldOwner = UUID.randomUUID()
          _ <- GavPublicationGuard.claim(gav, replacement, 10.seconds)
          released <- GavPublicationGuard.release(gav, oldOwner)
          transitioned <- GavPublicationGuard.transition(gav, oldOwner, GavPublicationGuard.Phase.Complete, 10.seconds)
          remaining <- GavPublicationGuard.state(gav)
        yield assertTrue(
          !released,
          !transitioned,
          remaining.contains(GavPublicationGuard.State(GavPublicationGuard.Phase.Publishing, replacement)),
        )
      }
    },
  ).provide(TestInfrastructure.databaseLayer) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds)
