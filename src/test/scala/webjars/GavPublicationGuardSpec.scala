package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.GavPublicationGuard
import zio.*
import zio.redis.Redis
import zio.test.*

object GavPublicationGuardSpec extends ZIOSpecDefault:

  private val config = GavPublicationGuard.Config.make(
    uncertaintyTtl = 500.millis,
    heartbeatEvery = 50.millis,
    completionTtl = 10.seconds,
    pollEvery = 20.millis,
    contentionTimeout = 5.seconds,
  ).toOption.get

  private def clear(gav: MavenCentral.GroupArtifactVersion): ZIO[Redis, Throwable, Unit] =
    ZIO.serviceWithZIO[Redis] { redis =>
      redis.del(GavPublicationGuard.stateKey(gav)).unit
    }

  private def withGav[A](test: MavenCentral.GroupArtifactVersion => ZIO[Redis, Throwable, A]): ZIO[Redis, Throwable, A] =
    Random.nextUUID.flatMap { id =>
      val gav = MavenCentral.gav("org.webjars.test", s"guard-$id", "1.0.0")
      clear(gav) *> test(gav).ensuring(clear(gav).ignoreLogged)
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
          redis <- ZIO.service[Redis]
          _ <- redis.set(GavPublicationGuard.stateKey(gav), "publishing:abandoned-owner", expireTime = Some(100.millis))
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
            redis <- ZIO.service[Redis]
            state <- redis.get(GavPublicationGuard.stateKey(gav)).returning[String]
          yield outcome == GavPublicationGuard.Outcome.Published && state.exists(_.startsWith("complete:"))
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
          redis <- ZIO.service[Redis]
          _ <- redis.set(GavPublicationGuard.stateKey(gav), "publishing:other", expireTime = Some(2.seconds))
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
          redis <- ZIO.service[Redis]
          key = GavPublicationGuard.stateKey(gav)
          _ <- redis.set(key, "publishing:replacement-owner")
          released <- GavPublicationGuard.release(redis, key, "publishing:old-owner")
          transitioned <- GavPublicationGuard.transition(
            redis,
            key,
            "publishing:old-owner",
            "complete:old-owner",
            10.seconds,
          )
          remaining <- redis.get(key).returning[String]
        yield assertTrue(
          !released,
          !transitioned,
          remaining.contains("publishing:replacement-owner"),
        )
      }
    },
  ).provide(TestInfrastructure.sharedRedisLayer) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(30.seconds)
