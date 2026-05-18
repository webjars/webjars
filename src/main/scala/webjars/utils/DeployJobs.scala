package webjars.utils

import zio.*
import zio.direct.*
import zio.http.Client
import zio.redis.Redis
import zio.stream.ZStream

trait DeployJobs[Env]:
  /** Start or join an in-process deployment job. Every subscriber sees the
   *  full message stream from the beginning, even if they attach after the
   *  job is already running. Concurrent requests for the same
   *  (deployable, nameOrUrlish, upstreamVersion) — including transitive
   *  dependencies of other in-flight jobs — share a single deploy. */
  def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean = true): ZStream[Client & Redis & Env, Nothing, String]

object DeployJobs:

  private case class Key(deployableName: String, nameOrUrlish: String, upstreamVersion: String)

  private case class Job(
    log: Ref[Chunk[String]],
    subs: Ref[Set[Queue[Option[String]]]],
    lock: Semaphore,
    done: Promise[Nothing, Unit],
  )

  private object Job:
    val make: UIO[Job] =
      defer:
        val log = Ref.make(Chunk.empty[String]).run
        val subs = Ref.make(Set.empty[Queue[Option[String]]]).run
        val lock = Semaphore.make(1).run
        val done = Promise.make[Nothing, Unit].run
        Job(log, subs, lock, done)

  // Job entries are evicted this long after completion so quick reconnects
  // see the full transcript but the map doesn't grow unboundedly.
  private val completedJobTtl: Duration = 5.minutes

  def live[Env : Tag]: ZLayer[DeployWebJar[Env], Nothing, DeployJobs[Env]] =
    ZLayer.fromZIO:
      defer:
        val deployWebJar = ZIO.service[DeployWebJar[Env]].run
        val jobs = Ref.Synchronized.make(Map.empty[Key, Job]).run
        DeployJobsLive[Env](deployWebJar, jobs)

  private case class DeployJobsLive[Env](
    deployWebJar: DeployWebJar[Env],
    jobs: Ref.Synchronized[Map[Key, Job]],
  ) extends DeployJobs[Env]:

    def deploy(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean = true): ZStream[Client & Redis & Env, Nothing, String] =
      val key = Key(deployable.name, nameOrUrlish, upstreamVersion)
      ZStream.unwrap:
        defer:
          val (job, isNew) = jobs.modifyZIO { current =>
            current.get(key) match
              case Some(existing) => ZIO.succeed(((existing, false), current))
              case None           => Job.make.map(j => ((j, true), current + (key -> j)))
          }.run

          if isNew then
            runProducer(deployable, nameOrUrlish, upstreamVersion, deployDependencies, job, key).forkDaemon.run

          subscribe(job)

    private def runProducer(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, deployDependencies: Boolean, job: Job, key: Key): URIO[Client & Redis & Env, Unit] =
      val work: ZIO[Client & Redis & Env, Throwable, Unit] =
        ZIO.scoped[Client & Redis & Env]:
          defer:
            if deployDependencies then runDependencyDeploys(deployable, nameOrUrlish, upstreamVersion, job).run
            deployWebJar.deploy(deployable, nameOrUrlish, upstreamVersion)
              .runForeach(msg => publish(job, msg))
              .run

      work.foldZIO(
        e => publish(job, Option(e.getMessage).getOrElse(e.getClass.getSimpleName)),
        _ => ZIO.unit,
      ) *> finishJob(job) *> scheduleCleanup(key, job)

    private def runDependencyDeploys(deployable: Deployable, nameOrUrlish: String, upstreamVersion: String, job: Job): ZIO[Scope & Client & Redis & Env, Throwable, Unit] =
      defer:
        publish(job, "Determining dependency graph").run
        val packageInfo = deployable.info(nameOrUrlish, upstreamVersion).run
        val depGraph = deployable.depGraph(packageInfo).run
        val intro =
          if depGraph.isEmpty then "No dependencies."
          else "Deploying these dependencies:\n  " + depGraph.map((n, v) => s"$n#$v").mkString("\n  ")
        publish(job, intro).run
        ZIO.foreachDiscard(depGraph) { (depName, depVersion) =>
          publish(job, s"Deploying Dependency: ${deployable.name} $depName $depVersion") *>
            deploy(deployable, depName, depVersion, deployDependencies = false).runForeach(m => publish(job, m))
        }.run

    private def publish(job: Job, msg: String): UIO[Unit] =
      job.lock.withPermit:
        job.log.update(_ :+ msg) *>
        job.subs.get.flatMap(qs => ZIO.foreachDiscard(qs)(_.offer(Some(msg))))

    private def finishJob(job: Job): UIO[Unit] =
      job.lock.withPermit:
        job.subs.get.flatMap(qs => ZIO.foreachDiscard(qs)(_.offer(None))) *>
        job.done.succeed(()).unit

    private def subscribe(job: Job): ZStream[Any, Nothing, String] =
      ZStream.unwrap:
        job.lock.withPermit:
          defer:
            val snapshot = job.log.get.run
            val q = Queue.unbounded[Option[String]].run
            val alreadyDone = job.done.isDone.run
            if alreadyDone then q.offer(None).run
            else job.subs.update(_ + q).run
            val live = ZStream.fromQueue(q).collectWhileSome.ensuring:
              job.lock.withPermit(job.subs.update(_ - q)) *> q.shutdown
            ZStream.fromChunk(snapshot) ++ live

    private def scheduleCleanup(key: Key, job: Job): UIO[Unit] =
      (job.done.await *> ZIO.sleep(completedJobTtl) *> jobs.update(_ - key)).forkDaemon.unit
