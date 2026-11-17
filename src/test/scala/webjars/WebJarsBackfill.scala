package webjars

import webjars.utils.Db
import zio.*
import zio.direct.*
import zio.http.Client

import javax.sql.DataSource

/**
 * One-time migration (see MIGRATION.md): applies the Flyway migrations, then
 * loads every WebJar listed by the live (Valkey) site's `/all` into Postgres
 * through `WebJarsCache.setArtifactDetails`. Upserts, so safe to re-run.
 *
 *   ./sbta "Test/runMain webjars.WebJarsBackfill heroku"
 *
 * `heroku` reads the target database's URL with `heroku config:get DATABASE_URL`;
 * otherwise the `DATABASE_URL` environment variable is used.
 */
object WebJarsBackfill extends ZIOAppDefault:

  private def herokuDatabaseUrl: Task[Db.Jdbc] =
    ZIO.attemptBlocking:
      val process = ProcessBuilder("heroku", "config:get", "DATABASE_URL").redirectError(ProcessBuilder.Redirect.INHERIT).start().nn
      String(process.getInputStream.nn.readAllBytes().nn).trim
    .flatMap(url => ZIO.fromEither(Db.parse(url)).mapError(e => RuntimeException(s"DATABASE_URL: $e")))

  private val backfill: ZIO[Client & DataSource, Throwable, Unit] =
    defer:
      val migrations = ZIO.serviceWithZIO[DataSource](FlywayMigrate.migrate).run
      val (duration, written) = CacheHydrate.fromUrl(CacheHydrate.DefaultUrl).timed.run
      Console.printLine(s"migrations applied: $migrations; $written WebJars written in ${duration.toSeconds}s").run

  def run =
    defer:
      val args = getArgs.run
      val jdbc = (if args.contains("heroku") then herokuDatabaseUrl else Db.jdbcFromEnv).run
      backfill.provide(Client.default, Db.dataSourceLayer(jdbc, "backfill", 5)).run
