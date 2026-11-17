package webjars

import org.flywaydb.core.Flyway
import webjars.utils.Db
import zio.*

import javax.sql.DataSource

/**
 * Applies the SQL migrations in `src/main/resources/db/migration`. Runs as
 * `bin/flyway-migrate` in Heroku's release phase (see Procfile), after the slug
 * is built and before the new release serves traffic. A failure aborts the
 * release, so a deploy never runs against a schema it doesn't expect.
 */
object FlywayMigrate extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = Logging.bootstrap

  def migrate(dataSource: DataSource): Task[Int] =
    ZIO.attemptBlocking:
      Flyway.configure(getClass.getClassLoader)
        .dataSource(dataSource)
        .locations("classpath:db/migration")
        .load()
        .migrate()
        .migrationsExecuted

  // The layer's Scope closes the pool (and HikariCP's housekeeping thread)
  // before exit; otherwise the JVM would not terminate after migrating.
  def run =
    ZIO.serviceWithZIO[DataSource](migrate)
      .flatMap(n => ZIO.logInfo(s"flyway: applied $n migration(s)"))
      .provide(Db.fromEnv("flyway", 2))
