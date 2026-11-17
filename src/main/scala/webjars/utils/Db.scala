package webjars.utils

import com.augustnagro.magnum.*
import com.augustnagro.magnum.ziomagnum.*
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import zio.*
import zio.direct.*

import java.net.{URI, URLDecoder}
import java.nio.charset.StandardCharsets.UTF_8
import javax.sql.DataSource
import scala.util.Try

/** Postgres connection setup, shared by the processes (`Main`, `Deploy`, `Cleanup`) and the release-phase migrator (`FlywayMigrate`). */
object Db:

  case class Jdbc(url: String, user: String, password: String) derives CanEqual

  // magnum needs a `SqlLogger` for every query. This one never touches the
  // events' untyped `params` (bind values), only the SQL text and the cause.
  given SqlLogger = new SqlLogger:
    def log(event: SqlSuccessEvent): Unit = ()
    def exceptionMsg(event: SqlExceptionEvent): String =
      s"error executing query:\n${event.sql}\n${event.cause}"

  /**
   * Heroku's `DATABASE_URL` is libpq-style (`postgres://user:pass@host:port/db`).
   * Heroku Postgres requires TLS, so `sslmode=require` is added unless the URL
   * sets an `sslmode` (local plaintext dev uses `?sslmode=disable`).
   */
  def parse(raw: String): Either[String, Jdbc] =
    for
      uri    <- Try(URI(raw)).toEither.left.map(t => s"not a URI: ${t.getMessage}")
      _      <- Option(uri.getScheme).filter(s => s == "postgres" || s == "postgresql").toRight("scheme must be postgres://")
      userPw <- Option(uri.getRawUserInfo).toRight("no user:password")
      creds  <- userPw.split(":", 2) match
                  case Array(u, p) => Right((URLDecoder.decode(u, UTF_8), URLDecoder.decode(p, UTF_8)))
                  case _           => Left("no user:password")
      host   <- Option(uri.getHost).filter(_.nonEmpty).toRight("no host")
      db     <- Option(uri.getPath).map(_.stripPrefix("/")).filter(_.nonEmpty).toRight("no database name")
    yield
      val port = if uri.getPort > 0 then uri.getPort else 5432
      val query = Option(uri.getRawQuery).filter(_.nonEmpty) match
        case Some(q) if q.contains("sslmode=") => q
        case Some(q)                           => s"$q&sslmode=require"
        case None                              => "sslmode=require"
      Jdbc(s"jdbc:postgresql://$host:$port/$db?$query", creds._1, creds._2)

  val jdbcFromEnv: ZIO[Any, Throwable, Jdbc] =
    defer:
      val url = ZIO.systemWith(_.env("DATABASE_URL")).someOrFail(RuntimeException("DATABASE_URL env var not set")).run
      ZIO.fromEither(parse(url)).mapError(e => RuntimeException(s"DATABASE_URL: $e")).run

  /**
   * A small HikariCP pool. `initializationFailTimeout = -1` lets the process
   * start while Postgres is unreachable: queries fail (and are logged) until
   * the database is back, and the in-memory snapshots keep their last state.
   */
  def dataSourceLayer(jdbc: Jdbc, poolName: String, maxPoolSize: Int): ZLayer[Any, Throwable, DataSource] =
    ZLayer.scoped:
      ZIO.fromAutoCloseable:
        ZIO.attemptBlocking:
          val config = HikariConfig()
          config.setJdbcUrl(jdbc.url)
          config.setUsername(jdbc.user)
          config.setPassword(jdbc.password)
          config.setPoolName(poolName)
          config.setMaximumPoolSize(maxPoolSize)
          config.setInitializationFailTimeout(-1)
          HikariDataSource(config)
    .map(env => ZEnvironment[DataSource](env.get))

  def fromEnv(poolName: String, maxPoolSize: Int): ZLayer[Any, Throwable, DataSource] =
    ZLayer.fromZIO(jdbcFromEnv).flatMap(env => dataSourceLayer(env.get, poolName, maxPoolSize))

  def withDataSource[A](f: DataSource ?=> Task[A]): RIO[DataSource, A] =
    ZIO.serviceWithZIO[DataSource](dataSource => f(using dataSource))

  /** `d` as a Postgres interval, e.g. `now() + ${Db.interval(7.days)}`. */
  def interval(d: Duration): Frag =
    sql"make_interval(secs => ${d.toMillis / 1000.0})"

  /** Deletes the rows whose time is up: they used to expire through a Redis TTL. Readers already ignore them. */
  val purgeExpired: RIO[DataSource, Int] =
    withDataSource:
      transaction("purge expired"):
        defer:
          val artifacts   = sql"delete from artifact_tombstone where expires_at <= now()".zUpdate.run
          val versions    = sql"delete from version_tombstone where expires_at <= now()".zUpdate.run
          val publication = sql"delete from gav_publication where expires_at <= now()".zUpdate.run
          val popular     = sql"delete from popular_daily where day < (now() at time zone 'UTC')::date - ${PopularMetrics.RetentionDays}".zUpdate.run
          artifacts + versions + publication + popular
