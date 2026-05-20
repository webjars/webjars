package webjars

import zio.*
import zio.logging.*

/**
 * Shared logger bootstrap for [[Main]] and the test app.
 *
 * Uses [[LogFormat.default]] but appends [[LogFormat.allAnnotations]] so that
 * keys set via `ZIO.logAnnotate` actually appear in console output. In
 * particular this is what makes `Middleware.requestLogging` useful — without
 * the annotations we'd just see "Http request served" with no method, URL,
 * status, duration, or User-Agent.
 */
object Logging:

  private val format: LogFormat =
    LogFormat.default |-| LogFormat.allAnnotations

  val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.removeDefaultLoggers >>> consoleLogger(
      ConsoleLoggerConfig(format, LogFilter.LogLevelByNameConfig.default),
    )
