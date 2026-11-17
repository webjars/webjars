package webjars

import zio.*
import zio.logging.*
import zio.logging.slf4j.bridge.Slf4jBridge

/**
 * Shared logger bootstrap for [[Main]] and the test app.
 *
 * Uses [[LogFormat.default]] but appends [[LogFormat.allAnnotations]] so that
 * keys set via `ZIO.logAnnotate` actually appear in console output. In
 * particular this is what makes `Middleware.requestLogging` useful — without
 * the annotations we'd just see "Http request served" with no method, URL,
 * status, duration, or User-Agent.
 *
 * The SLF4J v2 bridge ([[Slf4jBridge]]) routes all SLF4J logging from
 * third-party libraries (testcontainers, netty, the Postgres driver, HikariCP, Flyway, …) into ZIO logging.
 * It is the only SLF4J provider on the classpath — there is no
 * `slf4j-simple` binding — so there is no "Failed to load StaticLoggerBinder
 * / NOP logger" warning and no risk of circular logging.
 */
object Logging:

  private val format: LogFormat =
    LogFormat.default |-| LogFormat.allAnnotations

  val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.removeDefaultLoggers >>> consoleLogger(
      ConsoleLoggerConfig(format, LogFilter.LogLevelByNameConfig.default),
    // Without FiberRef propagation because of https://github.com/zio/zio-logging/issues/1180:
    // `initialize` runs every FiberRef's `join` on each slf4j log call, and zio-magnum's join
    // closes the transaction's connection. The cost is that slf4j lines don't carry the
    // calling fiber's annotations/spans. Switch back to `Slf4jBridge.initialize` once a fixed
    // release is out.
    ) >+> Slf4jBridge.initializeWithoutFiberRefPropagation
