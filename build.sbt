name := "webjars"

scalaVersion := "3.8.4"

enablePlugins(JavaAppPackaging)

// Lock down the primary launcher so `bin/webjars` keeps booting the server
// `Main` even though we now ship a second main class. sbt-native-packager
// auto-generates `bin/deploy` from `webjars.Deploy` because of multiple
// discovered main classes — we don't need a custom mapping for it.
Compile / mainClass := Some("webjars.Main")

val zioVersion = "2.1.26"
val zioHttpVersion = "3.11.2"
val zioJsonVersion = "0.9.2"
val zioConfigVersion = "4.0.7"
val zioLoggingVersion = "2.5.3"
// Pinned at 1.1.4 — 2.x hangs the tar tests in this project
// (`ArchiveCreator - tar with file excludes`, etc.). Revisit when 2.x
// either documents the migration or those tests are rewritten.
val zioStreamsCompressVersion = "1.1.4"

libraryDependencies ++= Seq(
  // ZIO core
  "dev.zio" %% "zio"                          % zioVersion,
  "dev.zio" %% "zio-streams"                  % zioVersion,
  "dev.zio" %% "zio-direct"                   % "1.0.0-RC7",

  // ZIO HTTP (web framework + client + template2)
  "dev.zio" %% "zio-http"                     % zioHttpVersion,

  // JSON
  "dev.zio" %% "zio-json"                     % zioJsonVersion,

  // Configuration
  "dev.zio" %% "zio-config"                   % zioConfigVersion,
  "dev.zio" %% "zio-config-typesafe"          % zioConfigVersion,

  // Logging
  "dev.zio" %% "zio-logging"                  % zioLoggingVersion,
  "org.slf4j" % "slf4j-simple"                % "2.0.18",

  // Redis
  "dev.zio" %% "zio-redis"                    % "1.2.1",

  "com.jamesward" %% "zio-http-guard" % "0.0.1",

  // In-process cache
  "dev.zio" %% "zio-cache"                    % "0.2.8",

  // Archive handling (ZIO Streams Compress)
  "dev.zio" %% "zio-streams-compress-tar"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-zip"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-gzip"    % zioStreamsCompressVersion,

  // XML
  "org.scala-lang.modules" %% "scala-xml"     % "2.4.0",

  // Maven Central
  "com.jamesward" %% "zio-mavencentral"       % "0.12.0",

  // Git
  "org.eclipse.jgit" % "org.eclipse.jgit"     % "7.6.0.202603022253-r",

  // Hashing
  "com.outr" %% "hasher"                      % "1.2.3",

  // WebJars
  //  • `WebJar` scope — version source for the generated locator (build-time only).
  //  • `Sass`   scope — SCSS sources made available to sbt-sass at compile-time.
  //  • `Test`   scope — JAR present at runtime in reStartTest (local /webjars/).
  // Prod jar contains none of these; URLs are baked at build time.
  "org.webjars.npm" % "bootstrap"                 % "5.3.8"   % Set(WebJar, Test, Sass),
  "org.webjars"     % "highlightjs"               % "11.11.1" % Set(WebJar, Test),
  // jquery pinned to 3.7.1 — jquery 4.0.0 drops legacy APIs that
  // select2 4.0.13 (still our pinned version) relies on.
  "org.webjars"     % "jquery"                    % "4.0.0"  % Set(WebJar, Test),
  "org.webjars"     % "select2"                   % "4.0.13" % Set(WebJar, Test),
  "org.webjars.npm" % "select2-bootstrap-5-theme" % "1.3.0"  % Set(WebJar, Test),
  "org.webjars.npm" % "jquery.typewatch"          % "2.1.0"  % Set(WebJar, Test),

  // Testing
  "dev.zio" %% "zio-test"                     % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"                 % zioVersion % Test,
  "dev.zio" %% "zio-http-testkit"             % zioHttpVersion % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test,
)

fork := true

// JDK 25: netty-epoll initialization touches `sun.misc.Unsafe`, which is
// restricted by default in JDK 25+. Allow it explicitly so the zio-http
// server boots. Also silences the matching warnings from netty/scala-library.
javaOptions ++= Seq(
  "--enable-native-access=ALL-UNNAMED",
  "--sun-misc-unsafe-memory-access=allow",
)

scalacOptions ++= Seq(
  "-explain",
  "-feature",
  "-Werror",
  "-Wunused:implicits",
  "-Wunused:locals",
  "-Wunused:privates",
  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
  "-Wconf:msg=The method `apply` is inserted:s",
  "-Wconf:msg=Flag .* set repeatedly:s",
)

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty

// Boot the full server with the same testcontainer-backed valkey + literal
// test config used by the unit-test layer. Useful for live smoke tests
// (`test-integration.sh`) and for `reStartTest` below.
lazy val runTest = taskKey[Unit]("run WebJarsTestApp")

runTest := (Test / runMain).toTask(" webjars.WebJarsTestApp").value

// Re-run `WebJarsTestApp` in a forked JVM, restarting on file changes.
// Mirrors the `reStartTest` task in javadoccentral.
lazy val reStartTest =
  inputKey[spray.revolver.AppProcess]("re-start, but test")

reStartTest :=
  Def.inputTask {
    spray.revolver.Actions.restartApp(
      streams.value,
      reLogTag.value,
      thisProjectRef.value,
      reForkOptions.value,
      Some("webjars.WebJarsTestApp"),
      (Test / fullClasspath).value,
      reStartArgs.value,
      spray.revolver.Actions.startArgsParser.parsed
    )
  }.dependsOn(Compile / products).evaluated
