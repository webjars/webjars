name := "webjars"

scalaVersion := "3.9.0"

// --- Local library co-development -------------------------------------------
// `-Dlocal` swaps published artifacts for source checkouts under `../../` when
// present, e.g. `./sbt -Dlocal test`. This mirrors the pattern in
// ../../toolbook/build.sbt while keeping this single-module build flat.
// Normal and CI builds use the published artifacts declared below.
val zioGitDir                = file("../../zio-git")
val zioMavenCentralDir       = file("../../zio-mavencentral")
val useLocalSubprojects      = sys.props.get("local").isDefined
val zioGitLocal              = useLocalSubprojects && zioGitDir.exists()
val zioMavenCentralLocal     = useLocalSubprojects && zioMavenCentralDir.exists()

// Source dependencies wired onto the root project below only under `-Dlocal`.
val localSubprojectDeps: Seq[ClasspathDep[ProjectReference]] =
  (if (zioGitLocal) Seq(RootProject(zioGitDir): ClasspathDep[ProjectReference]) else Seq.empty) ++
    (if (zioMavenCentralLocal) Seq(RootProject(zioMavenCentralDir): ClasspathDep[ProjectReference]) else Seq.empty)

// Published artifacts are used only when their local checkout is not active,
// so source projects and published jars never both land on the classpath.
libraryDependencies ++=
  (if (zioGitLocal) Seq.empty else Seq("com.jamesward" %% "zio-git" % "0.0.3")) ++
  (if (zioMavenCentralLocal) Seq.empty
   else Seq("com.jamesward" %% "zio-mavencentral" % "0.14.0"))

// Lock down the primary launcher so `bin/webjars` keeps booting the server
// `Main` even though we now ship a second main class. sbt-native-packager
// auto-generates `bin/deploy` from `webjars.Deploy` because of multiple
// discovered main classes — we don't need a custom mapping for it.
Compile / mainClass := Some("webjars.Main")

val zioStreamsCompressVersion = "2.1.4"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-config-typesafe"          % "4.1.0",
  "dev.zio" %% "zio-logging-slf4j2-bridge"    % "2.5.3",
  "dev.zio" %% "zio-redis"                    % "1.3.0",
  "dev.zio" %% "zio-cache"                    % "0.3.0",
  "com.jamesward" %% "zio-http-guard"         % "0.0.2",

  "dev.zio" %% "zio-streams-compress-tar"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-zip"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-gzip"    % zioStreamsCompressVersion,

  // WebJars
  //  • `WebJar` scope — version source for the generated locator (build-time only).
  //  • `Sass`   scope — SCSS sources made available to sbt-sass at compile-time.
  //  • `Test`   scope — JAR present at runtime in reStartTest (local /webjars/).
  // Prod jar contains none of these; URLs are baked at build time.
  "org.webjars.npm" % "bootstrap"                 % "5.3.8"   % Set(WebJar, Test, Sass),
  "org.webjars"     % "highlightjs"               % "11.11.1" % Set(WebJar, Test),

  "dev.zio" %% "zio-test-sbt"                 % ("dev.zio" %% "zio").version % Test,
  "dev.zio" %% "zio-http-testkit"             % ("dev.zio" %% "zio-http").version % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test,
  "rocks.earlyeffect" %% "chekhov-zio-test"   % "0.0.5" % Test,

  "com.jamesward" % "skills" % "0.0.3" % Skills,
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

//// Boot the full server with the same testcontainer-backed valkey + literal
//// test config used by the unit-test layer. Useful for live smoke tests
//// (`test-integration.sh`) and for `Test/runReload` below.
//@transient lazy val runTest = taskKey[Unit]("run WebJarsTestApp")
//
//runTest := (Test / runMain).toTask(" webjars.WebJarsTestApp").value

Test / mainClass := Some("webjars.WebJarsTestApp")

// Explicit root project so we can attach optional local source dependencies
// under `-Dlocal` (a bare build.sbt can't call `.dependsOn`). Under sbt 2.0,
// the bare settings above are applied to every subproject, so this root still
// picks them all up and the build stays flat. Published artifacts are used for
// any local checkout that is absent.
lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .dependsOn(localSubprojectDeps *)

skillsJarsOutputDir := Some(file(".kiro/skills"))

mcpEnabled := true        // default: false
mcpPort    := 5055        // default: 5010
