name := "webjars"

scalaVersion := "3.8.2"

val zioVersion = "2.1.24"
val zioHttpVersion = "3.8.1"
val zioJsonVersion = "0.9.0"
val zioConfigVersion = "4.0.6"
val zioLoggingVersion = "2.5.3"
val zioStreamsCompressVersion = "1.1.3"

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
  "dev.zio" %% "zio-logging-slf4j2"           % zioLoggingVersion,
  "ch.qos.logback" % "logback-classic"         % "1.5.18",

  // Redis
  "dev.zio" %% "zio-redis"                    % "1.1.13",

  // Archive handling (ZIO Streams Compress)
  "dev.zio" %% "zio-streams-compress-tar"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-zip"     % zioStreamsCompressVersion,
  "dev.zio" %% "zio-streams-compress-gzip"    % zioStreamsCompressVersion,

  // XML
  "org.scala-lang.modules" %% "scala-xml"     % "2.3.0",

  // Maven Central
  "com.jamesward" %% "zio-mavencentral"       % "0.5.3",

  // Parsing & URIs
  "com.lihaoyi" %% "fastparse"                % "3.1.1",
  "com.indoorvivants" %% "scala-uri"           % "4.2.0",

  // Git
  "org.eclipse.jgit" % "org.eclipse.jgit"     % "7.5.0.202512021534-r",

  // Crypto (GPG signing for Maven Central)
  "org.bouncycastle" % "bcpg-jdk18on"         % "1.83",
  "org.bouncycastle" % "bcutil-jdk18on"        % "1.83",

  // Hashing
  "com.outr" %% "hasher"                      % "1.2.3",

  // WebJars (frontend dependencies served from classpath)
  // brought in transitively via select2-bootstrap-5-theme
  //"org.webjars" % "bootstrap" % "5.3.3",
  "org.webjars" % "highlightjs"                % "11.11.1",
  "org.webjars" % "jquery"                     % "3.7.1",
  "org.webjars" % "select2"                    % "4.0.13",
  "org.webjars.npm" % "select2-bootstrap-5-theme" % "1.3.0",
  "org.webjars.npm" % "jquery.typewatch"       % "2.1.0",

  // Testing
  "dev.zio" %% "zio-test"                     % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt"                 % zioVersion % Test,
  "dev.zio" %% "zio-http-testkit"             % zioHttpVersion % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test,
)

libraryDependencySchemes += "dev.zio" %% "zio-json" % VersionScheme.Always

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

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
