enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "3.8.1"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,
  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.apache.commons" % "commons-compress" % "1.28.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "7.5.0.202512021534-r",
  "org.bouncycastle" % "bcpg-jdk18on" % "1.83",
  "org.bouncycastle" % "bcutil-jdk18on" % "1.83",
  "com.outr" %% "hasher" % "1.2.3",
  "com.indoorvivants" %% "scala-uri" % "4.2.0",
  "com.jamesward" %% "zio-mavencentral" % "0.2.0",
  "org.webjars" %% "webjars-play" % "3.0.10",
  // brought in transatively via select2-bootstrap-5-theme
  //"org.webjars" % "bootstrap" % "5.3.3",
  "org.webjars" % "highlightjs" % "11.11.1",
  "org.webjars" % "jquery" % "3.7.1",
  "org.webjars" % "select2" % "4.0.13",
  "org.webjars.npm" % "select2-bootstrap-5-theme" % "1.3.0",
  "org.webjars.npm" % "jquery.typewatch" % "2.1.0",
  specs2 % Test
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

pipelineStages := Seq(gzip, digest)

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty
