enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.13.11"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,

  "dev.zio" %% "zio" % "2.0.15",
  "dev.zio" %% "zio-streams" % "2.0.15",
  "dev.zio" %% "zio-http" % "3.0.0-RC2",
  "dev.zio" %% "zio-json" % "0.6.0",
  "dev.zio" %% "zio-direct" % "1.0.0-RC7",

  // https://github.com/playframework/playframework/releases/2.8.15
  "com.google.inject" % "guice" % "5.1.0",
  "com.google.inject.extensions" % "guice-assistedinject" % "5.1.0",

  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "3.0.1",
  "org.apache.commons" % "commons-compress" % "1.23.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "6.6.0.202305301015-r",
  "org.bouncycastle" % "bcpg-jdk18on" % "1.75",
  "com.outr" %% "hasher" % "1.2.2",
  "org.webjars" %% "webjars-play" % "2.8.18",
  "org.webjars.bower" % "bootstrap" % "3.4.1",
  "org.webjars.bower" % "select2" % "3.5.4",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1",

  specs2 % Test,

  "dev.zio" %% "zio-test" % "2.0.15" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.0.15" % Test,
  "dev.zio" %% "zio-test-magnolia" % "2.0.15" % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-Xcheckinit",
  "-Xfatal-warnings",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  //"-Ywarn-dead-code", // disabled due to zio-direct issue
  "-Ywarn-extra-implicit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:locals",
  //"-Ywarn-unused:params", // disabled because Play routes needs a param to match a path
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
)

pipelineStages := Seq(gzip, digest)

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty
