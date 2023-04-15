enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,

  // https://github.com/playframework/playframework/releases/2.8.15
  "com.google.inject" % "guice" % "5.1.0",
  "com.google.inject.extensions" % "guice-assistedinject" % "5.1.0",

  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "3.0.1",
  "org.apache.commons" % "commons-compress" % "1.23.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "6.5.0.202303070854-r",
  "org.bouncycastle" % "bcpg-jdk18on" % "1.73",
  "com.outr" %% "hasher" % "1.2.2",
  "org.webjars" %% "webjars-play" % "2.8.18",
  "org.webjars.bower" % "bootstrap" % "3.3.7",
  "org.webjars.bower" % "select2" % "4.0.13",
  "org.webjars.bower" % "highlightjs" % "9.12.0",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1",
  specs2 % Test
)

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
  "-Ywarn-dead-code",
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
