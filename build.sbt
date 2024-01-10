enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,
  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "3.0.2",
  "org.apache.commons" % "commons-compress" % "1.25.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "6.8.0.202311291450-r",
  "org.bouncycastle" % "bcpg-jdk18on" % "1.77",
  "com.outr" %% "hasher" % "1.2.2",
  "org.webjars" %% "webjars-play" % "3.0.1",
  "org.webjars.bower" % "bootstrap" % "3.4.1",
  "org.webjars.bower" % "select2" % "3.5.4",
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
