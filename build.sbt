enablePlugins(PlayScala)

name := "webjars"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  ws,
  caffeine,
  guice,
  filters,
  "net.spy" % "spymemcached" % "2.12.3",
  "com.lihaoyi" %% "fastparse" % "2.3.0",
  "org.apache.commons" % "commons-compress" % "1.18",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "5.6.0.201912101111-r",
  "com.outr" %% "hasher" % "1.2.2",
  "org.webjars" %% "webjars-play" % "2.8.8-1",
  "org.webjars.bower" % "bootstrap" % "3.3.4",
  "org.webjars.bower" % "select2" % "3.5.2",
  "org.webjars.bower" % "highlightjs" % "9.10.0",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1",
  specs2 % Test
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

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

Global / onLoad := (Global / onLoad).value.andThen { state =>
  if (sys.props("java.specification.version") != "1.8") {
    sys.error("Java 8 is required for this project.")
    state.exit(ok = false)
  }
  else {
    state
  }
}

pipelineStages := Seq(gzip, digest)

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty