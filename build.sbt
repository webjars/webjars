name := "webjars"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.11"

incOptions := incOptions.value.withNameHashing(true)

updateOptions := updateOptions.value.withCachedResolution(true)

TwirlKeys.constructorAnnotations += "@javax.inject.Inject()"

libraryDependencies ++= Seq(
  ws,
  cache,
  filters,
  "commons-codec" % "commons-codec" % "1.10",
  "org.apache.commons" % "commons-compress" % "1.9",
  "org.apache.commons" % "commons-io" % "1.3.2",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.4.0.201606070830-r",
  "org.webjars" %% "webjars-play" % "2.5.0-4",
  "org.webjars" % "jquery" % "3.2.0",
  "org.webjars" % "bootstrap" % "3.3.4",
  "org.webjars" % "select2" % "3.5.2",
  "org.webjars" % "highlightjs" % "8.0-3",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1" exclude("org.webjars.bower", "jquery"),
  "com.bionicspirit" %% "shade" % "1.7.4",
  specs2 % Test
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

onLoad in Global := (onLoad in Global).value.andThen { state =>
  if (sys.props("java.specification.version") != "1.8") {
    sys.error("Java 8 is required for this project.")
    state.exit(ok = false)
  }
  else {
    state
  }
}

lazy val root = (project in file(".")).enablePlugins(PlayScala)

pipelineStages := Seq(gzip, digest)
