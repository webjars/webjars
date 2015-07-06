name := "webjars"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"

libraryDependencies ++= Seq(
  ws,
  cache,
  filters,
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "commons-codec" % "commons-codec" % "1.10",
  "org.apache.commons" % "commons-compress" % "1.9",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "4.0.0.201506090130-r",
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "bootstrap" % "3.3.4",
  "org.webjars" % "select2" % "3.5.2",
  "org.webjars" % "highlightjs" % "8.0-3",
  "org.webjars.bower" % "jquery-typewatch" % "2.2.1" exclude("org.webjars.bower", "jquery"),
  "com.bionicspirit" %% "shade" % "1.6.0"
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
