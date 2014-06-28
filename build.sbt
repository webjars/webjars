name := "webjars"

version := "1.0-SNAPSHOT"

resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"

libraryDependencies ++= Seq(
  ws,
  cache,
  filters,
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "jquery" % "1.11.0",
  "org.webjars" % "bootstrap" % "3.1.1",
  "org.webjars" % "highlightjs" % "8.0-1",
  "com.github.mumoshu" %% "play2-memcached" % "0.6.0"
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

onLoad in Global := (onLoad in Global).value.andThen { state =>
  if (sys.props("java.specification.version") != "1.8") {
    sys.error("Java 8 is required for this project.")
    state.exit(false)
  }
  else {
    state
  }
}

lazy val root = (project in file(".")).enablePlugins(PlayScala)
