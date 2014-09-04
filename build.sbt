name := "webjars"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"

libraryDependencies ++= Seq(
  ws,
  cache,
  filters,
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "jquery" % "1.11.1",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "highlightjs" % "8.0-3",
  "com.bionicspirit" %% "shade" % "1.6.0"
)

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

lazy val root = (project in file(".")).enablePlugins(PlayScala)
