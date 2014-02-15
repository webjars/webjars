name := "webjars"

version := "1.0-SNAPSHOT"

//resolvers += "Sonatype OSS Snapshots Repository" at "http://oss.sonatype.org/content/groups/public",

resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"

libraryDependencies ++= Seq(
  cache,
  "org.webjars" %% "webjars-play" % "2.2.1",
  "org.webjars" % "jquery" % "1.11.0",
  "org.webjars" % "bootstrap" % "3.1.1",
  "org.webjars" % "highlightjs" % "8.0-1",
  "com.github.mumoshu" %% "play2-memcached" % "0.3.0.2"
)

play.Project.playScalaSettings
