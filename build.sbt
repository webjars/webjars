name := "webjars"

version := "1.0-SNAPSHOT"

//resolvers += "Sonatype OSS Snapshots Repository" at "http://oss.sonatype.org/content/groups/public",

resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"

libraryDependencies ++= Seq(
  cache,
  "org.webjars" %% "webjars-play" % "2.2.1",
  "org.webjars" % "bootstrap" % "2.1.1",
  "org.webjars" % "highlightjs" % "7.3",
  "com.github.mumoshu" %% "play2-memcached" % "0.3.0.2"
)

play.Project.playScalaSettings
