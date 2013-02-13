import sbt._
import Keys._
import play.Project

object ApplicationBuild extends Build {

    val appName         = "webjars"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.webjars" % "bootstrap" % "2.1.1",
      "org.webjars" % "highlightjs" % "7.3",
      "org.webjars" % "webjars-play" % "2.1.0",
      "com.github.mumoshu" %% "play2-memcached" % "0.3.0"
    )

    val main = Project(appName, appVersion, appDependencies).settings(
      resolvers += "Sonatype OSS Snapshots Repository" at "http://oss.sonatype.org/content/groups/public",
      resolvers += "Spy Repository" at "http://files.couchbase.com/maven2"
    )

}
