import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "webjars"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.webjars" % "bootstrap" % "2.1.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "webjars" at "http://webjars.github.com/m2"
    )

}
