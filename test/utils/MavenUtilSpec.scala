package utils

import akka.util.Timeout
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class MavenUtilSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  val ws = StandaloneWS.apply()
  val git = GitUtil(ExecutionContext.global, ws.client)
  val maven = MavenUtil(ExecutionContext.global, git)

  "converting npm deps to maven" should {
    "work with standard npm deps" in {
      val deps = Map(
        "traceur" -> "^0.0.72"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("traceur") must beSome ("[0.0.72,0.0.73)")
    }
    "work with versionless git npm deps" in {
      val deps = Map(
        "route-recognizer" -> "git://github.com/btford/route-recognizer",
        "HTML5-Desktop-Notifications" -> "https://github.com/ttsvetko/HTML5-Desktop-Notifications.git"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
      mavenDeps.get("github-com-ttsvetko-HTML5-Desktop-Notifications") must beSome ("0.0.0-1ad38f3041")
    }
    "work with versioned git npm deps" in {
      val deps = Map(
        "route-recognizer" -> "git://github.com/btford/route-recognizer#0.1.1",
        "react-tools" -> "git://github.com/facebook/react.git#b4e74e38e43ac53af8acd62c78c9213be0194245"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
      mavenDeps.get("github-com-facebook-react") must beSome ("b4e74e38e43ac53af8acd62c78c9213be0194245")
    }
    "work with github npm deps" in {
      val deps = Map(
        "route-recognizer" -> "btford/route-recognizer#0.1.1",
        "react-tools" -> "github:facebook/react#b4e74e3"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
      mavenDeps.get("github-com-facebook-react") must beSome ("b4e74e3")
    }
    "work with semver deps" in {
      val deps = Map(
        "iron-a11y-announcer" -> "PolymerElements/iron-a11y-announcer#^1.0.0"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-PolymerElements-iron-a11y-announcer") must beSome ("[1.0.0,2)")
    }
  }

  step(ws.close())

}
