package utils

import akka.util.Timeout
import play.api.test._

import scala.concurrent.duration._

class MavenSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val git = application.injector.instanceOf[Git]
  lazy val maven = application.injector.instanceOf[Maven]

  "converting npm deps to maven" should {
    "work with standard npm deps" in {
      val deps = Map(
        "traceur" -> "^0.0.72"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("traceur") must beSome ("[0.0.72,0.0.73-0)")
    }
    "work with versionless git npm deps" in {
      val deps = Map(
        "route-recognizer" -> "git://github.com/btford/route-recognizer",
        "HTML5-Desktop-Notifications" -> "https://github.com/ttsvetko/HTML5-Desktop-Notifications.git"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
      mavenDeps.get("github-com-ttsvetko-html5-desktop-notifications") must beSome ("3.0.0")
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
      mavenDeps.get("github-com-polymerelements-iron-a11y-announcer") must beSome ("[1.0.0,2.0.0-0)")
    }
    "work with scoped deps" in {
      val deps = Map(
        "@reactivex/rxjs" -> "5.0.0-alpha.7"
      )
      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("reactivex__rxjs") must beSome ("5.0.0-alpha.7")
    }
    "not work with invalid value" in {
      // from: https://github.com/QueraTeam/jquery-duration-picker/blob/master/bower.json#L26
      val deps = Map(
        "semantic" -> "semantic-ui#~2.1.4"
      )
      await(maven.convertNpmBowerDependenciesToMaven(deps)) must throwA[Exception]
    }

    "work with a tgz" in {
      val deps = Map(
        "wrench" -> "https://github.com/derekslife/wrench-js/tarball/156eaceed68ed31ffe2a3ecfbcb2be6ed1417fb2"
      )

      val mavenDeps = await(maven.convertNpmBowerDependenciesToMaven(deps))
      mavenDeps.get("github-com-derekslife-wrench-js") must beSome ("156eaceed68ed31ffe2a3ecfbcb2be6ed1417fb2")
    }
  }

}
