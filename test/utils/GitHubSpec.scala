package utils

import java.net.URL

import play.api.libs.json.Json
import play.api.test._

import scala.util.Try


class GitHubSpec extends PlaySpecification with GlobalApplication {

  lazy val gitHub: GitHub = application.injector.instanceOf[GitHub]

  "gitHubUrl" should {
    "work normally" in {
      GitHub.gitHubUrl("git://github.com/isaacs/inherits").map(_.toString) must beASuccessfulTry ("https://github.com/isaacs/inherits")
      GitHub.gitHubUrl("git://github.com/isaacs/inherits").map(_.toString) must beASuccessfulTry ("https://github.com/isaacs/inherits")
      GitHub.gitHubUrl("https://github.com/isaacs/inherits").map(_.toString) must beASuccessfulTry ("https://github.com/isaacs/inherits")
      GitHub.gitHubUrl(new URL("https://github.com/isaacs/inherits.git")).map(_.toString) must beASuccessfulTry ("https://github.com/isaacs/inherits")
      GitHub.gitHubUrl(new URL("https://www.github.com/isaacs/inherits.git")).map(_.toString) must beASuccessfulTry ("https://www.github.com/isaacs/inherits")
      GitHub.gitHubUrl(new URL("https://github.com/zippyui/react-flex#readme")).map(_.toString) must beASuccessfulTry ("https://github.com/zippyui/react-flex")
      GitHub.gitHubUrl("https://foo.com") must beAFailedTry
    }
  }

  "currentUrls" should {
    "work for good urls" in {
      val url = new URL("https://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage.toString must beEqualTo (url.toString)
    }
    "work for http to https redirects" in {
      val url = new URL("http://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage.toString must beEqualTo ("https://github.com/isaacs/inherits")
    }
    "fail for not founds" in {
      val url = new URL("http://github.com/asdf1234/zxcv4321")
      await(gitHub.currentUrls(url)) must throwA[ServerError]
    }
  }

  "raw" should {
    "work for valid files" in {
      val url = GitHub.gitHubUrl("https://github.com/PolymerElements/iron-behaviors.git").get
      val content = await(gitHub.raw(url, "v2.0.0", "bower.json"))
      Try(Json.parse(content)) must beASuccessfulTry
    }
  }

}
