package utils

import io.lemonlabs.uri.AbsoluteUrl
import play.api.libs.json.Json
import play.api.test._

import scala.util.Try


class GitHubSpec extends PlaySpecification with GlobalApplication {

  lazy val gitHub: GitHub = application.injector.instanceOf[GitHub]

  "gitHubUrl" should {
    "work normally" in {
      GitHub.gitHubUrl("git://github.com/isaacs/inherits") must beASuccessfulTry (AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl("git://github.com/isaacs/inherits") must beASuccessfulTry (AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl("https://github.com/isaacs/inherits") must beASuccessfulTry (AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl(AbsoluteUrl.parse("https://github.com/isaacs/inherits.git")) must beASuccessfulTry (AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl(AbsoluteUrl.parse("https://www.github.com/isaacs/inherits.git")) must beASuccessfulTry (AbsoluteUrl.parse("https://www.github.com/isaacs/inherits"))
      GitHub.gitHubUrl(AbsoluteUrl.parse("https://github.com/zippyui/react-flex#readme")) must beASuccessfulTry (AbsoluteUrl.parse("https://github.com/zippyui/react-flex"))
      GitHub.gitHubUrl("https://foo.com") must beAFailedTry
    }
  }

  "currentUrls" should {
    "work for good urls" in {
      val url = AbsoluteUrl.parse("https://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage must beEqualTo (url)
    }
    "work for http to https redirects" in {
      val url = AbsoluteUrl.parse("http://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage must beEqualTo (AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
    }
    "fail for not founds" in {
      val url = AbsoluteUrl.parse("http://github.com/asdf1234/zxcv4321")
      await(gitHub.currentUrls(url)) must throwA[ServerError]
    }
    "work for sub paths" in {
      val url = AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components/tree/gip-recia-esco-content-menu-lit/@uportal/esco-content-menu-lit")
      val (homepage, gitUrl, issuesUrl) = await(gitHub.currentUrls(url))
      homepage must beEqualTo (AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components"))
      gitUrl must beEqualTo (AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components.git"))
      issuesUrl must beEqualTo (AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components/issues"))
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
