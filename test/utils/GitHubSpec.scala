package utils

import java.net.URL

import play.api.test._


class GitHubSpec extends PlaySpecification with GlobalApplication {

  lazy val gitHub: GitHub = application.injector.instanceOf[GitHub]

  "gitHubUrl" should {
    "work normally" in {
      GitHub.gitHubUrl("git://github.com/isaacs/inherits") must beASuccessfulTry (new URL("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl("git://github.com/isaacs/inherits") must beASuccessfulTry (new URL("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl("https://github.com/isaacs/inherits") must beASuccessfulTry (new URL("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl(new URL("https://github.com/isaacs/inherits.git")) must beASuccessfulTry (new URL("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl(new URL("https://www.github.com/isaacs/inherits.git")) must beASuccessfulTry (new URL("https://www.github.com/isaacs/inherits"))
      GitHub.gitHubUrl(new URL("https://github.com/zippyui/react-flex#readme")) must beASuccessfulTry (new URL("https://github.com/zippyui/react-flex"))
      GitHub.gitHubUrl("https://foo.com") must beAFailedTry
    }
  }

  "currentUrls" should {
    "work for good urls" in {
      val url = new URL("https://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage must beEqualTo (url)
    }
    "work for http to https redirects" in {
      val url = new URL("http://github.com/isaacs/inherits")
      val (homepage, _, _) = await(gitHub.currentUrls(url))
      homepage must beEqualTo (new URL("https://github.com/isaacs/inherits"))
    }
    "fail for not founds" in {
      val url = new URL("http://github.com/asdf1234/zxcv4321")
      await(gitHub.currentUrls(url)) must throwA[ServerError]
    }
  }

}
