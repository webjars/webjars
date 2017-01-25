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
      GitHub.gitHubUrl(new URL("https://www.github.com/isaacs/inherits.git")) must beASuccessfulTry (new URL("https://github.com/isaacs/inherits"))
      GitHub.gitHubUrl(new URL("https://github.com/zippyui/react-flex#readme")) must beASuccessfulTry (new URL("https://github.com/zippyui/react-flex"))
      GitHub.gitHubUrl("https://foo.com") must beAFailedTry
    }
  }

}
