package utils


import akka.util.Timeout

import play.api.test._

import scala.concurrent.duration._

class BowerGitHubSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bowerGitHub: Bower = application.injector.instanceOf[BowerGitHub]


  "groupId" should {
    "contain the org when given a Bower package name" in {
      val justName = await(bowerGitHub.info("jQuery"))
      bowerGitHub.groupId(justName) must beSome ("org.webjars.bowergithub.jquery")
    }
    "be lowercase" in {
      val gitHubWithUpperCase = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements"))
      bowerGitHub.groupId(gitHubWithUpperCase) must beSome ("org.webjars.bowergithub.polymerelements")
    }
  }

  "artifactId" should {
    "be the repo name" in {
      val gitHubWithUpperCase = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements"))
      bowerGitHub.artifactId(gitHubWithUpperCase) must beSome ("iron-elements")
    }
    "be lowercase" in {
      val justName = await(bowerGitHub.info("jQuery"))
      bowerGitHub.artifactId(justName) must beSome ("jquery")
    }
    "not contain a .git" in {
      val gitHubWithUpperCase = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements.git"))
      bowerGitHub.artifactId(gitHubWithUpperCase) must beSome ("iron-elements")
    }
  }

  "name" should {
    "be the package name" in {
      val gitHubWithUpperCase = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements"))
      gitHubWithUpperCase.name must beEqualTo ("iron-elements")
    }
    "be the same case as the bower.json" in {
      val justName = await(bowerGitHub.info("jQuery"))
      justName.name must beEqualTo ("jQuery")
    }
  }
}
