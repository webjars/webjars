package utils


import java.io.ByteArrayInputStream
import java.net.URL

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

import scala.concurrent.duration._

class BowerGitHubSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bowerGitHub: BowerGitHub = application.injector.instanceOf[BowerGitHub]

  "groupId" should {
    "contain the org when given a Bower package name" in {
      await(bowerGitHub.groupId("jQuery")) must beEqualTo ("org.webjars.bowergithub.jquery")
    }
    "be lowercase" in {
      await(bowerGitHub.groupId("https://github.com/PolymerElements/iron-elements")) must beEqualTo ("org.webjars.bowergithub.polymerelements")
    }
    "work with forks" in {
      await(bowerGitHub.groupId("https://github.com/jamesward/iron-elements")) must beEqualTo ("org.webjars.bowergithub.jamesward")
    }
  }

  "artifactId" should {
    "be the repo name" in {
      val url = "https://github.com/PolymerElements/iron-elements"
      await(bowerGitHub.artifactId(url)) must beEqualTo ("iron-elements")
    }
    "be lowercase" in {
      await(bowerGitHub.artifactId("jQuery")) must beEqualTo ("jquery")
    }
    "not contain a .git" in {
      val url = "https://github.com/PolymerElements/iron-elements.git"
      await(bowerGitHub.artifactId(url)) must beEqualTo ("iron-elements")
    }
    "work with forks" in {
      val url = "https://github.com/jamesward/test-iron-elements"
      await(bowerGitHub.artifactId(url)) must beEqualTo ("test-iron-elements")
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

  "bowerToMaven" should {
    "work with a plain name and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jQuery" -> "1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.0")
    }
    "work with a github short reference" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery/jquery" -> "1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.0")
    }
    "work with a github short reference" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "jquery/jquery"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[0,)")
    }
    "work with a github short reference and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "jquery/jquery#1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.0")
    }
    "work with a github url" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "https://github.com/jquery/jquery"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[0,)")
    }
    "work with a github url and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "https://github.com/jquery/jquery#1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.0")
    }
  }

  "dependencies" should {
    "be converted from bower package names" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("jQuery" -> "3.2.1")))
      dependencies.head must beEqualTo ("org.webjars.bowergithub.jquery", "jquery", "3.2.1")
    }
    "be converted from github short syntax package names" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("iron-validator-behavior" -> "PolymerElements/iron-validator-behavior#^1.0.0")))
      dependencies.head must beEqualTo ("org.webjars.bowergithub.polymerelements", "iron-validator-behavior", "[1.0.0,2)")
    }
    "not have a prepended v in the version" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("jQuery" -> "v3.2.1")))
      dependencies.head must beEqualTo ("org.webjars.bowergithub.jquery", "jquery", "3.2.1")
    }
  }

  "version" should {
    "not contain a prepended v" in {
      val bowerInfo = await(bowerGitHub.info("jQuery", Some("v3.2.1")))
      bowerInfo.version must beEqualTo ("3.2.1")
    }
    "should always be a git tag with any prepended v removed" in {
      val gitHubInfo = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements.git", Some("v1.0.10")))
      gitHubInfo.version must beEqualTo ("1.0.10")
    }
  }

  "contents" should {
    "be in the META-INF/resources/webjars/[BOWER NAME]/[CONTENTS] form" in {
      val url = new URL("https://bower-as-a-service.herokuapp.com/download/jQuery/v3.2.1")
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, bowerGitHub.contentsInSubdir, bowerGitHub.excludes, "", "org.webjars.bowergithub.jquery", "jquery", "3.2.1", "jQuery/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("META-INF/resources/webjars/jQuery/dist/jquery.js")
    }
  }
}
