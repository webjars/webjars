package utils


import org.apache.pekko.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import play.api.test._

import java.io.{BufferedInputStream, ByteArrayInputStream}
import java.net.URI
import scala.concurrent.duration._

class BowerGitHubSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bowerGitHub: BowerGitHub = application.injector.instanceOf[BowerGitHub]

  "groupId" should {
    "contain the org when given a Bower package name" in {
      await(bowerGitHub.groupId("jQuery", "3.0.0")) must beEqualTo ("org.webjars.bowergithub.jquery")
    }
    "be lowercase" in {
      await(bowerGitHub.groupId("https://github.com/PolymerElements/iron-elements", "1.0.10")) must beEqualTo ("org.webjars.bowergithub.polymerelements")
    }
    "work with forks" in {
      await(bowerGitHub.groupId("https://github.com/jamesward/iron-elements", "1.0.10")) must beEqualTo ("org.webjars.bowergithub.jamesward")
    }
    "work with moved repos when given a name" in {
      await(bowerGitHub.groupId("webcomponentsjs", "1.1.0")) must beEqualTo ("org.webjars.bowergithub.webcomponents")
    }
    "work with moved repos when given a url" in {
      await(bowerGitHub.groupId("https://github.com/polymer/webcomponentsjs", "1.1.0")) must beEqualTo ("org.webjars.bowergithub.webcomponents")
      await(bowerGitHub.groupId("https://github.com/polymer/webcomponentsjs.git", "1.1.0")) must beEqualTo ("org.webjars.bowergithub.webcomponents")
    }
  }

  "artifactId" should {
    "be the repo name" in {
      val url = "https://github.com/PolymerElements/iron-elements"
      await(bowerGitHub.artifactId(url, "1.0.10")) must beEqualTo ("iron-elements")
    }
    "be lowercase" in {
      await(bowerGitHub.artifactId("jQuery", "3.0.0")) must beEqualTo ("jquery")
    }
    "not contain a .git" in {
      val url = "https://github.com/PolymerElements/iron-elements.git"
      await(bowerGitHub.artifactId(url, "1.0.10")) must beEqualTo ("iron-elements")
    }
    "work with forks" in {
      val url = "https://github.com/jamesward/test-iron-elements"
      await(bowerGitHub.artifactId(url, "1.0.10")) must beEqualTo ("test-iron-elements")
    }
    "work with moved repos when given a name" in {
      await(bowerGitHub.artifactId("webcomponentsjs", "1.1.0")) must beEqualTo ("webcomponentsjs")
    }
    "work with moved repos when given a url" in {
      await(bowerGitHub.artifactId("https://github.com/jamesward/iron-elements", "1.0.10")) must beEqualTo ("test-iron-elements")
      await(bowerGitHub.artifactId("https://github.com/jamesward/iron-elements.git", "1.0.10")) must beEqualTo ("test-iron-elements")
    }
  }

  "name" should {
    "be the same as the bower.json when using a url" in {
      val info = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements", "1.0.10"))
      info.name must beEqualTo ("iron-elements")
    }
    "be the same as the bower index when using a bower name" in {
      val info = await(bowerGitHub.info("jQuery", "3.5.1"))
      info.name must beEqualTo ("jQuery")
    }
  }

  "bowerToMaven" should {
    "work with a plain name and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jQuery" -> "1.12.4"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.12.4")
    }
    "work with a plain name and semver range" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jQuery" -> "^1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[1.0.0,2.0.0-0)")
    }
    "work with a plain name and semver range and a v prefix" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jQuery" -> "^v1.0.0"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[1.0.0,2.0.0-0)")
    }
    "work with a github short reference" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery/jquery" -> "1.0.1"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.1")
    }
    "work with a github short reference" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "jquery/jquery"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[0,)")
    }
    "work with a github short reference and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "jquery/jquery#1.0.1"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.1")
    }
    "work with a github short reference and version with a v" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "jquery/jquery#v1.0.1"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.1")
    }
    "work with a github url" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "https://github.com/jquery/jquery"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("[0,)")
    }
    "work with a github url and version" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "https://github.com/jquery/jquery#1.0.1"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.1")
    }
    "work with a github url and version with a v" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("jquery" -> "https://github.com/jquery/jquery#v1.0.1"))
      group must beEqualTo ("org.webjars.bowergithub.jquery")
      artifact must beEqualTo ("jquery")
      version must beEqualTo ("1.0.1")
    }
    "work with semver ranges and a v prefix" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("shadycss" -> "webcomponents/shadycss#^v1.1.0"))
      group must beEqualTo ("org.webjars.bowergithub.webcomponents")
      artifact must beEqualTo ("shadycss")
      version must beEqualTo ("[1.1.0,2.0.0-0)")
    }
    "work with moved repos given a name" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("webcomponentsjs" -> "1.0.22"))
      group must beEqualTo ("org.webjars.bowergithub.webcomponents")
      artifact must beEqualTo ("webcomponentsjs")
      version must beEqualTo ("1.0.22")
    }
    "work with moved repos given a repo" in {
      val (group, artifact, version) = await(bowerGitHub.bowerToMaven("webcomponentsjs" -> "polymer/webcomponentsjs#1.0.22"))
      group must beEqualTo ("org.webjars.bowergithub.webcomponents")
      artifact must beEqualTo ("webcomponentsjs")
      version must beEqualTo ("1.0.22")
    }
  }

  "dependencies" should {
    "be converted from bower package names" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("jQuery" -> "3.2.1")))
      dependencies.head must beEqualTo (("org.webjars.bowergithub.jquery", "jquery", "3.2.1"))
    }
    "be converted from github short syntax package names" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("iron-validator-behavior" -> "PolymerElements/iron-validator-behavior#^1.0.0")))
      dependencies.head must beEqualTo (("org.webjars.bowergithub.polymerelements", "iron-validator-behavior", "[1.0.0,2.0.0-0)"))
    }
    "not have a prepended v in the version" in {
      val dependencies = await(bowerGitHub.mavenDependencies(Map("jQuery" -> "v3.2.1")))
      dependencies.head must beEqualTo (("org.webjars.bowergithub.jquery", "jquery", "3.2.1"))
    }
  }

  "version" should {
    "not contain a prepended v" in {
      val bowerInfo = await(bowerGitHub.info("jQuery", "v3.2.1"))
      bowerInfo.version must beEqualTo ("3.2.1")
    }
    "should always be a git tag with any prepended v removed" in {
      val gitHubInfo = await(bowerGitHub.info("https://github.com/PolymerElements/iron-elements.git", "v1.0.10"))
      gitHubInfo.version must beEqualTo ("1.0.10")
    }
  }

  "contents" should {
    "be in the META-INF/resources/webjars/[BOWER NAME]/[CONTENTS] form" in {
      val name = "jquery"

      val archive = await(bowerGitHub.archive(name, "3.2.1"))

      val excludes = await(bowerGitHub.excludes(name, "3.2.1"))

      val maybeBaseDirGlob = await(bowerGitHub.maybeBaseDirGlob(name))

      val webJar = WebJarCreator.createWebJar(archive, maybeBaseDirGlob, excludes, "", "jquery", Set.empty, "org.webjars.bowergithub.jquery", "jquery", "3.2.1", "jQuery/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val names = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      names must contain ("META-INF/resources/webjars/jQuery/dist/jquery.js")
    }
    "must exclude ignored files" in {
      val name = "vaadin-grid"

      val archive = await(bowerGitHub.archive(name, "4.0.0-alpha5"))
      val excludes = await(bowerGitHub.excludes(name, "4.0.0-alpha5"))

      val maybeBaseDirGlob = await(bowerGitHub.maybeBaseDirGlob(name))

      val webJar = WebJarCreator.createWebJar(archive, maybeBaseDirGlob, excludes, "", "vaadin-grid", Set.empty, "org.webjars.bowergithub.vaadin", "vaadin-grid", "4.0.0-alpha5", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val names = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      names must contain ("META-INF/resources/webjars/vaadin-grid/index.html")
      names must not contain "META-INF/resources/webjars/vaadin-grid/.npmignore"
    }
  }

  "archive" should {
    "work with a v in front of the version" in {
      val inputStream = await(bowerGitHub.archive("vaadin-grid", "v4.0.0-alpha5"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("bower.json")
    }
    "work when using a bower name" in {
      val inputStream = await(bowerGitHub.archive("vaadin-grid", "4.0.0-alpha5"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("bower.json")
    }
    "work when using a github short url" in {
      val inputStream = await(bowerGitHub.archive("vaadin/vaadin-grid", "4.0.0-alpha5"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("bower.json")
    }
  }

  "excludes" should {
    "contain contents of the ignore section from bower.json" in {
      val excludes = await(bowerGitHub.excludes("vaadin-grid", "4.0.0-alpha5"))
      excludes must contain ("**" + "/.*")
    }
  }

  "releaseVersion" should {
    val packageInfo = PackageInfo("foo", "3.2.1", None, new URI("foo://bar"), None, Seq.empty, Map.empty, Map.empty, None)

    "strip the v prefix" in {
      bowerGitHub.releaseVersion(Some("v1.2.3"), packageInfo) must beEqualTo ("1.2.3")
    }
    "leave the version alone if there is no v prefix" in {
      bowerGitHub.releaseVersion(Some("1.2.3"), packageInfo) must beEqualTo ("1.2.3")
    }
    "fallback to the packageInfo when no version is specified" in {
      bowerGitHub.releaseVersion(None, packageInfo) must beEqualTo ("3.2.1")
    }
    "use the specified version instead of the package verison" in {
      val packageInfo = await(bowerGitHub.info("polymerelements/paper-ripple", "v1.0.10"))
      bowerGitHub.releaseVersion(None, packageInfo) must beEqualTo ("1.0.10")
    }
  }

  "polymer/polymer 2.5.0" should {
    "have dependencies" in {
      val info = await(bowerGitHub.info("polymer/polymer", "2.5.0"))
      info.dependencies.head._1 must beEqualTo ("shadycss")
    }
  }

}
