package utils

import java.io.BufferedInputStream
import java.net.{URI, URL}

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.libs.json.JsResultException
import play.api.test._

import scala.concurrent.duration._

class NPMSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val npm: NPM = application.injector.instanceOf[NPM]

  "inflight 1.0.4" should {
    "have the correct github url" in {
      await(npm.info("inflight", Some("1.0.4"))).gitHubUrl must beSome(new URL("https://github.com/npm/inflight"))
    }
  }
  "inherits 2.0.1" should {
    "have a homepage" in {
      await(npm.info("inherits", Some("2.0.1"))).homepageUrl must beEqualTo (new URL("https://github.com/isaacs/inherits"))
    }
  }
  "simple-fmt" should {
    "have an issue tracking url" in {
      await(npm.info("simple-fmt", Some("0.1.0"))).issuesUrl must beEqualTo (new URL("https://github.com/olov/simple-fmt/issues"))
    }
  }
  "weinre 2.0.0-pre-I0Z7U9OV" should {
    "have a correct vcs url" in {
      val info = await(npm.info("weinre", Some("2.0.0-pre-I0Z7U9OV")))
      info.gitHubUrl must beNone
    }
  }
  "valid git url" should {
    "have versions" in {
      val versions = await(npm.versions("visionmedia/mocha"))
      versions.length must beGreaterThan (0)
      versions.contains("1.0.0") must beTrue
    }
  }
  "invalid git url" should {
    "fail" in {
      await(npm.versions("foo/bar")) must throwA[Exception]
    }
  }
  "git repo master info" should {
    "work" in {
      val info = await(npm.info("visionmedia/mocha"))
      info.name must beEqualTo ("mocha")
      info.version mustNotEqual ""
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/mochajs/mocha.git"))
      info.sourceUrl must beEqualTo (new URL("https://github.com/mochajs/mocha"))
    }
  }
  "git repo tagged version info" should {
    "work" in {
      val info = await(npm.info("mochajs/mocha", Some("2.2.5")))
      info.name must beEqualTo ("mocha")
      info.version must beEqualTo ("2.2.5")
    }
  }
  "git repo tagged version zip" should {
    "work" in {
      val tgz = await(npm.tgz("mochajs/mocha", "2.2.5"))
      val bufferedInputStream = new BufferedInputStream(tgz)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      bufferedInputStream.available() must beEqualTo (645120)
    }
  }
  "git fork - github short url" should {
    "have the correct urls" in {
      val info = await(npm.info("btford/route-recognizer"))
      info.name must beEqualTo ("route-recognizer")
      info.version mustNotEqual ""
      info.homepageUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer"))
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/btford/route-recognizer.git"))
      info.sourceUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer"))
      info.issuesUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer/issues"))
      info.gitHubUrl must beSome (new URL("https://github.com/btford/route-recognizer"))
    }
  }
  "git fork - git url" should {
    "have the correct urls" in {
      val info = await(npm.info("git://github.com/btford/route-recognizer"))
      info.name must beEqualTo ("route-recognizer")
      info.version mustNotEqual ""
      info.homepageUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer"))
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/btford/route-recognizer.git"))
      info.sourceUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer"))
      info.issuesUrl must beEqualTo (new URL("https://github.com/btford/route-recognizer/issues"))
      info.gitHubUrl must beSome (new URL("https://github.com/btford/route-recognizer"))
    }
  }
  "info on amp-ui 3.2.0" should {
    "fail with a nice error" in {
      await(npm.info("amp-ui", Some("3.2.0"))) must throwA[JsResultException]
    }
  }
  "versions on redux" should {
    "return versions" in {
      await(npm.versions("redux")) must containAllOf(Seq("3.0.4", "0.0.1"))
    }
  }

  "scoped packages" should {
    "have info" in {
      await(npm.info("@reactivex/rxjs", Some("5.0.0-alpha.7"))).name must beEqualTo("@reactivex/rxjs")
    }
    "have versions" in {
      await(npm.versions("@reactivex/rxjs")) must contain("5.0.0-alpha.7")
    }
    "have a tgz" in {
      val tgz = await(npm.tgz("@reactivex/rxjs", "5.0.0-alpha.7"))
      val bufferedInputStream = new BufferedInputStream(tgz)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      bufferedInputStream.available() must beEqualTo (1687)
      archiveStream.getNextEntry.getName must beEqualTo ("package/package.json")
    }
  }

  "typescript versions" should {
    "work" in {
      await(npm.versions("typescript")) must contain("1.7.5")
    }
  }

  "npm info for react-flex" should {
    "have replaced urls due to the github repo being deleted" in {
      val info = await(npm.info("react-flex", Some("2.2.7")))
      info.homepageUrl must beEqualTo (new URL("https://www.npmjs.com/package/react-flex"))
      info.gitHubOrgRepo must beNone
    }
  }

  "npm info for git@github.com:yiminghe/async-validator.git" should {
    "have a uri format for the sourceConnectionUri" in {
      val info = await(npm.info("async-validator", Some("1.0.0")))
      info.sourceConnectionUri.getScheme must beEqualTo("https")
      info.sourceConnectionUri.getHost must beEqualTo("github.com")
      info.sourceConnectionUri.getPath must beEqualTo("/yiminghe/async-validator.git")
    }
  }

  "repositoryToUri" should {
    "work with regular urls" in {
      val ssh = NPM.repositoryToUri("ssh://host.xz/another/repo.git").get
      ssh.getScheme must beEqualTo("ssh")
      ssh.getHost must beEqualTo("host.xz")
      ssh.getPath must beEqualTo("/another/repo.git")

      val git = NPM.repositoryToUri("git://host.xz/another/repo.git").get
      git.getScheme must beEqualTo("git")
      git.getHost must beEqualTo("host.xz")
      git.getPath must beEqualTo("/another/repo.git")

      val https = NPM.repositoryToUri("https://host.xz/another/repo.git").get
      https.getScheme must beEqualTo("https")
      https.getHost must beEqualTo("host.xz")
      https.getPath must beEqualTo("/another/repo.git")
    }
    "work with git ssh short syntax" in {
      val plain = NPM.repositoryToUri("host.xz:/another/repo.git").get
      plain.getScheme must beEqualTo("ssh")
      plain.getHost must beEqualTo("host.xz")
      plain.getPath must beEqualTo("/another/repo.git")

      val another = NPM.repositoryToUri("host.xz:another/repo.git").get
      another.getScheme must beEqualTo("ssh")
      another.getAuthority must beEqualTo("host.xz")
      another.getHost must beEqualTo("host.xz")
      another.getPath must beEqualTo("/another/repo.git")

      val user = NPM.repositoryToUri("user@host.xz:/another/repo.git").get
      user.getScheme must beEqualTo("ssh")
      user.getHost must beEqualTo("host.xz")
      user.getPath must beEqualTo("/another/repo.git")
      user.getUserInfo must beEqualTo("user")

      val anotherUser = NPM.repositoryToUri("user@host.xz:another/repo.git").get
      anotherUser.getScheme must beEqualTo("ssh")
      anotherUser.getAuthority must beEqualTo("user@host.xz")
      anotherUser.getHost must beEqualTo("host.xz")
      anotherUser.getPath must beEqualTo("/another/repo.git")
      anotherUser.getUserInfo must beEqualTo("user")
    }
    "work with gist short syntax" in {
      val uri = NPM.repositoryToUri("gist:11081aaa281").get
      uri.getScheme must beEqualTo("https")
      uri.getHost must beEqualTo("gist.github.com")
      uri.getPath must beEqualTo("/11081aaa281.git")
    }
    "work with bitbucket short syntax" in {
      val uri = NPM.repositoryToUri("bitbucket:another/repo").get
      uri.getScheme must beEqualTo("https")
      uri.getHost must beEqualTo("bitbucket.org")
      uri.getPath must beEqualTo("/another/repo.git")
    }
    "work with gitlab short syntax" in {
      val uri = NPM.repositoryToUri("gitlab:another/repo").get
      uri.getScheme must beEqualTo("https")
      uri.getHost must beEqualTo("gitlab.com")
      uri.getPath must beEqualTo("/another/repo.git")
    }
    "work wuth github short syntax" in {
      val uri = NPM.repositoryToUri("another/repo").get
      uri.getScheme must beEqualTo("https")
      uri.getHost must beEqualTo("github.com")
      uri.getPath must beEqualTo("/another/repo.git")
    }
  }

  "npm info for @types/react" should {
    "work for the latest version" in {
      val info = await(npm.info("@types/react"))
      info.name must beEqualTo("@types/react")
    }
    "work for 15.0.3" in {
      val info = await(npm.info("@types/react", Some("15.0.3")))
      info.name must beEqualTo("@types/react")
      info.version must beEqualTo("15.0.3")
    }
  }

}
