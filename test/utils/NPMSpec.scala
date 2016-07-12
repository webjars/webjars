package utils

import java.io.BufferedInputStream

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test._

import scala.concurrent.duration._

class NPMSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val application = new GuiceApplicationBuilder().build

  lazy val npm = application.injector.instanceOf[NPM]

  "chokidar 1.0.1" should {
    "have a license" in {
      await(npm.info("chokidar", Some("1.0.1"))).licenses must contain ("MIT")
    }
  }
  "is-dotfile" should {
    "have a license" in {
      await(npm.info("is-dotfile", Some("1.0.0"))).licenses must contain ("MIT")
    }
  }
  "inflight 1.0.4" should {
    "have the correct github url" in {
      await(npm.info("inflight", Some("1.0.4"))).gitHubHome must beASuccessfulTry("https://github.com/npm/inflight")
    }
  }
  "inherits 2.0.1" should {
    "have a homepage" in {
      await(npm.info("inherits", Some("2.0.1"))).homepage must beEqualTo ("https://github.com/isaacs/inherits")
    }
  }
  "simple-fmt" should {
    "have an issue tracking url" in {
      await(npm.info("simple-fmt", Some("0.1.0"))).issuesUrl must beEqualTo ("https://github.com/olov/simple-fmt/issues")
    }
  }
  "weinre 2.0.0-pre-I0Z7U9OV" should {
    "have a correct vcs url" in {
      val info = await(npm.info("weinre", Some("2.0.0-pre-I0Z7U9OV")))
      info.gitHubHome must beAFailedTry
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
      info.sourceConnectionUrl must beEqualTo ("https://github.com/mochajs/mocha")
      info.sourceUrl must beEqualTo ("https://github.com/mochajs/mocha")
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
      info.homepage must beEqualTo ("https://github.com/btford/route-recognizer")
      info.sourceConnectionUrl must beEqualTo ("https://github.com/btford/route-recognizer")
      info.sourceUrl must beEqualTo ("https://github.com/btford/route-recognizer")
      info.issuesUrl must beEqualTo ("https://github.com/btford/route-recognizer/issues")
      info.gitHubHome must beASuccessfulTry("https://github.com/btford/route-recognizer")
    }
  }
  "git fork - git url" should {
    "have the correct urls" in {
      val info = await(npm.info("git://github.com/btford/route-recognizer"))
      info.name must beEqualTo ("route-recognizer")
      info.version mustNotEqual ""
      info.homepage must beEqualTo ("https://github.com/btford/route-recognizer")
      info.sourceConnectionUrl must beEqualTo ("git://github.com/btford/route-recognizer")
      info.sourceUrl must beEqualTo ("https://github.com/btford/route-recognizer")
      info.issuesUrl must beEqualTo ("https://github.com/btford/route-recognizer/issues")
      info.gitHubHome must beASuccessfulTry("https://github.com/btford/route-recognizer")
    }
  }
  "info on amp-ui 3.2.0" should {
    "fail with a nice error" in {
      await(npm.info("amp-ui", Some("3.2.0"))) must throwA[Exception]("The source repository for amp-ui 3.2.0 could not be determined but is required to published to Maven Central.  This will need to be fixed in the project's package metadata.")
    }
  }
  "versions on redux" should {
    "return versions" in {
      await(npm.versions("redux")) must containAllOf(Seq("3.0.4", "0.0.1"))
    }
  }

  "licenses" should {
    "be able to be fetched from git repos" in {
      await(npm.info("ms", Some("0.7.1"))).licenses must beEqualTo(Seq("MIT"))
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
    }
  }

  "typescript versions" should {
    "work" in {
      await(npm.versions("typescript")) must contain("1.7.5")
    }
  }

}
