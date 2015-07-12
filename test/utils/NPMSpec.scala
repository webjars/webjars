package utils

import java.io.BufferedInputStream

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class NPMSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  val ws = StandaloneWS.apply()
  val npm = NPM(ExecutionContext.global, ws.client)

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

  step(ws.close())

}
