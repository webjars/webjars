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
  "converting npm deps to maven" should {
    "work with standard npm deps" in {
      val npmDeps = Map(
        "traceur" -> "^0.0.72"
      )
      val mavenDeps = await(npm.convertNpmDependenciesToMaven(npmDeps))
      mavenDeps.get("traceur") must beSome ("[0.0.72,0.0.73)")
    }
    "work with versionless git npm deps" in {
      val npmDeps = Map(
        "route-recognizer" -> "git://github.com/btford/route-recognizer"
      )
      val mavenDeps = await(npm.convertNpmDependenciesToMaven(npmDeps))
      val latestVersion = await(npm.versions("git://github.com/btford/route-recognizer")).headOption
      mavenDeps.get("github-com-btford-route-recognizer") must beEqualTo (latestVersion)
    }
    "work with versioned git npm deps" in {
      val npmDeps = Map(
        "route-recognizer" -> "git://github.com/btford/route-recognizer#0.1.1"
      )
      val mavenDeps = await(npm.convertNpmDependenciesToMaven(npmDeps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
    }
    "work with github npm deps" in {
      val npmDeps = Map(
        "route-recognizer" -> "btford/route-recognizer#0.1.1"
      )
      val mavenDeps = await(npm.convertNpmDependenciesToMaven(npmDeps))
      mavenDeps.get("github-com-btford-route-recognizer") must beSome ("0.1.1")
    }
  }
  "artifactId" should {
    "convert a name to a name" in {
      await(npm.artifactId("foo")) must beEqualTo ("foo")
    }
    "convert a github url to a name" in {
      await(npm.artifactId("mochajs/mocha")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a git:// url to a name" in {
      await(npm.artifactId("git://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a https:// url to a name" in {
      await(npm.artifactId("https://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
  }

  step(ws.close())

}
