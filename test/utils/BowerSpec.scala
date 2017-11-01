package utils

import java.io.BufferedInputStream
import java.net.{URI, URL}

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

import scala.concurrent.duration._

class BowerSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bower: Bower = application.injector.instanceOf[Bower]

  "jquery info" should {
    "work with a correct version" in {
      await(bower.info("jquery", Some("1.11.1"))).name must equalTo("jquery")
    }
    "fail with an invalid version" in {
      await(bower.info("jquery", Some("0.0.0"))) must throwA[Exception]
    }
  }
  "bootstrap" should {
    val info = await(bower.info("bootstrap", Some("3.3.2")))
    "have a dependency on jquery" in {
      info.dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }
  "dc.js" should {
    "have the corrected GitHub url" in {
      await(bower.info("dc.js", Some("1.7.3"))).maybeHomepageUrl must beSome(new URL("https://github.com/dc-js/dc.js"))
    }
  }
  "sjcl" should {
    "download" in {
      val is = new BufferedInputStream(await(bower.archive("sjcl", "1.0.2")))
      val zis = new ArchiveStreamFactory().createArchiveInputStream(is)
      val files = Stream.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must contain ("sjcl.js")
    }
  }

  "valid git short url" should {
    "have versions" in {
      val versions = await(bower.versions("PolymerElements/iron-elements"))
      versions.length must beGreaterThan (0)
      versions.contains("v1.0.0") must beTrue
    }
  }
  "invalid git url" should {
    "fail" in {
      await(bower.versions("foo/bar")) must throwA[Exception]
    }
  }
  "git repo master info" should {
    "work" in {
      val info = await(bower.info("PolymerElements/iron-elements"))
      info.name must beEqualTo ("iron-elements")
      info.version mustNotEqual ""
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/PolymerElements/iron-elements.git"))
      info.maybeHomepageUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements"))
    }
  }

  "git repo without 'version' info in bower.json" should {
    "have version without git 'v' prefix" in {
      val info = await(bower.info("https://github.com/vaadin/vaadin-grid"))
      info.name must beEqualTo ("vaadin-grid")
      info.version must not startingWith "v"
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/vaadin/vaadin-grid.git"))
      info.maybeHomepageUrl must beSome (new URL("https://github.com/vaadin/vaadin-grid"))
    }
  }

  "git repo tagged version zip" should {
    "work" in {
      val zip = await(bower.archive("PolymerElements/iron-elements", "v1.0.0"))
      val bufferedInputStream = new BufferedInputStream(zip)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)

      bufferedInputStream.available() must beEqualTo (10240)
    }
  }
  "git fork - github short url" should {
    "have the correct urls" in {
      val info = await(bower.info("PolymerElements/iron-elements"))
      info.name must beEqualTo ("iron-elements")
      info.version mustNotEqual ""
      info.maybeHomepageUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements"))
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/PolymerElements/iron-elements.git"))
      info.maybeIssuesUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements/issues"))
      info.maybeGitHubUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements"))
    }
  }
  "git fork - git url" should {
    "have the correct urls" in {
      val info = await(bower.info("git://github.com/PolymerElements/iron-elements"))
      info.name must beEqualTo ("iron-elements")
      info.version mustNotEqual ""
      info.maybeHomepageUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements"))
      info.sourceConnectionUri must beEqualTo (new URI("https://github.com/PolymerElements/iron-elements.git"))
      info.maybeIssuesUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements/issues"))
      info.maybeGitHubUrl must beSome (new URL("https://github.com/PolymerElements/iron-elements"))
    }
  }

  "git commits" should {
    "work as version" in {
      val info = await(bower.info("git://github.com/dc-js/dc.js", Some("6e95388b9a")))
      info.version must beEqualTo ("6e95388b9a")
    }
  }

  "homepage" should {
    "be have a default" in {
      await(bower.info("git://github.com/millermedeiros/requirejs-plugins")).maybeHomepageUrl must beSome (new URL("https://github.com/millermedeiros/requirejs-plugins"))
    }
  }

  "git repo with branch" should {
    "fetch the versions" in {
      await(bower.versionsOnBranch("git://github.com/mdedetrich/requirejs-plugins", "jsonSecurityVulnerability")) must contain ("d9c103e7a0")
    }
  }

  "long file names" should {
    "work" in {
      val is = new BufferedInputStream(await(bower.archive("highcharts/highcharts", "v4.2.5")))
      val zis = new ArchiveStreamFactory().createArchiveInputStream(is)
      val files = Stream.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must contain ("tools/ant-contrib-0.6-bin/docs/api/net/sf/antcontrib/perf/AntPerformanceListener.StopWatchComparator.html")
    }
  }

  "bootstrap" should {
    "have a dependency on jquery" in {
      val packageInfo = await(bower.info("bootstrap", Some("3.3.2")))
      packageInfo.dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }

}
