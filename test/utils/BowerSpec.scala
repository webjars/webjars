package utils

import java.io.BufferedInputStream
import java.net.{URI, URL}

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.libs.concurrent.Futures
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BowerSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bower: Bower = application.injector.instanceOf[Bower]

  lazy implicit val ec: ExecutionContext = application.injector.instanceOf[ExecutionContext]
  lazy implicit val futures: Futures = application.injector.instanceOf[Futures]

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
      versions must not be empty
      versions must contain ("1.0.0")
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
  "git repo tagged version info" should {
    "work" in {
      val info = await(bower.info("PolymerElements/iron-elements", Some("v1.0.0")))
      info.name must beEqualTo ("iron-elements")
      info.version must beEqualTo ("1.0.0")
    }
  }
  "git repo tagged version zip" should {
    "work" in {
      val zip = await(bower.archive("PolymerElements/iron-elements", "v1.0.0"))
      val bufferedInputStream = new BufferedInputStream(zip)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)
      archiveStream.getNextEntry.getName must beEqualTo (".bower.json")
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

  "bootstrap" should {
    "have a dependency on jquery" in {
      val packageInfo = await(bower.info("bootstrap", Some("3.3.2")))
      packageInfo.dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }

  "lookup" should {
    "work with a name" in {
      val url = await(bower.lookup("jquery", "3.0.0"))
      url.toString must beEqualTo ("https://github.com/jquery/jquery-dist")
    }
    "fail with an invalid name" in {
      await(bower.lookup("asdfqwer1234", "1.2.3")) must throwA[Exception]
    }
    "work with a valid github git url" in {
      val url = await(bower.lookup("https://github.com/jquery/jquery-dist.git", "3.0.0"))
      url.toString must beEqualTo ("https://github.com/jquery/jquery-dist")
    }
    "fail with an invalid url" in {
      await(bower.lookup("https://asdf.com/", "1.2.3")) must throwA[Exception]
    }
  }

  "pathPrefix" should {
    "be in the form artifactid/releaseVersion" in {
      val packageInfo = await(bower.info("bootstrap", Some("3.3.2")))
      val pathPrefix = await(bower.pathPrefix("foobar", "1.2.3", packageInfo))
      pathPrefix must beEqualTo ("foobar/1.2.3/")
    }
  }

  "info" should {
    "work even when a package.json doesn't exist" in {
      await(bower.info("https://github.com/Polymer/polymer-analyzer.git", Some("v2.7.0"))).name must equalTo("polymer-analyzer")
    }
  }

  "depGraph" should {
    "work with bootstrap" in {
      val packageInfo = await(bower.info("bootstrap", Some("3.3.7")))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph must beEqualTo(Map("jquery" -> "3.3.1"))
    }
    "work with " in {
      val packageInfo = await(bower.info("ng-bootstrap-select", Some("0.5.0")))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph.keySet must beEqualTo(Set("angular", "bootstrap", "bootstrap-select", "jquery"))
    }
    "work with PolymerElements/iron-behaviors" in {
      val packageInfo = await(bower.info("PolymerElements/iron-behaviors", Some("v2.0.0")))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph.keySet must beEqualTo(Set("Polymer/polymer", "polymerelements/iron-a11y-keys-behavior", "webcomponents/shadycss", "webcomponents/webcomponentsjs"))
    }
  }

  "github.com/uwdata/vega-lite" should {
    "download" in {
      val is = new BufferedInputStream(await(bower.archive("uwdata/vega-lite", "v2.1.2")))
      val zis = new ArchiveStreamFactory().createArchiveInputStream(is)
      val files = Stream.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must not contain "examples/compiled/data"
    }
  }

  "dojox" should {
    "have the right licenses" in {
      val packageInfo = await(bower.info("dojox", Some("1.13.0")))
      packageInfo.metadataLicenses must beEqualTo (Seq("BSD-3-Clause", "AFL-2.1"))
    }
  }

}
