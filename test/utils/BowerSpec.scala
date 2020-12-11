package utils

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.libs.concurrent.Futures
import play.api.test._

import java.io.BufferedInputStream
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BowerSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val bower: Bower = application.injector.instanceOf[Bower]

  lazy implicit val ec: ExecutionContext = application.injector.instanceOf[ExecutionContext]
  lazy implicit val futures: Futures = application.injector.instanceOf[Futures]

  "jquery info" should {
    "work with a correct version" in {
      await(bower.info("jquery", "1.11.1")).name must equalTo("jquery")
    }
    "fail with an invalid version" in {
      await(bower.info("jquery", "0.0.0")) must throwA[Exception]
    }
  }

  "bootstrap" should {
    val info = await(bower.info("bootstrap", "3.3.2"))
    "have a dependency on jquery" in {
      info.dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }

  "dc.js" should {
    "have the corrected GitHub url" in {
      await(bower.info("dc.js", "1.7.3")).maybeHomepageUrl.map(_.toString) must beSome("https://github.com/dc-js/dc.js")
    }
  }

  "sjcl" should {
    "download" in {
      val is = new BufferedInputStream(await(bower.archive("sjcl", "1.0.2")))
      val zis = new ArchiveStreamFactory().createArchiveInputStream(is)
      val files = LazyList.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName)
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

  "git repo tagged version info" should {
    "work" in {
      val info = await(bower.info("PolymerElements/iron-elements", "v1.0.0"))
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
      val info = await(bower.info("PolymerElements/iron-elements", "v1.0.0"))
      info.name must beEqualTo ("iron-elements")
      info.version mustNotEqual ""
      info.maybeHomepageUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements")
      info.sourceConnectionUri.toString must beEqualTo ("https://github.com/PolymerElements/iron-elements.git")
      info.maybeIssuesUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements/issues")
      info.maybeGitHubUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements")
    }
  }

  "git fork - git url" should {
    "have the correct urls" in {
      val info = await(bower.info("git://github.com/PolymerElements/iron-elements", "v1.0.0"))
      info.name must beEqualTo ("iron-elements")
      info.version mustNotEqual ""
      info.maybeHomepageUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements")
      info.sourceConnectionUri.toString must beEqualTo ("https://github.com/PolymerElements/iron-elements.git")
      info.maybeIssuesUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements/issues")
      info.maybeGitHubUrl.map(_.toString) must beSome ("https://github.com/PolymerElements/iron-elements")
    }
  }

  "git commits" should {
    "work as version" in {
      val info = await(bower.info("git://github.com/dc-js/dc.js", "6e95388b9a"))
      info.version must beEqualTo ("6e95388b9a")
    }
  }

  "homepage" should {
    "have a default" in {
      await(bower.info("git://github.com/millermedeiros/requirejs-plugins", "1.0.3")).maybeHomepageUrl.map(_.toString) must beSome ("https://github.com/millermedeiros/requirejs-plugins")
    }
  }

  "bootstrap" should {
    "have a dependency on jquery" in {
      val packageInfo = await(bower.info("bootstrap", "3.3.2"))
      packageInfo.dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }

  "lookup" should {
    "work with a name" in {
      val url = await(bower.lookup("jquery"))
      url.toString must beEqualTo ("https://github.com/jquery/jquery-dist")
    }
    "fail with an invalid name" in {
      await(bower.lookup("asdfqwer1234")) must throwA[Exception]
    }
    "work with a valid github git url" in {
      val url = await(bower.lookup("https://github.com/jquery/jquery-dist.git"))
      url.toString must beEqualTo ("https://github.com/jquery/jquery-dist")
    }
    "fail with an invalid url" in {
      await(bower.lookup("https://asdf.com/")) must throwA[Exception]
    }
  }

  "pathPrefix" should {
    "be in the form artifactid/releaseVersion" in {
      val packageInfo = await(bower.info("bootstrap", "3.3.2"))
      val pathPrefix = await(bower.pathPrefix("foobar", "1.2.3", packageInfo))
      pathPrefix must beEqualTo ("foobar/1.2.3/")
    }
  }

  "info" should {
    "work even when a package.json doesn't exist" in {
      await(bower.info("https://github.com/Polymer/polymer-analyzer.git", "v2.7.0")).name must equalTo("polymer-analyzer")
    }
  }

  "depGraph" should {
    "work with bootstrap" in {
      val packageInfo = await(bower.info("bootstrap", "3.3.7"))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph must beEqualTo(Map("jquery" -> "3.5.1"))
    }
    "work with " in {
      val packageInfo = await(bower.info("ng-bootstrap-select", "0.5.0"))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph.keySet must beEqualTo(Set("angular", "bootstrap", "bootstrap-select", "jquery"))
    }
    "work with PolymerElements/iron-behaviors" in {
      val packageInfo = await(bower.info("PolymerElements/iron-behaviors", "v2.0.0"))
      val depGraph = await(bower.depGraph(packageInfo))
      depGraph.keySet must beEqualTo(Set("Polymer/polymer", "polymerelements/iron-a11y-keys-behavior", "webcomponents/shadycss", "webcomponents/webcomponentsjs"))
    }
  }

  "github.com/uwdata/vega-lite" should {
    "download" in {
      val is = new BufferedInputStream(await(bower.archive("uwdata/vega-lite", "v2.1.2")))
      val zis = new ArchiveStreamFactory().createArchiveInputStream(is)
      val files = LazyList.continually(zis.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must not contain "examples/compiled/data"
    }
  }

  "dojox" should {
    "have the right licenses" in {
      val packageInfo = await(bower.info("dojox", "1.13.0"))
      packageInfo.metadataLicenses must beEqualTo (Seq("BSD-3-Clause", "AFL-2.1"))
    }
  }

  "jquery info" should {
    "have a license" in {
      val packageInfo = await(bower.info("jquery", "1.11.1"))
      await(bower.licenses("jquery", "1.11.1", packageInfo)) must contain("MIT")
    }
  }

  "bootstrap" should {
    "have a license" in {
      val packageInfo = await(bower.info("bootstrap", "3.3.2"))
      await(bower.licenses("bootstrap", "3.3.2", packageInfo)) must contain("MIT")
    }
  }

  "angular" should {
    "have an MIT license" in {
      val packageInfo = await(bower.info("angular", "1.4.0"))
      await(bower.licenses("angular", "1.4.0", packageInfo)) must contain("MIT")
    }
  }
  "angular-equalizer" should {
    "have an MIT license" in {
      val packageInfo = await(bower.info("angular-equalizer", "2.0.1"))
      await(bower.licenses("angular-equalizer", "2.0.1", packageInfo)) must contain("MIT")
    }
  }

  "zeroclipboard 2.2.0" should {
    "have an MIT license" in {
      val packageInfo = await(bower.info("zeroclipboard", "2.2.0"))
      await(bower.licenses("zeroclipboard", "2.2.0", packageInfo)) must beEqualTo(Set("MIT"))
    }
  }

  "angular-translate 2.7.2" should {
    "fail with a useful error" in {
      val packageInfo = await(bower.info("angular-translate", "2.7.2"))
      await(bower.licenses("angular-translate", "2.7.2", packageInfo)) must throwA[LicenseNotFoundException]
    }
  }

  "dojox" should {
    "have the right licenses" in {
      val packageInfo = await(bower.info("dojox", "1.13.0"))
      await(bower.licenses("dojox", "1.13.0", packageInfo)) must beEqualTo (Set("BSD 3-Clause", "AFL-2.1"))
    }
  }

  "swagger-ui" should {
    "have the right license" in {
      val packageInfo = await(bower.info("swagger-ui", "3.13.0"))
      await(bower.licenses("swagger-ui", "3.13.0", packageInfo)) must beEqualTo (Set("Apache-2.0"))
    }
  }

  "be able to be fetched from git repos" in {
    val packageInfo = await(bower.info("git://github.com/mdedetrich/requirejs-plugins", "d9c103e7a0"))
    await(bower.licenses("git://github.com/mdedetrich/requirejs-plugins", "d9c103e7a0", packageInfo)) must beEqualTo(Set("MIT"))
  }

  /*
  // This is broken due to upstream: https://github.com/webjars/webjars/issues/1265

  "tinymce-dist 4.2.5" should {
    "have an LGPL-2.1 license" in {
      await(bower.info("tinymce-dist", Some("4.2.5"))).licenses must beEqualTo (Set("LGPL-2.1"))
    }
  }
  */

}
