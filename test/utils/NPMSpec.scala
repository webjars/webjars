package utils

import io.lemonlabs.uri.{AbsoluteUrl, Host, Path}
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.pekko.util.Timeout
import play.api.libs.concurrent.Futures
import play.api.libs.json._
import play.api.test._

import java.io.BufferedInputStream
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

class NPMSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 10.minutes

  lazy val npm: NPM = application.injector.instanceOf[NPM]
  lazy implicit val ec: ExecutionContext = application.injector.instanceOf[ExecutionContext]
  lazy implicit val futures: Futures = application.injector.instanceOf[Futures]

  "inflight 1.0.4" should {
    "have the correct github url" in {
      await(npm.info("inflight", "1.0.4")).maybeGitHubUrl must beSome(AbsoluteUrl.parse("https://github.com/isaacs/inflight-DEPRECATED-DO-NOT-USE"))
    }
  }

  "inherits 2.0.1" should {
    "have a homepage" in {
      await(npm.info("inherits", "2.0.1")).maybeHomepageUrl must beSome(AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
    }
  }

  "simple-fmt" should {
    "have an issue tracking url" in {
      await(npm.info("simple-fmt", "0.1.0")).maybeIssuesUrl must beSome(AbsoluteUrl.parse("https://github.com/olov/simple-fmt/issues"))
    }
  }

  "weinre 2.0.0-pre-I0Z7U9OV" should {
    "have a correct vcs url" in {
      val info = await(npm.info("weinre", "2.0.0-pre-I0Z7U9OV"))
      info.maybeGitHubUrl must beNone
    }
  }

  "valid git url" should {
    "have versions" in {
      val versions = await(npm.versions("visionmedia/mocha"))
      versions must not be empty
      versions.contains("1.0.0") must beTrue
    }
  }

  "invalid git url" should {
    "fail" in {
      await(npm.versions("foo/bar")) must throwA[Exception]
    }
  }

  "git repo tagged version info" should {
    "work" in {
      val info = await(npm.info("mochajs/mocha", "2.2.5"))
      info.name must beEqualTo ("mocha")
      info.version must beEqualTo ("2.2.5")
    }
  }

  "git repo tagged version zip" should {
    "work" in {
      val tgz = await(npm.archive("mochajs/mocha", "2.2.5"))
      val bufferedInputStream = new BufferedInputStream(tgz)
      new ArchiveStreamFactory().createArchiveInputStream[TarArchiveInputStream](bufferedInputStream)
      bufferedInputStream.available() must beEqualTo (640512)
    }
  }

  "git fork - github short url" should {
    "have the correct urls" in {
      val info = await(npm.info("btford/route-recognizer", "0.1.1"))
      info.name must beEqualTo ("route-recognizer")
      info.version mustNotEqual ""
      info.maybeHomepageUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer"))
      info.sourceConnectionUri must beEqualTo (AbsoluteUrl.parse("https://github.com/btford/route-recognizer.git"))
      info.maybeIssuesUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer/issues"))
      info.maybeGitHubUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer"))
    }
  }

  "git fork - git url" should {
    "have the correct urls" in {
      val info = await(npm.info("git://github.com/btford/route-recognizer", "0.1.1"))
      info.name must beEqualTo ("route-recognizer")
      info.version mustNotEqual ""
      info.maybeHomepageUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer"))
      info.sourceConnectionUri must beEqualTo (AbsoluteUrl.parse("https://github.com/btford/route-recognizer.git"))
      info.maybeIssuesUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer/issues"))
      info.maybeGitHubUrl must beSome (AbsoluteUrl.parse("https://github.com/btford/route-recognizer"))
    }
  }

  "info on amp-ui 3.2.0" should {
    "fail with a nice error" in {
      await(npm.info("amp-ui", "3.2.0")) must throwA[MissingMetadataException]
    }
  }

  "versions on redux" should {
    "return versions" in {
      await(npm.versions("redux")) must containAllOf(Seq("3.0.4", "0.0.1"))
    }
  }

  "scoped packages" should {
    "have info" in {
      await(npm.info("@reactivex/rxjs", "5.0.0-alpha.7")).name must beEqualTo("@reactivex/rxjs")
    }
    "have versions" in {
      await(npm.versions("@reactivex/rxjs")) must contain("5.0.0-alpha.7")
    }
    "have a tgz" in {
      val tgz = await(npm.archive("@reactivex/rxjs", "5.0.0-alpha.7"))
      val bufferedInputStream = new BufferedInputStream(tgz)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[TarArchiveInputStream](bufferedInputStream)
      bufferedInputStream.available() must beEqualTo (1687)
      archiveStream.getNextEntry.getName must beEqualTo ("package/package.json")
    }
  }

  "typescript versions" should {
    "work" in {
      await(npm.versions("typescript")) must contain("1.7.5")
    }
  }

  "npm info for git@github.com:yiminghe/async-validator.git" should {
    "have a uri format for the sourceConnectionUri" in {
      val info = await(npm.info("async-validator", "1.0.0"))
      info.sourceConnectionUri.scheme must beEqualTo("https")
      info.sourceConnectionUri.host must beEqualTo(Host.parse("github.com"))
      info.sourceConnectionUri.path must beEqualTo(Path.parse("/yiminghe/async-validator.git"))
    }
  }

  "repositoryToUri" should {
    "work with regular urls" in {
      val ssh = NPM.repositoryToUri("ssh://host.xz/another/repo.git").get
      ssh.scheme must beEqualTo("ssh")
      ssh.host must beEqualTo(Host.parse("host.xz"))
      ssh.path must beEqualTo(Path.parse("/another/repo.git"))

      val git = NPM.repositoryToUri("git://host.xz/another/repo.git").get
      git.scheme must beEqualTo("git")
      git.host must beEqualTo(Host.parse("host.xz"))
      git.path must beEqualTo(Path.parse("/another/repo.git"))

      val https = NPM.repositoryToUri("https://host.xz/another/repo.git").get
      https.scheme must beEqualTo("https")
      https.host must beEqualTo(Host.parse("host.xz"))
      https.path must beEqualTo(Path.parse("/another/repo.git"))
    }
    "work with git ssh short syntax" in {
      val plain = NPM.repositoryToUri("host.xz:/another/repo.git").get
      plain.scheme must beEqualTo("ssh")
      plain.host must beEqualTo(Host.parse("host.xz"))
      plain.path must beEqualTo(Path.parse("/another/repo.git"))

      val another = NPM.repositoryToUri("host.xz:another/repo.git").get
      another.scheme must beEqualTo("ssh")
      another.host must beEqualTo(Host.parse("host.xz"))
      another.path must beEqualTo(Path.parse("/another/repo.git"))

      val user = NPM.repositoryToUri("user@host.xz:/another/repo.git").get
      user.scheme must beEqualTo("ssh")
      user.host must beEqualTo(Host.parse("host.xz"))
      user.path must beEqualTo(Path.parse("/another/repo.git"))
      user.userInfo.map(_.user) must beSome("user")

      val anotherUser = NPM.repositoryToUri("user@host.xz:another/repo.git").get
      anotherUser.scheme must beEqualTo("ssh")
      anotherUser.host must beEqualTo(Host.parse("host.xz"))
      anotherUser.path must beEqualTo(Path.parse("/another/repo.git"))
      anotherUser.userInfo.map(_.user) must beSome("user")
    }
    "work with gist short syntax" in {
      val uri = NPM.repositoryToUri("gist:11081aaa281").get
      uri.scheme must beEqualTo("https")
      uri.host must beEqualTo(Host.parse("gist.github.com"))
      uri.path must beEqualTo(Path.parse("/11081aaa281.git"))
    }
    "work with bitbucket short syntax" in {
      val uri = NPM.repositoryToUri("bitbucket:another/repo").get
      uri.scheme must beEqualTo("https")
      uri.host must beEqualTo(Host.parse("bitbucket.org"))
      uri.path must beEqualTo(Path.parse("/another/repo.git"))
    }
    "work with gitlab short syntax" in {
      val uri = NPM.repositoryToUri("gitlab:another/repo").get
      uri.scheme must beEqualTo("https")
      uri.host must beEqualTo(Host.parse("gitlab.com"))
      uri.path must beEqualTo(Path.parse("/another/repo.git"))
    }
    "work wuth github short syntax" in {
      val uri = NPM.repositoryToUri("another/repo").get
      uri.scheme must beEqualTo("https")
      uri.host must beEqualTo(Host.parse("github.com"))
      uri.path must beEqualTo(Path.parse("/another/repo.git"))
    }
  }

  "npm info for @types/react" should {
    "work for 15.0.3" in {
      val info = await(npm.info("@types/react", "15.0.3"))
      info.name must beEqualTo("@types/react")
      info.version must beEqualTo("15.0.3")
    }
  }

  "quadkeytools" should {
    "work for 0.0.2" in {
      val info = await(npm.info("quadkeytools", "0.0.2"))
      info.maybeIssuesUrl must beSome (AbsoluteUrl.parse("https://bitbucket.org/steele/quadkeytools/issues"))
    }
  }

  "react-dnd" should {
    "fail with a useful error" in {
      val failedInfo = Try(await(npm.info("react-dnd", "2.4.0")))
      failedInfo must beAFailedTry[PackageInfo].withThrowable[MissingMetadataException]
      // todo: failedInfo.asInstanceOf[Failure[MissingMetadataException]].get.errors must have size 1
    }
  }

  "optionalDependencies" should {
    "not be dependencies" in {
      val info = await(npm.info("linkifyjs", "2.1.4"))
      info.dependencies must beEmpty
      info.optionalDependencies must have size 3
    }
  }

  "bugsReaderNullable" should {
    "not fail if there is no bugs path" in {
      val json = Json.obj()
      val result = json.validate[Option[AbsoluteUrl]](NPM.bugsReaderNullable)
      result must beEqualTo(JsSuccess[Option[AbsoluteUrl]](None))
    }
    "work if bugs is a url" in {
      val json = Json.obj(
        "bugs" -> "https://webjars.org"
      )
      val result = json.validate[Option[AbsoluteUrl]](NPM.bugsReaderNullable)
      result must beEqualTo(JsSuccess[Option[AbsoluteUrl]](AbsoluteUrl.parseOption("https://webjars.org"), __))
    }
    "work if bugs.url is a url" in {
      val json = Json.obj(
        "bugs" -> Json.obj(
          "url" -> "https://webjars.org"
        )
      )
      val result = json.validate[Option[AbsoluteUrl]](NPM.bugsReaderNullable)
      result must beEqualTo(JsSuccess[Option[AbsoluteUrl]](AbsoluteUrl.parseOption("https://webjars.org"), __))
    }
    "work if homepage has a GitHub url" in {
      val json = Json.obj(
        "homepage" -> "https://github.com/webjars/webjars"
      )
      val result = json.validate[Option[AbsoluteUrl]](NPM.bugsReaderNullable)
      result must beEqualTo(JsSuccess[Option[AbsoluteUrl]](AbsoluteUrl.parseOption("https://github.com/webjars/webjars/issues"), __))
    }
    "work if homepage has a GitHub url and bugs.url is set" in {
      val json = Json.obj(
        "homepage" -> "https://github.com/webjars/webjars",
        "bugs" -> Json.obj(
          "url" -> "https://webjars.org"
        )
      )
      val result = json.validate[Option[AbsoluteUrl]](NPM.bugsReaderNullable)
      result must beEqualTo(JsSuccess[Option[AbsoluteUrl]](AbsoluteUrl.parseOption("https://webjars.org"), __))
    }
  }

  "homepageToIssuesReader" should {
    "work with missing homepage" in {
      val json = Json.obj()
      val result = json.validate[AbsoluteUrl](NPM.homepageToIssuesReader)
      result must beAnInstanceOf[JsError]
    }
    "work with unknown homepage" in {
      val json = Json.obj("homepage" -> "https://webjars.org")
      val result = json.validate[AbsoluteUrl](NPM.homepageToIssuesReader)
      result must beAnInstanceOf[JsError]
    }
    "work with github homepage" in {
      val json = Json.obj("homepage" -> "https://github.com/webjars/webjars")
      val result = json.validate[AbsoluteUrl](NPM.homepageToIssuesReader)
      result must beEqualTo(JsSuccess[AbsoluteUrl](AbsoluteUrl.parse("https://github.com/webjars/webjars/issues"), __ \ "homepage"))
    }
  }

  "amdefine" should {
    "not work without a source override" in {
      await(npm.info("amdefine", "0.0.4")) must throwA[MissingMetadataException]
    }
    "work with a source override" in {
      val uri = AbsoluteUrl.parse("https://webjars.org")
      val info = await(npm.info("amdefine", "0.0.4", Some(uri)))
      info.sourceConnectionUri must beEqualTo(uri)
    }
  }

  "@types/escodegen 0.0.2" should {
    "have a valid archive" in {
      val is = await(npm.archive("@types/escodegen", "0.0.2"))
      is.read() must beGreaterThan(Int.MinValue)
    }
  }

  "artifactId" should {
    "deal with orgs" in {
      val packageInfo = await(npm.info("@types/react", "16.9.55"))
      await(npm.artifactId("@types/react", packageInfo.version)) must beEqualTo ("types__react")
    }
  }

  "electron-to-chromium 1.3.28" should {
    "work" in {
      val packageInfo = await(npm.info("electron-to-chromium", "1.3.28"))
      packageInfo.sourceConnectionUri must beEqualTo (AbsoluteUrl.parse("https://github.com/kilian/electron-to-chromium.git"))
    }
  }

  "depGraph" should {
    "work" in {
      val packageInfo = await(npm.info("ng-bootstrap-modal", "1.1.19"))
      val depGraph = await(npm.depGraph(packageInfo))
      depGraph.keys must contain ("path-is-absolute")
    }
    "deal with undeployables" in {
      // is-color-stop depends on an undeployable package 'rgba-regex'
      val packageInfo = await(npm.info("is-color-stop", "1.1.0"))
      val depGraph = await(npm.depGraph(packageInfo))
      depGraph.keySet must contain ("hsl-regex")
    }
  }

  "babel crap" should {
    "work" in {
      val info = await(npm.info("@babel/runtime", "7.12.1"))
      info.sourceConnectionUri must beEqualTo (AbsoluteUrl.parse("https://github.com/babel/babel.git"))
    }
  }

  "file" should {
    "work with an NPM artifact" in {
      val file = await(npm.file("jquery", "3.5.1", "dist/jquery.js"))
      file must contain ("jQuery JavaScript Library v3.5.1")
    }
    "work with a git repo" in {
      val file = await(npm.file("jquery/jquery", "3.5.1", "dist/jquery.js"))
      file must contain ("jQuery JavaScript Library v3.5.1")
    }
  }

  "mapbox-gl" should {
    "have the right license for version 1.12.0 of the NPM package" in {
      val packageInfo = await(npm.info("mapbox-gl", "1.12.0"))
      await(npm.licenses("mapbox-gl", "1.12.0", packageInfo)) must beEqualTo(Set(LicenseWithName("BSD 3-Clause")))
    }
    "have the right license for version v1.12.0 of the git repo" in {
      val packageInfo = await(npm.info("https://github.com/mapbox/mapbox-gl-js.git", "v1.12.0"))
      await(npm.licenses("https://github.com/mapbox/mapbox-gl-js.git", "v1.12.0", packageInfo)) must beEqualTo (Set(LicenseWithName("BSD 3-Clause")))
    }
  }

  "chokidar 1.0.1" should {
    "have a license" in {
      val packageInfo = await(npm.info("chokidar", "1.0.1"))
      await(npm.licenses("chokidar", "1.0.1", packageInfo)) must contain (LicenseWithName("MIT"))
    }
  }

  "entities 1.0.0" should {
    "fail with a useful error" in {
      val packageInfo = await(npm.info("entities", "1.0.0"))
      await(npm.licenses("entities", "1.0.0", packageInfo)) must beEqualTo(Set(LicenseWithName("BSD-like")))
    }
  }

  "async-validator" should {
    "have an MIT license" in {
      val packageInfo = await(npm.info("async-validator", "1.0.0"))
      await(npm.licenses("async-validator", "1.0.0", packageInfo)) must contain(LicenseWithName("MIT"))
    }
  }

  "esprima 3.1.3" should {
    "have a BSD 2-Clause license" in {
      val packageInfo = await(npm.info("esprima", "3.1.3"))
      await(npm.licenses("esprima", "3.1.3", packageInfo)) must contain(LicenseWithName("BSD-2-Clause"))
    }
  }

  "NPM @zalando/oauth2-client-js" should {
    "have the right license" in {
      val packageInfo = await(npm.info("@zalando/oauth2-client-js", "0.0.18"))
      await(npm.licenses("@zalando/oauth2-client-js", "0.0.18", packageInfo)) must beEqualTo (Set(LicenseWithName("Apache 2.0")))
    }
  }

  "licenseReference" should {
    "work with SPDX OR expressions" in {
      await(npm.licenseReference("foo", "0.0.0", "(Apache-2.0 OR MIT)")) must containTheSameElementsAs(Seq(LicenseWithName("Apache-2.0"), LicenseWithName("MIT")))
      await(npm.licenseReference("foo", "0.0.0", "(Apache-2.0 or MIT)")) must containTheSameElementsAs(Seq(LicenseWithName("Apache-2.0"), LicenseWithName("MIT")))
    }
    "work with SPDX 'SEE LICENSE IN LICENSE' expressions" in {
      val packageInfo = await(npm.info("stacktracejs/error-stack-parser", "v2.0.0"))
      val licenses = await(npm.licenses("stacktracejs/error-stack-parser", "v2.0.0", packageInfo))
      licenses must containTheSameElementsAs(Seq(LicenseWithName("Unlicense")))
    }
    "be able to be fetched from git repos" in {
      val packageInfo = await(npm.info("ms", "0.7.1"))
      await(npm.licenses("ms", "0.7.1", packageInfo)) must beEqualTo(Set(LicenseWithName("MIT")))
    }
  }

  "convert github license URL to license" in {
    val result = await(npm.licenseReference("foo", "0.0.0", "https://github.com/facebook/flux/blob/master/LICENSE"))
    result must be equalTo Set(LicenseWithNameAndUrl("BSD 3-Clause", AbsoluteUrl.parse("https://github.com/facebook/flux/blob/master/LICENSE")))
  }

  "have the right license for material-design-icons 2.2.3" in {
    val packageInfo = await(npm.info("material-design-icons", "2.2.3"))
    await(npm.licenses("material-design-icons", "2.2.3", packageInfo)) must beEqualTo (Set(LicenseWithName("CC-BY-4.0")))
  }

  "libphonenumber-js 1.9.17" in {
    val packageInfo = await(npm.info("libphonenumber-js", "1.9.17"))
    packageInfo.sourceConnectionUri must beEqualTo(AbsoluteUrl.parse("https://gitlab.com/catamphetamine/libphonenumber-js.git"))
  }

  "mapbox-gl license" in {
    val packageInfo = await(npm.info("mapbox-gl", "2.15.0"))
    val licenses = await(npm.licenses("mapbox-gl", "2.15.0", packageInfo))
    licenses mustEqual Set(LicenseWithUrl(AbsoluteUrl.parse("file://LICENSE.txt")))
  }

  "@headlessui/react ^1.7.15" in {
    val latest = await(npm.latestDep("@headlessui/react", "^1.7.15"))
    latest mustEqual "1.7.19"
  }

  "@types/node latestDep" in {
    val latest = Try(await(npm.latestDep("@types/node", ">=13.7.0")))
    latest must beASuccessfulTry
  }

  "@xenova/transformers depGraph" in {
    val packageInfo = await(npm.info("@xenova/transformers", "2.15.0"))
    val depGraph = await(npm.depGraph(packageInfo))
    depGraph.keys must contain ("@types/node")
  }

  "registryMetadataUrl" in {
    npm.registryMetadataUrl("@reactivex/rxjs") must beEqualTo(AbsoluteUrl.parse("https://registry.npmjs.org/@reactivex/rxjs"))
    npm.registryMetadataUrl("@reactivex/rxjs", Some("5.0.0-alpha.7")) must beEqualTo(AbsoluteUrl.parse("https://registry.npmjs.org/@reactivex/rxjs/5.0.0-alpha.7"))
  }

  "@gip-recia/esco-content-menu-lit 0.2.0" in {
    val packageInfo = await(npm.info("@gip-recia/esco-content-menu-lit", "0.2.0"))
    packageInfo.maybeGitHubUrl must beSome (AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components"))
  }

  "firebase" should {
    "have versions" in {
      val versions = await(npm.versions("firebase"))
      versions must not be empty
      versions.contains("10.13.0") must beTrue
    }
  }

  "npm" should {
    "work" in {
      val packageInfo = await(npm.info("npm", "10.8.2"))
      val depGraph = await(npm.depGraph(packageInfo))
      depGraph.get("string-width") must beSome("4.2.3")
    }
  }

  /*
  Broken. See: https://github.com/webjars/webjars/issues/1920
  "have the right license for hmrc-frontend 1.27.0" in {
    val packageInfo = await(npm.info("hmrc-frontend", "1.27.0"))
    await(npm.licenses("hmrc-frontend", "1.27.0", packageInfo)) must beEqualTo (Set(LicenseWithName("Apache 2.0")))
  }
   */

}
