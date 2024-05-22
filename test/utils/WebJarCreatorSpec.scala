package utils

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.zip.{ZipArchiveEntry, ZipArchiveInputStream}
import play.api.test._

import java.io.ByteArrayInputStream
import java.net.URI
import java.util.zip.GZIPInputStream
import scala.concurrent.ExecutionContext

class WebJarCreatorSpec extends PlaySpecification with GlobalApplication {

  "WebJarUtils" should {
    "create a WebJar from a tgz" in {
      val url = new URI(s"https://registry.npmjs.org/npm/-/npm-2.10.0.tgz").toURL
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, Some("*/"), Set("node_modules"), "test", "Test", Set.empty, "test", "test", "2.10.0", "test")
      webJar.length must beGreaterThan(0)
    }
    "deal with different tgz base dirs" in {
      val url = new URI(s"https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz").toURL
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, Some("*/"), Set("node_modules"), "", "Test", Set.empty, "org.webjars.npm", "react-redux", "4.4.32", "react-redux/4.4.32/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/package.json")
    }
    "handle packages where the contents are in the base dir" in {
      val url = new URI(s"https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz").toURL
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, None, Set("node_modules"), "", "Test", Set.empty, "org.webjars.npm", "react-redux", "4.4.32", "react-redux/4.4.32/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/react-redux/package.json")
    }
    "create subdirectories for contents" in {
      val url = new URI(s"https://registry.npmjs.org/@types/react-router/-/react-router-2.0.41.tgz").toURL
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, Some("*/"), Set("node_modules"), "", "Test", Set.empty, "org.webjars.npm", "react-router", "2.0.41", "react-router/2.0.41/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val maybeLib = LazyList.continually(archiveStream.getNextEntry).find(_.getName == "META-INF/resources/webjars/react-router/2.0.41/lib/")
      maybeLib.exists(_.isDirectory) must beTrue
    }
    "handle non gzip tgzs" in {
      val url = new URI(s"https://registry.npmjs.org/@types/escodegen/-/escodegen-0.0.2.tgz").toURL
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, Some("*/"), Set("node_modules"), "", "Test", Set.empty, "org.webjars.npm", "escodegen", "0.0.2", "escodegen/0.0.2/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/escodegen/0.0.2/package.json")
    }

    "isExcluded must work with **" in {
      val excludes = Set("**/foo")

      WebJarCreator.isExcluded(excludes, "foo", true) must beTrue
      WebJarCreator.isExcluded(excludes, "test", false) must beFalse
      WebJarCreator.isExcluded(excludes, "test/test", false) must beFalse
      WebJarCreator.isExcluded(excludes, "foo/test", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/foo", true) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/foo/test", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/foo/foo", true) must beTrue
    }
    "isExcluded must work with *" in {
      val excludes = Set("*.foo")

      WebJarCreator.isExcluded(excludes, "asdf.foo", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/adsf.foo", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/test", false) must beFalse
    }
    "isExcluded must work with ** and *" in {
      val excludes = Set("**/.*")

      WebJarCreator.isExcluded(excludes, ".foo", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/.foo", false) must beTrue
      WebJarCreator.isExcluded(excludes, "foo/test", false) must beFalse
    }

    "exclude ** globs" in {
      val url = new URI("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip").toURL
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("**/test.js")

      val webJar = WebJarCreator.createWebJar(inputStream, Some("*/"), excludes, "", "Test", Set.empty, "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/visual/test.js"
    }
    "exclude * globs" in {
      val url = new URI("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip").toURL
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("*.js")

      val webJar = WebJarCreator.createWebJar(inputStream, Some("*/"), excludes, "", "Test", Set.empty, "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/visual/test.js"
    }
    "exclude ** and * globs" in {
      val url = new URI("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip").toURL
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("**/.*")

      val webJar = WebJarCreator.createWebJar(inputStream, Some("*/"), excludes, "", "Test", Set.empty, "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/.eslintrc.json"
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/.eslintrc.json"
    }
  }

  "vaadin-ordered-layout-1.0.0-alpha3" should {
    "not have duplicate dir entries" in {
      val url = new URI(s"https://bower-as-a-service.herokuapp.com/download/vaadin-ordered-layout/1.0.0-alpha3").toURL
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar[ZipArchiveEntry](inputStream, None, Set(".bower.json"), "", "Test", Set.empty, "org.webjars.bower", "vaadin-ordered-layout", "1.0.0-alpha3", "vaadin-ordered-layout/1.0.0-alpha3/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/vaadin-ordered-layout/1.0.0-alpha3/bower.json")
    }
  }

  "Created WebJar" should {
    "not have files in the root that are the directories" in {
      val url = new URI(s"https://registry.npmjs.org/virtual-keyboard/-/virtual-keyboard-1.30.1.tgz").toURL
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, Some("*/"), Set("node_modules"), "", "Test", Set.empty, "org.webjars.npm", "virtual-keyboard", "1.30.1", "virtual-keyboard/1.30.1/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain ("META-INF/")
      allNames must contain ("META-INF/resources/")
      allNames must contain ("META-INF/resources/webjars/")
      allNames must contain ("META-INF/resources/webjars/virtual-keyboard/")
      allNames must contain ("META-INF/resources/webjars/virtual-keyboard/1.30.1/")
      allNames must contain ("META-INF/resources/webjars/virtual-keyboard/1.30.1/package.json")
      allNames must not contain "META-INF"
    }
  }

  "work for classic" in {
    val version = "5.15.0"
    val url = new URI(s"https://github.com/swagger-api/swagger-ui/archive/v$version.zip").toURL
    val inputStream = url.openConnection().getInputStream

    val webJar = WebJarCreator.createWebJar(inputStream, Some("*/dist/"), Set.empty, "", "Test", Set.empty, "org.webjars", "swagger-ui", version, s"swagger-ui/$version/")

    val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

    val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
    allNames.size mustEqual 26
    allNames must contain(s"META-INF/resources/webjars/swagger-ui/$version/swagger-ui.js")
  }

  "work for classic from deployable" in {
    val name = "swagger-ui"
    val version = "v5.15.2"
    lazy val classic: Classic = application.injector.instanceOf[Classic]
    val info = await(classic.info(name, version))
    val archive = await(classic.archive(name, version))
    val maybeBaseGlob = await(classic.maybeBaseDirGlob(name))
    val excludes = await(classic.excludes(name, version))
    val releaseVersion = classic.releaseVersion(Some(version), info)
    val pathPrefix = await(classic.pathPrefix(name, releaseVersion, info))

    val webJar = WebJarCreator.createWebJar(archive, maybeBaseGlob, excludes, "", "Test", Set.empty, "org.webjars", "swagger-ui", releaseVersion, pathPrefix)

    val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

    val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
    allNames must contain(s"META-INF/resources/webjars/swagger-ui/$releaseVersion/swagger-ui.js")
  }

  "work with osgi" in {
    val name = "swagger-ui"
    val version = "v5.15.2"
    lazy val classic: Classic = application.injector.instanceOf[Classic]
    implicit lazy val ec: ExecutionContext = application.injector.instanceOf[ExecutionContext]
    val info = await(classic.info(name, version))
    val archive = await(classic.archive(name, version))
    val maybeBaseGlob = await(classic.maybeBaseDirGlob(name))
    val excludes = await(classic.excludes(name, version))
    val releaseVersion = classic.releaseVersion(Some(version), info)
    val pathPrefix = await(classic.pathPrefix(name, releaseVersion, info))
    val licenses = await(classic.licenses(name, version, info))

    val webJar = WebJarCreator.createWebJar(archive, maybeBaseGlob, excludes, "", info.name, licenses, "org.webjars", "swagger-ui", releaseVersion, pathPrefix)

    val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new ByteArrayInputStream(webJar))

    LazyList.continually(archiveStream.getNextEntry).find(_.getName == "META-INF/MANIFEST.MF").get

    val manifest = new String(archiveStream.readAllBytes())
    manifest must contain("Bundle-Description: WebJar for Swagger UI")
    manifest must contain("Bundle-License: Apache-2.0")
    manifest must contain("Bundle-SymbolicName: org.webjars.swagger-ui")
    manifest must contain("Bundle-Name: Swagger UI")
    manifest must contain("Bundle-Version: 5.15.2")
    manifest must contain("Bundle-ManifestVersion: 2")
  }

  "removeGlobPath" in {
    WebJarCreator.removeGlobPath("*", "asdf/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("*/", "asdf/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("asdf/", "asdf/") must beNone
    WebJarCreator.removeGlobPath("asdf/", "asdf/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("zxcv/", "asdf/foo") must beNone
    WebJarCreator.removeGlobPath("*/dist/", "asdf/dist/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("asdf/dist", "asdf/dist/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("asdf/dist/", "asdf/dist/foo") must beSome("foo")
    WebJarCreator.removeGlobPath("zxcv/dist/", "asdf/dist/foo") must beNone
    WebJarCreator.removeGlobPath("*/dist", "asdf/") must beNone
    WebJarCreator.removeGlobPath("*/dist", "asdf/dist/asdf/foo") must beSome("asdf/foo")
    WebJarCreator.removeGlobPath("*/dist", "asdf/dist/asdf/foo/") must beSome("asdf/foo/")
  }

}
