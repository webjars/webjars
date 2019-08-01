package utils

import java.io.ByteArrayInputStream
import java.net.URL
import java.util.zip.GZIPInputStream

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

class WebJarCreatorSpec extends PlaySpecification {

  "WebJarUtils" should {
    "create a WebJar from a tgz" in {
      val url = new URL(s"https://registry.npmjs.org/npm/-/npm-2.10.0.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "test", "test", "test", "2.10.0", "test")
      webJar.length must beGreaterThan(0)
    }
    "deal with different tgz base dirs" in {
      val url = new URL(s"https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "", "org.webjars.npm", "react-redux", "4.4.32", "react-redux/4.4.32/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/package.json")
    }
    "handle packages where the contents are in the base dir" in {
      val url = new URL(s"https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, false, Set("node_modules"), "", "org.webjars.npm", "react-redux", "4.4.32", "react-redux/4.4.32/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/react-redux/package.json")
    }
    "create subdirectories for contents" in {
      val url = new URL(s"https://registry.npmjs.org/@types/react-router/-/react-router-2.0.41.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "", "org.webjars.npm", "react-router", "2.0.41", "react-router/2.0.41/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val maybeLib = LazyList.continually(archiveStream.getNextEntry).find(_.getName == "META-INF/resources/webjars/react-router/2.0.41/lib/")
      maybeLib.exists(_.isDirectory) must beTrue
    }
    "handle non gzip tgzs" in {
      val url = new URL(s"https://registry.npmjs.org/@types/escodegen/-/escodegen-0.0.2.tgz")
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, true, Set("node_modules"), "", "org.webjars.npm", "escodegen", "0.0.2", "escodegen/0.0.2/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

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
      val url = new URL("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip")
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("**/test.js")

      val webJar = WebJarCreator.createWebJar(inputStream, true, excludes, "", "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/visual/test.js"
    }
    "exclude * globs" in {
      val url = new URL("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip")
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("*.js")

      val webJar = WebJarCreator.createWebJar(inputStream, true, excludes, "", "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/visual/test.js"
    }
    "exclude ** and * globs" in {
      val url = new URL("https://github.com/vaadin/vaadin-grid/archive/v4.0.0-alpha5.zip")
      val inputStream = url.openConnection().getInputStream

      val excludes = Set("**/.*")

      val webJar = WebJarCreator.createWebJar(inputStream, true, excludes, "", "", "", "", "vaadin-grid/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSet
      allNames must contain ("META-INF/resources/webjars/vaadin-grid/test/visual/sorting.html")
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/.eslintrc.json"
      allNames must not contain "META-INF/resources/webjars/vaadin-grid/test/.eslintrc.json"
    }
  }

  "vaadin-ordered-layout-1.0.0-alpha3" should {
    "not have duplicate dir entries" in {
      val url = new URL(s"https://bower-as-a-service.herokuapp.com/download/vaadin-ordered-layout/1.0.0-alpha3")
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, false, Set(".bower.json"), "", "org.webjars.bower", "vaadin-ordered-layout", "1.0.0-alpha3", "vaadin-ordered-layout/1.0.0-alpha3/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/vaadin-ordered-layout/1.0.0-alpha3/bower.json")
    }
  }

  "Created WebJar" should {
    "not have files in the root that are the directories" in {
      val url = new URL(s"https://registry.npmjs.org/virtual-keyboard/-/virtual-keyboard-1.30.1.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "", "org.webjars.npm", "virtual-keyboard", "1.30.1", "virtual-keyboard/1.30.1/")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).filterNot(_.isDirectory).map(_.getName)
      allNames must contain ("META-INF/resources/webjars/virtual-keyboard/1.30.1/package.json")
      allNames must not contain "META-INF"
    }
  }

}
