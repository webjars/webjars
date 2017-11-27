package utils

import java.io.ByteArrayInputStream
import java.net.URL
import java.util.zip.GZIPInputStream

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

class WebJarCreatorSpec extends PlaySpecification {

  "WebJarUtils" should {
    "create a WebJar from a tgz" in {
      val url = new URL(s"http://registry.npmjs.org/npm/-/npm-2.10.0.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "test", "test", "test", "2.10.0")
      webJar.length must beGreaterThan(0)
    }
    "deal with different tgz base dirs" in {
      val url = new URL(s"http://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "", "org.webjars.npm", "react-redux", "4.4.32")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/package.json")
    }
    "handle packages where the contents are in the base dir" in {
      val url = new URL(s"http://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, false, Set("node_modules"), "", "org.webjars.npm", "react-redux", "4.4.32")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/react-redux/4.4.32/react-redux/package.json")
    }
    "handle packages where the contents are in the base dir" in {
      val url = new URL(s"http://registry.npmjs.org/@types/react-router/-/react-router-2.0.41.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, true, Set("node_modules"), "", "org.webjars.npm", "react-router", "2.0.41")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val maybeLib = Stream.continually(archiveStream.getNextEntry).find(_.getName == "META-INF/resources/webjars/react-router/2.0.41/lib/")
      maybeLib.exists(_.isDirectory) must beTrue
    }
    "handle non gzip tgzs" in {
      val url = new URL(s"http://registry.npmjs.org/@types/escodegen/-/escodegen-0.0.2.tgz")
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, true, Set("node_modules"), "", "org.webjars.npm", "escodegen", "0.0.2")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/escodegen/0.0.2/package.json")
    }
  }

  "vaadin-ordered-layout-1.0.0-alpha3" should {
    "not have duplicate dir entries" in {
      val url = new URL(s"https://bower-as-a-service.herokuapp.com/download/vaadin-ordered-layout/1.0.0-alpha3")
      val inputStream = url.openConnection().getInputStream

      val webJar = WebJarCreator.createWebJar(inputStream, false, Set(".bower.json"), "", "org.webjars.bower", "vaadin-ordered-layout", "1.0.0-alpha3")

      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(new ByteArrayInputStream(webJar))

      val allNames = Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      allNames must contain("META-INF/resources/webjars/vaadin-ordered-layout/1.0.0-alpha3/bower.json")
    }
  }

}
