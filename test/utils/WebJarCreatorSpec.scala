package utils

import java.net.URL
import java.util.zip.GZIPInputStream

import play.api.test._

class WebJarCreatorSpec extends PlaySpecification {

  "WebJarUtils" should {
    "create a WebJar from a tgz" in {

      val url = new URL(s"http://registry.npmjs.org/npm/-/npm-2.10.0.tgz")
      val inputStream = url.openConnection().getInputStream
      val gzipInputStream = new GZIPInputStream(inputStream)

      val webJar = WebJarCreator.createWebJar(gzipInputStream, "package/", Set("node_modules"), "test", "test", "test", "2.10.0")

      webJar.length must beGreaterThan (0)
      // todo: more tests
    }
  }

}
