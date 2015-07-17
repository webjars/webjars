package utils

import java.util.Date

import akka.util.Timeout
import org.apache.commons.io.IOUtils
import play.api.Play
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BinTraySpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 60.seconds

  val ws = StandaloneWS()
  val binTray = BinTray(ExecutionContext.global, ws, FakeApplication().configuration)

  "BinTray with auth" should {
    if (FakeApplication().configuration.getString("bintray.username").isEmpty)
      "BinTray Auth" in skipped("skipped due to missing config")
    else {
      "create a package" in {
        val result = await(binTray.createPackage("webjars", "test", "foo", "foo description", Seq("test"), Set("MIT"), "http://github.com/webjars/webjars", Some("http://webjars.org"), Some("http://github.com/webjars/webjars/issues"), Some("webjars/webjars")))
        (result \ "created").asOpt[Date] must beSome
      }
      "create a version" in {
        val result = await(binTray.createVersion("webjars", "test", "foo", "0.0.1", "Release 0.0.1"))
        (result \ "created").asOpt[Date] must beSome
      }
      "upload a maven artifact" in {
        val bytes = Play.resourceAsStream("foo.jar")(FakeApplication()).map { inputStream =>
          val fileBytes = IOUtils.toByteArray(inputStream)
          inputStream.close()
          fileBytes
        }.get
        val result = await(binTray.uploadMavenArtifact("webjars", "test", "foo", "org/webjars/bower/foo/0.0.1/foo-0.0.1.jar", bytes))
        (result \ "message").asOpt[String] must beSome("success")
      }
      "sign an artifact" in {
        val result = await(binTray.signVersion("webjars", "test", "foo", "0.0.1"))
        (result \ "message").asOpt[String] must beSome("success")
      }
      "publish an artifact" in {
        val result = await(binTray.publishVersion("webjars", "test", "foo", "0.0.1"))
        (result \ "files").asOpt[Int] must beSome(2)
      }

      step {
        await(binTray.deletePackage("webjars", "test", "foo"))
      }
    }
  }

  "BinTray without auth" should {
    "convert licenses to accepted ones" in {
      val licenses = Seq("BSD 2-Clause", "BSD-2-Clause", "bsd2clause", "GPLv2", "GPLv3", "MIT/X11")
      val result = await(binTray.convertLicenses(licenses))
      result must be equalTo Set("GPL-2.0", "BSD 2-Clause", "GPL-3.0", "MIT")
    }
    "convert SPDX to BinTray" in {
      val licenses = Seq("OFL-1.1")
      val result = await(binTray.convertLicenses(licenses))
      result must be equalTo Set("Openfont-1.1")
    }
    "convert raw license URL to license" in {
      val licenses = Seq("http://polymer.github.io/LICENSE.txt")
      val result = await(binTray.convertLicenses(licenses))
      result must be equalTo Set("BSD 3-Clause")
    }
    "convert github license URL to license" in {
      val licenses = Seq("https://github.com/facebook/flux/blob/master/LICENSE")
      val result = await(binTray.convertLicenses(licenses))
      result must be equalTo Set("BSD 3-Clause")
    }
    "fail to convert incompatible licenses" in {
      await(binTray.convertLicenses(Seq("foo"))) must throwA[Exception]
    }
    "fail on license conversion if no valid licenses are found" in {
      await(binTray.convertLicenses(Seq())) must throwA[Exception]
    }
    "succeed with at least one valid license" in {
      val licenses = await(binTray.convertLicenses(Seq("foo", "MIT")))
      licenses must be equalTo Set("MIT")
    }
    "work with SPDX OR expressions" in {
      val licenses = await(binTray.convertLicenses(Seq("(Apache-2.0 OR MIT)")))
      licenses must be equalTo Set("Apache-2.0", "MIT")
    }
  }

  step(ws.close())

}