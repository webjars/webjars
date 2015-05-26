package utils

import java.util.Date

import org.apache.commons.io.IOUtils
import play.api.Play
import play.api.test._

import scala.concurrent.ExecutionContext

class BinTraySpec extends PlaySpecification {

  val ws = StandaloneWS()
  val binTray = BinTray(ExecutionContext.global, ws, FakeApplication().configuration)

  "BinTray" should {
    "create a package" in {
      val result = await(binTray.createPackage("webjars", "test", "foo", "foo description", Seq("test"), Seq("MIT"), "http://github.com/webjars/webjars", Some("http://webjars.org"), Some("http://github.com/webjars/webjars/issues"), Some("webjars/webjars")))
      (result \ "created").asOpt[Date] must beSome
    }
    "create a version" in {
      val result = await(binTray.createVersion("webjars", "test", "foo", "0.0.1", "Release 0.0.1"))
      (result \ "created").asOpt[Date] must beSome
    }
    "publish a maven release" in {
      val bytes = Play.resourceAsStream("foo.jar")(FakeApplication()).map { inputStream =>
        val fileBytes = IOUtils.toByteArray(inputStream)
        inputStream.close()
        fileBytes
      }.get
      val result = await(binTray.uploadMavenArtifact("webjars", "test", "foo", "org/webjars/bower/foo/0.0.1/foo-0.0.1.jar", bytes))
      (result \ "message").asOpt[String] must beSome ("success")
    }
    "sign an artifact" in {
      val result = await(binTray.signVersion("webjars", "test", "foo", "0.0.1"))
      (result \ "message").asOpt[String] must beSome ("success")
    }
    "convert licenses to accepted ones" in {
      val licenses = Seq("BSD 2-Clause", "BSD-2-Clause", "bsd2clause", "GPLv2", "GPLv3", "MIT/X11")
      val result = await(binTray.convertLicenses(licenses))
      result.size must be equalTo 6
      result(0) must be equalTo "BSD 2-Clause"
      result(1) must be equalTo "BSD 2-Clause"
      result(2) must be equalTo "BSD 2-Clause"
      result(3) must be equalTo "GPL-2.0"
      result(4) must be equalTo "GPL-3.0"
      result(5) must be equalTo "MIT"
    }
    "convert SPDX to BinTray" in {
      val licenses = Seq("OFL-1.1")
      val result = await(binTray.convertLicenses(licenses))
      result.size must be equalTo 1
      result(0) must be equalTo "Openfont-1.1"
    }
    "convert license URL to license" in {
      val licenses = Seq("http://polymer.github.io/LICENSE.txt")
      val result = await(binTray.convertLicenses(licenses))
      result.size must be equalTo 1
      result(0) must be equalTo "BSD 3-Clause"
    }
    "fail to convert incompatible licenses" in {
      await(binTray.convertLicenses(Seq("foo"))) must throwA[Exception]
    }
  }

  step {
    await(binTray.deletePackage("webjars", "test", "foo"))
  }

  step(ws.close())

}
