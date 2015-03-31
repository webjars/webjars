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
  }

  step {
    await(binTray.deletePackage("webjars", "test", "foo"))
  }

  step(ws.close())

}