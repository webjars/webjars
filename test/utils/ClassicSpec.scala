package utils

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream
import org.apache.pekko.util.Timeout
import play.api.test._

import java.io.BufferedInputStream
import scala.concurrent.duration._

class ClassicSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  lazy val classic: Classic = application.injector.instanceOf[Classic]

  "metadata" should {
    "work" in {
      await(classic.metadata("swagger-ui")).name must beEqualTo ("Swagger UI")
    }
    "fail when WebJar metadata not found" in {
      await(classic.metadata("does-not-exist")) must throwA[Exception]
    }
  }

  "info" should {
    "work" in {
      val info = await(classic.info("swagger-ui", "v5.15.1"))
      info.name shouldEqual "Swagger UI"
      info.version shouldEqual "5.15.1"
      info.sourceConnectionUri.toString shouldEqual "https://github.com/swagger-api/swagger-ui.git"
      info.metadataLicenses should contain("Apache-2.0")
    }
  }

  "archive" should {
    "work" in {
      val inputStream = await(classic.archive("swagger-ui", "v5.15.1"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("swagger-ui-5.15.1/dist/swagger-ui.js")
    }
  }

  "file" should {
    "work" in {
      val file = await(classic.file("swagger-ui", "v5.15.1", "dist/swagger-ui.js"))
      file must contain("swagger-ui.js.map")
    }
  }

  "versions" should {
    "work" in {
      val versions = await(classic.versions("swagger-ui"))
      versions should contain("v5.15.1")
    }
    "not work when the WebJar does not support the new classic deployment" in {
      await(classic.versions("jquery")) must throwA[Exception]
    }
  }

}
