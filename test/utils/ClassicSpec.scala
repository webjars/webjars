package utils

import io.lemonlabs.uri.AbsoluteUrl
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
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
      await(classic.metadata("swagger-ui")).asInstanceOf[Classic.MetadataNormal].name must beEqualTo ("Swagger UI")
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
      info.metadataLicenses should contain(SpdxLicense("Apache-2.0"))
    }
  }

  "archive" should {
    "work" in {
      val inputStream = await(classic.archive("swagger-ui", "v5.15.1"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("swagger-ui-5.15.1/dist/swagger-ui.js")
    }
    "work when download url does not have a v" in {
      val inputStream = await(classic.archive("vega", "v5.32.0"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[TarArchiveInputStream](new BufferedInputStream(inputStream))

      LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName) must contain ("package/build/vega.min.js")
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

  "datatables-select" should {
    "have versions" in {
      val versions = await(classic.versions("datatables-select"))
      versions should contain("2.0.4")
    }
    "have the files in the right place" in {
      val inputStream = await(classic.archive("datatables-select", "2.0.4"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[ZipArchiveInputStream](new BufferedInputStream(inputStream))
      val files = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must contain ("Select-2.0.4/js/dataTables.select.min.js")
    }
  }

  "flexmonster" should {
    "have versions" in {
      val versions = await(classic.versions("flexmonster"))
      versions should contain("2.9.107")
    }
    "have info" in {
      val info = await(classic.info("flexmonster", "2.9.107"))
      info.name must beEqualTo("flexmonster")
      info.version must beEqualTo("2.9.107")
      info.dependencies must beEmpty
      info.maybeGitHubUrl must beSome(AbsoluteUrl.parse("https://github.com/flexmonster/js-pivot-table"))
      info.metadataLicenses must contain(ProvidedLicense(LicenseWithNameAndUrl("Flexmonster Terms and Conditions", AbsoluteUrl.parse("https://www.flexmonster.com/terms/Flexmonster-Terms-and-Conditions.pdf"))))
    }
    "have the files in the right place" in {
      val inputStream = await(classic.archive("flexmonster", "2.9.107"))
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[TarArchiveInputStream](new BufferedInputStream(inputStream))
      val files = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      files must contain ("package/flexmonster.js")
    }
    "basedirglob" in {
      val baseDirGlob = await(classic.maybeBaseDirGlob("flexmonster"))
      baseDirGlob must beSome("*/")
    }
  }

}
