package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.Classic
import webjars.utils.Classic.*
import zio.test.*

object ClassicParseMetadataSpec extends ZIOSpecDefault:

  def spec = suite("Classic.parseMetadata")(
    test("parse normal metadata") {
      val props = """name=Swagger UI
                    |repo=swagger-api/swagger-ui
                    |base.dir=*/dist/""".stripMargin
      val result = Classic.parseMetadata("swagger-ui", props)
      assertTrue(
        result.isSuccess,
        result.get.isInstanceOf[MetadataNormal],
        result.get.id == MavenCentral.ArtifactId("swagger-ui"),
        result.get.asInstanceOf[MetadataNormal].name == "Swagger UI",
        result.get.asInstanceOf[MetadataNormal].repo == "swagger-api/swagger-ui",
        result.get.asInstanceOf[MetadataNormal].baseDir.contains("*/dist/"),
      )
    },
    test("parse npm metadata") {
      val props = """npm=flexmonster
                    |license.name=Flexmonster Terms
                    |license.url=https://www.flexmonster.com/terms""".stripMargin
      val result = Classic.parseMetadata("flexmonster", props)
      assertTrue(
        result.isSuccess,
        result.get.isInstanceOf[MetadataNpm],
        result.get.id == MavenCentral.ArtifactId("flexmonster"),
        result.get.asInstanceOf[MetadataNpm].packageName == "flexmonster",
        result.get.asInstanceOf[MetadataNpm].licenseName.contains("Flexmonster Terms"),
        result.get.asInstanceOf[MetadataNpm].licenseUrl.contains("https://www.flexmonster.com/terms"),
      )
    },
    test("fail on invalid metadata") {
      val props = "invalid=true"
      val result = Classic.parseMetadata("test", props)
      assertTrue(result.isFailure)
    },
    test("parse metadata with download url") {
      val props = """name=Vega
                    |repo=vega/vega
                    |download=https://registry.npmjs.org/vega/-/vega-${version}.tgz""".stripMargin
      val result = Classic.parseMetadata("vega", props)
      assertTrue(
        result.isSuccess,
        result.get.asInstanceOf[MetadataNormal].download.contains("https://registry.npmjs.org/vega/-/vega-${version}.tgz"),
      )
    },
    test("parse normal metadata with license overrides") {
      val props = """name=jQuery UI
                    |repo=jquery/jquery-ui
                    |license.name=MIT License
                    |license.url=https://github.com/jquery/jquery-ui/blob/main/LICENSE.txt""".stripMargin
      val result = Classic.parseMetadata("jquery-ui", props)
      assertTrue(
        result.isSuccess,
        result.get.isInstanceOf[MetadataNormal],
        result.get.asInstanceOf[MetadataNormal].licenseName.contains("MIT License"),
        result.get.asInstanceOf[MetadataNormal].licenseUrl.contains("https://github.com/jquery/jquery-ui/blob/main/LICENSE.txt"),
      )
    },
    test("parse normal metadata without license overrides") {
      val props = """name=Swagger UI
                    |repo=swagger-api/swagger-ui""".stripMargin
      val result = Classic.parseMetadata("swagger-ui", props)
      assertTrue(
        result.isSuccess,
        result.get.asInstanceOf[MetadataNormal].licenseName.isEmpty,
        result.get.asInstanceOf[MetadataNormal].licenseUrl.isEmpty,
      )
    },
  )
