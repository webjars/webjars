package webjars

import io.lemonlabs.uri.AbsoluteUrl
import webjars.utils.*
import webjars.utils.Classic.*
import webjars.utils.LicenseMetadata.*
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.compress.*
import zio.http.Client
import zio.stream.*
import zio.test.*

object ClassicSpec extends ZIOSpecDefault:

  private def withClassic[A](f: Classic => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val cache = CacheLive()
        val git = GitLive(client)
        val gitHub = GitHubLive(client, testConfig, cache)
        val semVer = SemVerLive(client)
        val maven = MavenLive(git, semVer)
        val licenseDetector = LicenseDetectorLive(client, testConfig.githubAuthToken)
        val npm = NPMLive(client, licenseDetector, git, gitHub, maven, semVer)
        val classic = ClassicLive(client, licenseDetector, gitHub, cache, testConfig, npm)
        f(classic)
      }
    }

  def spec = suite("Classic")(
    suite("metadata")(
      test("work") {
        withClassic { classic =>
          classic.metadata("swagger-ui").map { metadata =>
            assertTrue(metadata.asInstanceOf[MetadataNormal].name == "Swagger UI")
          }
        }
      },
      test("fail when WebJar metadata not found") {
        withClassic { classic =>
          classic.metadata("does-not-exist").exit.map(r => assertTrue(r.isFailure))
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("info")(
      test("work") {
        withClassic { classic =>
          classic.info("swagger-ui", "v5.15.1").map { info =>
            assertTrue(
              info.name == "Swagger UI",
              info.version == "5.15.1",
              info.sourceConnectionUri.toString == "https://github.com/swagger-api/swagger-ui.git",
              info.metadataLicenses.contains(SpdxLicense("Apache-2.0")),
            )
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("archive")(
      test("work") {
        withClassic { classic =>
          classic.archive("swagger-ui", "v5.15.1").flatMap { inputStream =>
            ZStream.fromInputStream(inputStream).runCollect.flatMap { bytes =>
              ZStream.fromChunk(bytes)
                .via(ZipUnarchiver.unarchive)
                .map(_._1.name)
                .runCollect
                .map { files =>
                  assertTrue(files.contains("swagger-ui-5.15.1/dist/swagger-ui.js"))
                }
            }
          }
        }
      },
      test("work when download url does not have a v") {
        withClassic { classic =>
          classic.archive("vega", "v5.32.0").flatMap { inputStream =>
            ZStream.fromInputStream(inputStream).runCollect.flatMap { bytes =>
              ZStream.fromChunk(bytes)
                .via(TarUnarchiver.unarchive)
                .map(_._1.name)
                .runCollect
                .map { files =>
                  assertTrue(files.contains("package/build/vega.min.js"))
                }
            }
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("file")(
      test("work") {
        withClassic { classic =>
          classic.file("swagger-ui", "v5.15.1", "dist/swagger-ui.js").map { file =>
            assertTrue(file.contains("swagger-ui.js.map"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("versions")(
      test("work") {
        withClassic { classic =>
          classic.versions("swagger-ui").map { versions =>
            assertTrue(versions.contains("v5.15.1"))
          }
        }
      },
      test("work for jquery") {
        withClassic { classic =>
          classic.versions("jquery").map { versions =>
            assertTrue(versions.nonEmpty)
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("datatables-select")(
      test("have versions") {
        withClassic { classic =>
          classic.versions("datatables-select").map { versions =>
            assertTrue(versions.contains("2.0.4"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("flexmonster")(
      test("have versions") {
        withClassic { classic =>
          classic.versions("flexmonster").map { versions =>
            assertTrue(versions.contains("2.9.107"))
          }
        }
      },
      test("have info") {
        withClassic { classic =>
          classic.info("flexmonster", "2.9.107").map { info =>
            assertTrue(
              info.name == "flexmonster",
              info.version == "2.9.107",
              info.dependencies.isEmpty,
              info.maybeGitHubUrl.contains(AbsoluteUrl.parse("https://github.com/flexmonster/js-pivot-table")),
              info.metadataLicenses.contains(ProvidedLicense(LicenseWithNameAndUrl("Flexmonster Terms and Conditions", AbsoluteUrl.parse("https://www.flexmonster.com/terms/Flexmonster-Terms-and-Conditions.pdf")))),
            )
          }
        }
      },
      test("basedirglob") {
        withClassic { classic =>
          classic.maybeBaseDirGlob("flexmonster").map { baseDirGlob =>
            assertTrue(baseDirGlob.contains("*/"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(300.seconds)
