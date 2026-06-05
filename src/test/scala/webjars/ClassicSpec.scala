package webjars

import webjars.utils.*
import webjars.utils.Classic.*
import webjars.utils.LicenseMetadata.*
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.compress.*
import zio.http.{Client, URL}
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
    suite("jquery-ui")(
      // Reproduces https://github.com/webjars/webjars/issues/2221 — GitHub's
      // License API returns spdx_id=NOASSERTION for jquery-ui's modified
      // LICENSE.txt, which previously propagated straight into the published
      // POM as `<name>NOASSERTION</name>`. Now treated as UnresolvedLicense
      // so the .properties override / LICENSE-file fallback can take over.
      test("license is UnresolvedLicense when GitHub returns NOASSERTION") {
        withClassic { classic =>
          val metadata = MetadataNormal(
            com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("jquery-ui"),
            "jQuery UI",
            "jquery/jquery-ui",
            None, None, None, None, None,
          )
          classic.license(metadata).map { licenseMetadata =>
            assertTrue(licenseMetadata == UnresolvedLicense)
          }
        }
      },
      test("license honors license.name/license.url overrides for GitHub-based metadata") {
        withClassic { classic =>
          val metadata = MetadataNormal(
            com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("jquery-ui"),
            "jQuery UI",
            "jquery/jquery-ui",
            None, None, None,
            Some("MIT License"),
            Some("https://github.com/jquery/jquery-ui/blob/main/LICENSE.txt"),
          )
          classic.license(metadata).map { licenseMetadata =>
            assertTrue(
              licenseMetadata == ProvidedLicense(
                LicenseWithNameAndUrl("MIT License", URL.unsafeParse("https://github.com/jquery/jquery-ui/blob/main/LICENSE.txt"))
              )
            )
          }
        }
      },
      test("resolved licenses use overrides and never contain NOASSERTION") {
        withClassic { classic =>
          val metadata = MetadataNormal(
            com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("jquery-ui"),
            "jQuery UI",
            "jquery/jquery-ui",
            Some("https://jqueryui.com/resources/download/jquery-ui-${version}.zip"),
            None,
            Some("*/"),
            Some("MIT License"),
            Some("https://github.com/jquery/jquery-ui/blob/main/LICENSE.txt"),
          )
          classic.infoFromMetadata(metadata, "1.14.2", None).flatMap { packageInfo =>
            classic.licensesFromMetadata(metadata, "1.14.2", packageInfo).map { licenses =>
              assertTrue(
                licenses.nonEmpty,
                licenses.exists(_.maybeName.contains("MIT License")),
                !licenses.exists(_.maybeName.contains("NOASSERTION")),
              )
            }
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
              info.maybeGitHubUrl.contains(URL.unsafeParse("https://github.com/flexmonster/js-pivot-table")),
              info.metadataLicenses.contains(ProvidedLicense(LicenseWithNameAndUrl("Flexmonster Terms and Conditions", URL.unsafeParse("https://www.flexmonster.com/terms/Flexmonster-Terms-and-Conditions.pdf")))),
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
    suite("npm metadata with repo and base.dir overrides")(
      // @tabby_ai/hijri-converter publishes no `repository` field, so the
      // default https://github.com/<name> fallback 404s. The `repo` override
      // resolves that, and `base.dir=*/dist` scopes the JAR to the
      // published dist/ directory.
      test("info uses repo override for sourceConnectionUri") {
        withClassic { classic =>
          val metadata = MetadataNpm(
            com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("tabby_ai__hijri-converter"),
            "@tabby_ai/hijri-converter",
            Some("tabby-ai/hijri-converter"),
            Some("*/dist"),
            None, None,
          )
          classic.infoFromMetadata(metadata, "1.0.5", None).map { info =>
            assertTrue(
              info.name == "@tabby_ai/hijri-converter",
              info.version == "1.0.5",
              info.maybeGitHubUrl.contains(URL.unsafeParse("https://github.com/tabby-ai/hijri-converter")),
              info.sourceConnectionUri.toString == "https://github.com/tabby-ai/hijri-converter.git",
            )
          }
        }
      },
      // Regression: `versions(nameOrUrlish)` previously forwarded
      // `nameOrUrlish` straight to `npm.versions`, which 404s for any
      // classic NPM webjar whose .properties basename differs from the
      // NPM package name (scoped packages have to use a `__` basename
      // because `@`/`/` aren't legal in a Maven artifactId). That made
      // /exists return deployable=false and the UI surfaced
      // "The Classic WebJar … Can't Be Deployed This Way".
      test("versions resolves scoped NPM package via the .properties basename") {
        withClassic { classic =>
          classic.versions("tabby_ai__hijri-converter").map { versions =>
            assertTrue(versions.contains("1.0.5"))
          }
        }
      },
      test("base.dir override is returned by maybeBaseDirGlobFromMetadata") {
        withClassic { classic =>
          val metadata = MetadataNpm(
            com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("tabby_ai__hijri-converter"),
            "@tabby_ai/hijri-converter",
            Some("tabby-ai/hijri-converter"),
            Some("*/dist"),
            None, None,
          )
          classic.maybeBaseDirGlobFromMetadata(metadata).map { baseDirGlob =>
            assertTrue(baseDirGlob.contains("*/dist"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(300.seconds)
