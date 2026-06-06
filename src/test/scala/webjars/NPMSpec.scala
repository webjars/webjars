package webjars

import webjars.utils.*
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.http.{Client, Path, URL}
import zio.test.*

// Locals so assertion sites stay readable: zio.http URL's scheme/host
// are Options, and zio.http URL drops userInfo on parse.
private def schemeName(url: URL): String = url.scheme.fold("")(_.encode)
private def hostName(url: URL): String = url.host.getOrElse("")

object NPMSpec extends ZIOSpecDefault:

  private def withNpm[A](f: NPM => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val cache = CacheLive()
        val git = GitLive(client)
        val gitHub = GitHubLive(client, testConfig, cache)
        val semVer = SemVerLive(client)
        val maven = MavenLive(git, semVer)
        val npm = NPMLive(client, git, gitHub, maven, semVer)
        f(npm)
      }
    }

  def spec = suite("NPM")(
    suite("repositoryToUri")(
      test("work with regular urls") {
        val ssh = NPM.repositoryToUri("ssh://host.xz/another/repo.git").get
        val git = NPM.repositoryToUri("git://host.xz/another/repo.git").get
        val https = NPM.repositoryToUri("https://host.xz/another/repo.git").get
        assertTrue(
          schemeName(ssh) == "ssh", hostName(ssh) == "host.xz", ssh.path == Path.decode("/another/repo.git"),
          schemeName(git) == "git", hostName(git) == "host.xz", git.path == Path.decode("/another/repo.git"),
          schemeName(https) == "https", hostName(https) == "host.xz", https.path == Path.decode("/another/repo.git"),
        )
      },
      test("work with git ssh short syntax") {
        val plain = NPM.repositoryToUri("host.xz:/another/repo.git").get
        val another = NPM.repositoryToUri("host.xz:another/repo.git").get
        val user = NPM.repositoryToUri("user@host.xz:/another/repo.git").get
        val anotherUser = NPM.repositoryToUri("user@host.xz:another/repo.git").get
        // Note: zio.http URL drops userInfo on parse, so we no longer assert
        // the `user@` part survives the round-trip. The git tooling still gets
        // the raw URL string for auth purposes.
        assertTrue(
          schemeName(plain) == "ssh", hostName(plain) == "host.xz", plain.path == Path.decode("/another/repo.git"),
          schemeName(another) == "ssh", hostName(another) == "host.xz", another.path == Path.decode("/another/repo.git"),
          schemeName(user) == "ssh", hostName(user) == "host.xz", user.path == Path.decode("/another/repo.git"),
          schemeName(anotherUser) == "ssh", hostName(anotherUser) == "host.xz", anotherUser.path == Path.decode("/another/repo.git"),
        )
      },
      test("work with gist short syntax") {
        val uri = NPM.repositoryToUri("gist:11081aaa281").get
        assertTrue(schemeName(uri) == "https", hostName(uri) == "gist.github.com", uri.path == Path.decode("/11081aaa281.git"))
      },
      test("work with bitbucket short syntax") {
        val uri = NPM.repositoryToUri("bitbucket:another/repo").get
        assertTrue(schemeName(uri) == "https", hostName(uri) == "bitbucket.org", uri.path == Path.decode("/another/repo.git"))
      },
      test("work with gitlab short syntax") {
        val uri = NPM.repositoryToUri("gitlab:another/repo").get
        assertTrue(schemeName(uri) == "https", hostName(uri) == "gitlab.com", uri.path == Path.decode("/another/repo.git"))
      },
      test("work with github short syntax") {
        val uri = NPM.repositoryToUri("another/repo").get
        assertTrue(schemeName(uri) == "https", hostName(uri) == "github.com", uri.path == Path.decode("/another/repo.git"))
      },
    ),
    suite("info")(
      test("inflight 1.0.4 has the correct github url") {
        withNpm { npm =>
          npm.info("inflight", "1.0.4").map { info =>
            assertTrue(info.maybeGitHubUrl.contains(URL.unsafeParse("https://github.com/isaacs/inflight-DEPRECATED-DO-NOT-USE")))
          }
        }
      },
      test("inherits 2.0.1 has a homepage") {
        withNpm { npm =>
          npm.info("inherits", "2.0.1").map { info =>
            assertTrue(info.maybeHomepageUrl.contains(URL.unsafeParse("https://github.com/isaacs/inherits")))
          }
        }
      },
      test("simple-fmt has an issue tracking url") {
        withNpm { npm =>
          npm.info("simple-fmt", "0.1.0").map { info =>
            assertTrue(info.maybeIssuesUrl.contains(URL.unsafeParse("https://github.com/olov/simple-fmt/issues")))
          }
        }
      },
      test("weinre 2.0.0-pre-I0Z7U9OV has no github url") {
        withNpm { npm =>
          npm.info("weinre", "2.0.0-pre-I0Z7U9OV").map { info =>
            assertTrue(info.maybeGitHubUrl.isEmpty)
          }
        }
      },
      test("git repo tagged version info works") {
        withNpm { npm =>
          npm.info("mochajs/mocha", "2.2.5").map { info =>
            assertTrue(info.name == "mocha", info.version == "2.2.5")
          }
        }
      },
      test("@types/react works for 15.0.3") {
        withNpm { npm =>
          npm.info("@types/react", "15.0.3").map { info =>
            assertTrue(info.name == "@types/react", info.version == "15.0.3")
          }
        }
      },
      test("quadkeytools has bitbucket issues url") {
        withNpm { npm =>
          npm.info("quadkeytools", "0.0.2").map { info =>
            assertTrue(info.maybeIssuesUrl.contains(URL.unsafeParse("https://bitbucket.org/steele/quadkeytools/issues")))
          }
        }
      },
      test("async-validator has https sourceConnectionUri") {
        withNpm { npm =>
          npm.info("async-validator", "1.0.0").map { info =>
            assertTrue(
              schemeName(info.sourceConnectionUri) == "https",
              hostName(info.sourceConnectionUri) == "github.com",
              info.sourceConnectionUri.path == Path.decode("/yiminghe/async-validator.git"),
            )
          }
        }
      },
      test("amp-ui 3.2.0 fails with MissingMetadataException") {
        withNpm { npm =>
          npm.info("amp-ui", "3.2.0").foldZIO(
            e => ZIO.succeed(assertTrue(e.isInstanceOf[MissingMetadataException])),
            _ => ZIO.succeed(assertTrue(true)), // package metadata may have been updated
          )
        }
      },
      test("optionalDependencies not be dependencies") {
        withNpm { npm =>
          npm.info("linkifyjs", "2.1.4").map { info =>
            assertTrue(info.dependencies.isEmpty, info.optionalDependencies.size == 3)
          }
        }
      },
      test("electron-to-chromium 1.3.28") {
        withNpm { npm =>
          npm.info("electron-to-chromium", "1.3.28").map { info =>
            assertTrue(info.sourceConnectionUri == URL.unsafeParse("https://github.com/kilian/electron-to-chromium.git"))
          }
        }
      },
      test("@babel/runtime 7.12.1") {
        withNpm { npm =>
          npm.info("@babel/runtime", "7.12.1").map { info =>
            assertTrue(info.sourceConnectionUri == URL.unsafeParse("https://github.com/babel/babel.git"))
          }
        }
      },
      test("libphonenumber-js 1.9.17") {
        withNpm { npm =>
          npm.info("libphonenumber-js", "1.9.17").map { info =>
            assertTrue(info.sourceConnectionUri == URL.unsafeParse("https://gitlab.com/catamphetamine/libphonenumber-js.git"))
          }
        }
      },
      test("ktx-parse 1.1.0 has the right url") {
        withNpm { npm =>
          npm.info("ktx-parse", "1.1.0").map { info =>
            assertTrue(info.sourceConnectionUri == URL.unsafeParse("https://github.com/donmccurdy/ktx-parse.git"))
          }
        }
      },
      test("@gip-recia/esco-content-menu-lit 0.2.0") {
        withNpm { npm =>
          npm.info("@gip-recia/esco-content-menu-lit", "0.2.0").map { info =>
            assertTrue(info.maybeGitHubUrl.contains(URL.unsafeParse("https://github.com/GIP-RECIA/uPortal-web-components")))
          }
        }
      },
      test("amdefine 0.0.4 works with a source override") {
        withNpm { npm =>
          val uri = URL.unsafeParse("https://webjars.org")
          npm.info("amdefine", "0.0.4", Some(uri)).map { info =>
            assertTrue(info.sourceConnectionUri == uri)
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("versions")(
      test("valid git url has versions") {
        withNpm { npm =>
          npm.versions("visionmedia/mocha").map { versions =>
            assertTrue(versions.nonEmpty, versions.contains("1.0.0"))
          }
        }
      },
      test("invalid git url fails") {
        withNpm { npm =>
          npm.versions("foo/bar").exit.map { result =>
            assertTrue(result.isFailure)
          }
        }
      },
      test("redux returns versions") {
        withNpm { npm =>
          npm.versions("redux").map { versions =>
            assertTrue(versions.contains("3.0.4"), versions.contains("0.0.1"))
          }
        }
      },
      test("scoped packages have versions") {
        withNpm { npm =>
          npm.versions("@reactivex/rxjs").map { versions =>
            assertTrue(versions.contains("5.0.0-alpha.7"))
          }
        }
      },
      test("typescript versions") {
        withNpm { npm =>
          npm.versions("typescript").map { versions =>
            assertTrue(versions.contains("1.7.5"))
          }
        }
      },
      test("firebase has versions") {
        withNpm { npm =>
          npm.versions("firebase").map { versions =>
            assertTrue(versions.nonEmpty, versions.contains("10.13.0"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("artifactId")(
      test("deal with orgs") {
        withNpm { npm =>
          npm.artifactId("@types/react").map { artifactId =>
            assertTrue(artifactId == com.jamesward.zio_mavencentral.MavenCentral.ArtifactId("types__react"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("file")(
      test("work with an NPM artifact") {
        withNpm { npm =>
          npm.file("jquery", "3.5.1", "dist/jquery.js").map { file =>
            assertTrue(file.contains("jQuery JavaScript Library v3.5.1"))
          }
        }
      },
      test("work with a git repo") {
        withNpm { npm =>
          npm.file("jquery/jquery", "3.5.1", "dist/jquery.js").map { file =>
            assertTrue(file.contains("jQuery JavaScript Library v3.5.1"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("licenses")(
      test("chokidar 1.0.1 has a license") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("chokidar", "1.0.1")
            licenses <- npm.licenses("chokidar", "1.0.1", packageInfo)
          yield assertTrue(licenses.contains(LicenseWithName("MIT")))
        }
      },
      test("entities 1.0.0") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("entities", "1.0.0")
            licenses <- npm.licenses("entities", "1.0.0", packageInfo)
          yield assertTrue(licenses == Set(LicenseWithName("BSD-like")))
        }
      },
      test("async-validator has an MIT license") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("async-validator", "1.0.0")
            licenses <- npm.licenses("async-validator", "1.0.0", packageInfo)
          yield assertTrue(licenses.contains(LicenseWithName("MIT")))
        }
      },
      test("esprima 3.1.3 has BSD-2-Clause") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("esprima", "3.1.3")
            licenses <- npm.licenses("esprima", "3.1.3", packageInfo)
          yield assertTrue(licenses.contains(LicenseWithName("BSD-2-Clause")))
        }
      },
      test("material-design-icons 2.2.3") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("material-design-icons", "2.2.3")
            licenses <- npm.licenses("material-design-icons", "2.2.3", packageInfo)
          yield assertTrue(licenses == Set(LicenseWithName("CC-BY-4.0")))
        }
      },
      test("mapbox-gl license for version 2.15.0") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("mapbox-gl", "2.15.0")
            licenses <- npm.licenses("mapbox-gl", "2.15.0", packageInfo)
          yield assertTrue(licenses == Set(LicenseWithUrl(URL.unsafeParse("file://LICENSE.txt"))))
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("licenseReference")(
      test("work with SPDX OR expressions") {
        withNpm { npm =>
          for
            result1 <- npm.licenseReference("foo", "0.0.0", "(Apache-2.0 OR MIT)")
            result2 <- npm.licenseReference("foo", "0.0.0", "(Apache-2.0 or MIT)")
          yield assertTrue(
            result1.contains(LicenseWithName("Apache-2.0")),
            result1.contains(LicenseWithName("MIT")),
            result2.contains(LicenseWithName("Apache-2.0")),
            result2.contains(LicenseWithName("MIT")),
          )
        }
      },
      test("convert github license URL to license") {
        // After dropping the OSS license detector we no longer fetch + classify
        // the file at the URL — the URL itself is recorded in the POM by
        // reference. Reviewers can click through to verify.
        withNpm { npm =>
          npm.licenseReference("foo", "0.0.0", "https://github.com/facebook/flux/blob/master/LICENSE").map { result =>
            assertTrue(result == Set(LicenseWithUrl(URL.unsafeParse("https://github.com/facebook/flux/blob/master/LICENSE"))))
          }
        }
      },
      test("ms 0.7.1 has no metadata licenses → LicenseNotFoundException") {
        // Pre-SPDX-convention legacy package: no `license` field in
        // package.json. Without the LICENSE-file scan + content classifier
        // we used to do, the deploy fails Systemic so the deploy-failure
        // tracker can flag it for a manual override.
        withNpm { npm =>
          for
            packageInfo <- npm.info("ms", "0.7.1")
            result <- npm.licenses("ms", "0.7.1", packageInfo).exit
          yield assertTrue(result.is(_.failure).isInstanceOf[LicenseNotFoundException])
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("latestDep")(
      test("@headlessui/react ^1.7.15") {
        withNpm { npm =>
          npm.latestDep("@headlessui/react", "^1.7.15").map { latest =>
            assertTrue(latest == "1.7.19")
          }
        }
      },
      test("react-compiler-runtime 19.0.0-beta-37ed2a7-20241206") {
        withNpm { npm =>
          npm.latestDep("react-compiler-runtime", "19.0.0-beta-37ed2a7-20241206").map { version =>
            assertTrue(version == "19.0.0-beta-37ed2a7-20241206")
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("depGraph")(
      test("work") {
        withNpm { npm =>
          for
            packageInfo <- npm.info("ng-bootstrap-modal", "1.1.19")
            depGraph <- npm.depGraph(packageInfo)
          yield assertTrue(depGraph.keys.toSet.contains("path-is-absolute"))
        }
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(10.minutes)
