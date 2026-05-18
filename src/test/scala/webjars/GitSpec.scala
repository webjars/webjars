package webjars

import webjars.utils.*
import zio.*
import zio.compress.*
import zio.http.{Client, URL}
import zio.stream.*
import zio.test.*

object GitSpec extends ZIOSpecDefault:

  private def withGit[A](f: Git => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        f(GitLive(client))
      }
    }

  def spec = suite("Git")(
    suite("git versions")(
      test("not work with an invalid git url") {
        withGit { git =>
          git.versions("foo/bar").exit.map(r => assertTrue(r.isFailure))
        }
      },
      test("work with git://host/path.git") {
        withGit { git =>
          git.versions("git://github.com/mochajs/mocha.git").map(v => assertTrue(v.nonEmpty))
        }
      },
      test("work with https://host/path.git") {
        withGit { git =>
          git.versions("https://github.com/mochajs/mocha.git").map(v => assertTrue(v.nonEmpty))
        }
      },
      test("work with githuborg/repo") {
        withGit { git =>
          git.versions("mochajs/mocha").map(v => assertTrue(v.nonEmpty))
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("git file")(
      test("fetch a file with a version") {
        withGit { git =>
          git.file("mochajs/mocha", "2.2.5", "package.json").map { file =>
            assertTrue(file.length == 1683, file.contains("\"version\": \"2.2.5\""))
          }
        }
      },
      test("fetch a file with a different version") {
        withGit { git =>
          git.file("mochajs/mocha", "2.2.4", "package.json").map { file =>
            assertTrue(file.length == 1663, file.contains("\"version\": \"2.2.4\""))
          }
        }
      },
      test("fetch a file with a git url syntax") {
        withGit { git =>
          git.file(URL.unsafeParse("https://github.com/yiminghe/async-validator.git"), "v3.4.0", "LICENSE.md").map { file =>
            assertTrue(file.length == 1083, file.contains("The MIT License (MIT)"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("git tar")(
      test("fetch a tar") {
        withGit { git =>
          git.tar("mochajs/mocha", "2.2.5", Set("node_modules")).flatMap { tar =>
            ZStream.fromInputStream(tar)
              .via(TarUnarchiver.unarchive)
              .map(_._1.name)
              .runCollect
              .map { files =>
                assertTrue(
                  files.size == 178,
                  !files.exists(_.contains("node_modules")),
                  !files.exists(_.contains(".git")),
                )
              }
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("artifactId")(
      test("convert a name to a name") {
        withGit { git =>
          git.artifactId("foo").map(r => assertTrue(r == "foo"))
        }
      },
      test("convert a github url to a name") {
        withGit { git =>
          git.artifactId("mochajs/mocha").map(r => assertTrue(r == "github-com-mochajs-mocha"))
        }
      },
      test("convert a git:// url to a name") {
        withGit { git =>
          git.artifactId("git://github.com/mochajs/mocha.git").map(r => assertTrue(r == "github-com-mochajs-mocha"))
        }
      },
      test("convert a https:// url to a name") {
        withGit { git =>
          git.artifactId("https://github.com/mochajs/mocha.git").map(r => assertTrue(r == "github-com-mochajs-mocha"))
        }
      },
      test("convert a scoped name") {
        withGit { git =>
          git.artifactId("@reactivex/rxjs").map(r => assertTrue(r == "reactivex__rxjs"))
        }
      },
    ) @@ TestAspect.withLiveClock,
    suite("versionsOnBranch")(
      test("get the commits on a branch") {
        withGit { git =>
          git.versionsOnBranch("git://github.com/mochajs/mocha.git", "main").map { versions =>
            assertTrue(versions.contains("8a100df959"))
          }
        }
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(120.seconds)
