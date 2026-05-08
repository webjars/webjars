package webjars

import webjars.utils.{GitLive, Maven, MavenLive, SemVerLive}
import zio.*
import zio.http.Client
import zio.test.*

object MavenSpec extends ZIOSpecDefault:

  private def withMaven[A](f: Maven => ZIO[Scope, Throwable, A]): ZIO[Client, Throwable, A] =
    ZIO.serviceWithZIO[Client] { client =>
      ZIO.scoped {
        val git = GitLive(client)
        val semVer = SemVerLive(client)
        val maven = MavenLive(git, semVer)
        f(maven)
      }
    }

  def spec = suite("Maven")(
    suite("converting npm deps to maven")(
      test("work with standard npm deps") {
        withMaven { maven =>
          maven.convertNpmDependenciesToMaven(Map("traceur" -> "^0.0.72")).map { mavenDeps =>
            assertTrue(mavenDeps.get("traceur").contains("[0.0.72,0.0.73-0)"))
          }
        }
      },
      test("work with versionless git npm deps") {
        withMaven { maven =>
          val deps = Map(
            "route-recognizer" -> "git://github.com/btford/route-recognizer",
            "HTML5-Desktop-Notifications" -> "https://github.com/ttsvetko/HTML5-Desktop-Notifications.git",
          )
          maven.convertNpmDependenciesToMaven(deps).map { mavenDeps =>
            assertTrue(
              mavenDeps.get("github-com-btford-route-recognizer").contains("0.1.1"),
              mavenDeps.get("github-com-ttsvetko-html5-desktop-notifications").contains("3.0.0"),
            )
          }
        }
      },
      test("work with versioned git npm deps") {
        withMaven { maven =>
          val deps = Map(
            "route-recognizer" -> "git://github.com/btford/route-recognizer#0.1.1",
            "react-tools" -> "git://github.com/facebook/react.git#b4e74e38e43ac53af8acd62c78c9213be0194245",
          )
          maven.convertNpmDependenciesToMaven(deps).map { mavenDeps =>
            assertTrue(
              mavenDeps.get("github-com-btford-route-recognizer").contains("0.1.1"),
              mavenDeps.get("github-com-facebook-react").contains("b4e74e38e43ac53af8acd62c78c9213be0194245"),
            )
          }
        }
      },
      test("work with github npm deps") {
        withMaven { maven =>
          val deps = Map(
            "route-recognizer" -> "btford/route-recognizer#0.1.1",
            "react-tools" -> "github:facebook/react#b4e74e3",
          )
          maven.convertNpmDependenciesToMaven(deps).map { mavenDeps =>
            assertTrue(
              mavenDeps.get("github-com-btford-route-recognizer").contains("0.1.1"),
              mavenDeps.get("github-com-facebook-react").contains("b4e74e3"),
            )
          }
        }
      },
      test("work with scoped deps") {
        withMaven { maven =>
          val deps = Map("@reactivex/rxjs" -> "5.0.0-alpha.7")
          maven.convertNpmDependenciesToMaven(deps).map { mavenDeps =>
            assertTrue(mavenDeps.get("reactivex__rxjs").contains("5.0.0-alpha.7"))
          }
        }
      },
      test("not work with invalid value") {
        withMaven { maven =>
          val deps = Map("semantic" -> "semantic-ui#~2.1.4")
          maven.convertNpmDependenciesToMaven(deps).exit.map(r => assertTrue(r.isFailure))
        }
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(30.seconds)
