package webjars

import io.lemonlabs.uri.AbsoluteUrl
import webjars.utils.{CacheLive, GitHub, GitHubLive, ServerError}
import webjars.TestInfrastructure.testConfig
import zio.*
import zio.http.Client
import zio.test.*

object GitHubSpec extends ZIOSpecDefault:

  def spec = suite("GitHub")(
    suite("gitHubUrl")(
      test("work normally") {
        assertTrue(
          GitHub.gitHubUrl("git://github.com/isaacs/inherits").get == AbsoluteUrl.parse("https://github.com/isaacs/inherits"),
          GitHub.gitHubUrl("https://github.com/isaacs/inherits").get == AbsoluteUrl.parse("https://github.com/isaacs/inherits"),
          GitHub.gitHubUrl(AbsoluteUrl.parse("https://github.com/isaacs/inherits.git")).get == AbsoluteUrl.parse("https://github.com/isaacs/inherits"),
          GitHub.gitHubUrl(AbsoluteUrl.parse("https://www.github.com/isaacs/inherits.git")).get == AbsoluteUrl.parse("https://www.github.com/isaacs/inherits"),
          GitHub.gitHubUrl(AbsoluteUrl.parse("https://github.com/zippyui/react-flex#readme")).get == AbsoluteUrl.parse("https://github.com/zippyui/react-flex"),
          GitHub.gitHubUrl("https://foo.com").isFailure,
        )
      },
    ),
    suite("currentUrls")(
      test("work for good urls") {
        for
          client <- ZIO.service[Client]
          gitHub = GitHubLive(client, testConfig, CacheLive())
          (homepage, _, _) <- ZIO.scoped(gitHub.currentUrls(AbsoluteUrl.parse("https://github.com/isaacs/inherits")))
        yield assertTrue(homepage == AbsoluteUrl.parse("https://github.com/isaacs/inherits"))
      },
      test("fail for not founds") {
        for
          client <- ZIO.service[Client]
          gitHub = GitHubLive(client, testConfig, CacheLive())
          result <- ZIO.scoped(gitHub.currentUrls(AbsoluteUrl.parse("http://github.com/asdf1234/zxcv4321"))).exit
        yield assertTrue(result.isFailure)
      },
      test("work for sub paths") {
        for
          client <- ZIO.service[Client]
          gitHub = GitHubLive(client, testConfig, CacheLive())
          (homepage, gitUrl, issuesUrl) <- ZIO.scoped(gitHub.currentUrls(AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components/tree/esco-content-menu/main/@uportal/esco-content-menu-lit")))
        yield assertTrue(
          homepage == AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components"),
          gitUrl == AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components.git"),
          issuesUrl == AbsoluteUrl.parse("https://github.com/GIP-RECIA/uPortal-web-components/issues"),
        )
      },
    ) @@ TestAspect.withLiveClock,
    suite("raw")(
      test("work for valid files") {
        for
          client <- ZIO.service[Client]
          gitHub = GitHubLive(client, testConfig, CacheLive())
          url = GitHub.gitHubUrl("https://github.com/PolymerElements/iron-behaviors.git").get
          content <- ZIO.scoped(gitHub.raw(url, "v2.0.0", "bower.json"))
        yield assertTrue(content.contains("{"))
      },
    ) @@ TestAspect.withLiveClock,
    suite("tags")(
      test("work with paging") {
        for
          client <- ZIO.service[Client]
          gitHub = GitHubLive(client, testConfig, CacheLive())
          versions <- ZIO.scoped(gitHub.tags("swagger-api/swagger-ui"))
        yield assertTrue(versions.size >= 413)
      },
    ) @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(120.seconds)
