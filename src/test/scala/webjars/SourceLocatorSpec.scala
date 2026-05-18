package webjars

import webjars.utils.*
import zio.*
import zio.http.{Client, URL}
import zio.test.*

object SourceLocatorSpec extends ZIOSpecDefault:

  def spec = suite("SourceLocator")(
    test("https://github.com/angular/bower-angular-touch.git should work") {
      for
        client <- ZIO.service[Client]
        git = GitLive(client)
        sourceLocator = SourceLocatorLive(client, git)
        sourceUrl <- ZIO.scoped(sourceLocator.sourceUrl(URL.unsafeParse("https://github.com/angular/bower-angular-touch.git")))
      yield assertTrue(sourceUrl == URL.unsafeParse("https://github.com/angular/bower-angular-touch"))
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default) @@ TestAspect.timeout(30.seconds)
