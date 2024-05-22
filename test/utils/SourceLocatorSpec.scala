package utils

import io.lemonlabs.uri.AbsoluteUrl
import org.apache.pekko.util.Timeout
import play.api.test._

import scala.concurrent.duration._

class SourceLocatorSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val sourceLocator: SourceLocator = application.injector.instanceOf[SourceLocator]

  "https://git-r3lab.uni.lu/Fractalis/fractal.js.git" should {
    "work" in {
      val sourceUrl = await(sourceLocator.sourceUrl(AbsoluteUrl.parse("https://git-r3lab.uni.lu/Fractalis/fractal.js.git")))
      sourceUrl must beEqualTo (AbsoluteUrl.parse("https://git-r3lab.uni.lu/Fractalis/fractal.js"))
    }
  }

  "https://github.com/angular/bower-angular-touch.git" should {
    "work" in {
      val sourceUrl = await(sourceLocator.sourceUrl(AbsoluteUrl.parse("https://github.com/angular/bower-angular-touch.git")))
      sourceUrl must beEqualTo (AbsoluteUrl.parse("https://github.com/angular/bower-angular-touch"))
    }
  }

  "an invalid URL" should {
    "not work" in {
      await(sourceLocator.sourceUrl(AbsoluteUrl.parse("asdf"))) should throwA[Exception]
    }
  }

  // seems to be gone
  /*
  "git://git.coolaj86.com/coolaj86/atob.js.git" should {
    "work" in {
      val sourceUrl = await(sourceLocator.sourceUrl(new URI("git://git.coolaj86.com/coolaj86/atob.js.git")))
      sourceUrl.toString must beEqualTo("https://git.coolaj86.com/coolaj86/atob.js.git")
    }
  }
   */

}
