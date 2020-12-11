package utils

import akka.util.Timeout
import play.api.test._

import java.net.URI
import scala.concurrent.duration._

class SourceLocatorSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  lazy val sourceLocator: SourceLocator = application.injector.instanceOf[SourceLocator]

  "https://git-r3lab.uni.lu/Fractalis/fractal.js.git" should {
    "work" in {
      val sourceUrl = await(sourceLocator.sourceUrl(new URI("https://git-r3lab.uni.lu/Fractalis/fractal.js.git")))
      sourceUrl.toString must beEqualTo ("https://git-r3lab.uni.lu/Fractalis/fractal.js")
    }
  }

  "https://github.com/angular/bower-angular-touch.git" should {
    "work" in {
      val sourceUrl = await(sourceLocator.sourceUrl(new URI("https://github.com/angular/bower-angular-touch.git")))
      sourceUrl.toString must beEqualTo ("https://github.com/angular/bower-angular-touch")
    }
  }

  "an invalid URL" should {
    "not work" in {
      await(sourceLocator.sourceUrl(new URI("asdf"))) should throwA[Exception]
    }
  }

}
