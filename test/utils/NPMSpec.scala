package utils

import akka.util.Timeout
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class NPMSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  val ws = StandaloneWS.apply()
  val npm = NPM(ExecutionContext.global, ws.client)

  "chokidar 1.0.1" should {
    "have a license" in {
      await(npm.info("chokidar", "1.0.1")).licenses must contain ("MIT")
    }
  }
  "inflight 1.0.4" should {
    "have the correct github url" in {
      await(npm.info("inflight", "1.0.4")).gitHubHome must beASuccessfulTry("https://github.com/npm/inflight")
    }
  }
  "inherits 2.0.1" should {
    "have a homepage" in {
      await(npm.info("inherits", "2.0.1")).homepage must beEqualTo ("https://github.com/isaacs/inherits")
    }
  }

  step(ws.close())

}