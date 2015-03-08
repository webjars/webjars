package utils

import play.api.test._
import play.api.test.Helpers._

import scala.concurrent.ExecutionContext

class BowerSpec extends PlaySpecification {

  val ws = StandaloneWS.apply()
  val bower = Bower(ExecutionContext.global, ws)

  "jquery info" should {
    "work with a correct version" in {
      await(bower.info("jquery", "1.11.1")).artifactId must equalTo("jquery")
    }
    "fail with an invalid version" in {
      await(bower.info("jquery", "0.0.0")) must throwA[Exception]
    }
    "have a license" in {
      await(bower.info("jquery", "1.11.1")).licenses must contain("MIT")
    }
  }
  "bootstrap" should {
    "have a license" in {
      await(bower.info("bootstrap", "3.3.2")).licenses must contain("MIT")
    }
    "have a dependency on jquery" in {
      await(bower.info("bootstrap", "3.3.2")).dependencies must contain("jquery" -> ">= 1.9.1")
    }
  }

  step(ws.close())

}