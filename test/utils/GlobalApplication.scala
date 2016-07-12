package utils

import org.specs2.specification.AfterAll
import play.api.inject.guice.GuiceApplicationBuilder

trait GlobalApplication extends AfterAll {

  lazy val application = new GuiceApplicationBuilder().build

  override def afterAll {
    application.stop()
  }

}
