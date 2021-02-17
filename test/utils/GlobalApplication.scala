package utils

import org.specs2.specification.AfterAll
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.WithApplication

trait GlobalApplication extends AfterAll {

  lazy val application = new GuiceApplicationBuilder()
    .overrides(bind[Memcache].to[MemcacheMock], bind[MavenCentral].to[MavenCentralMock]).build()

  override def afterAll(): Unit = {
    application.stop()
  }

}

class WithMocks extends WithApplication(
  _.overrides(bind[Memcache].to[MemcacheMock], bind[MavenCentral].to[MavenCentralMock])
)