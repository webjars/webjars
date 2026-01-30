package utils

import org.specs2.specification.AfterAll
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.WithApplication

trait GlobalApplication extends AfterAll {

  lazy val application = new GuiceApplicationBuilder()
    .overrides(
      bind[Valkey].to[ValkeyTest],
      bind[MavenCentralDeployer].to[MavenCentralDeployerMock],
    )
    .configure("webjars.classic.branch" -> "dev", "mavencentral.limit" -> "5")
    .build()

  override def afterAll(): Unit = {
    application.stop()
  }

}

class WithMocks extends WithApplication(
  _.overrides(
    bind[Valkey].to[ValkeyTest],
    bind[MavenCentralDeployer].to[MavenCentralDeployerMock],
    bind[MavenCentralWebJars].to[MavenCentralWebJarsMock],
  )
)
