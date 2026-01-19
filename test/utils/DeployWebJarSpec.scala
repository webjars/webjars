package utils

import org.apache.pekko.stream.scaladsl.{Keep, Sink}
import org.apache.pekko.util.Timeout
import play.api.test.{PlaySpecification, WithApplication}

import scala.concurrent.duration._

class DeployWebJarSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  "DeployWebJar" should {
    "work" in new WithMocks() {
      override def running() = {
        val deployWebJar: DeployWebJar = app.injector.instanceOf[DeployWebJar]
        val npm: NPM = app.injector.instanceOf[NPM]

        val output = await(deployWebJar.localDeploy(npm, "jquery", "3.2.1", false, false).toMat(Sink.seq)(Keep.right).run())
        output.last must contain("GroupID = org.webjars.npm")
        output.last must contain("ArtifactID = jquery")
        output.last must contain("Version = 3.2.1")
      }
    }
    "fail when the WebJar exists" in new WithApplication() {
      override def running() = {
        lazy val deployWebJar: DeployWebJar = app.injector.instanceOf[DeployWebJar]
        lazy val npm: NPM = app.injector.instanceOf[NPM]

        await(deployWebJar.localDeploy(npm, "jquery", "3.2.1", false, false).to(Sink.ignore).run()) must throwAn[IllegalStateException]
      }
    }
    "deploy deps" in new WithMocks() {
      override def running() = {
        val deployWebJar: DeployWebJar = app.injector.instanceOf[DeployWebJar]
        val npm: NPM = app.injector.instanceOf[NPM]

        val output = await(deployWebJar.localDeploy(npm, "react", "16.8.6", true, false).toMat(Sink.seq)(Keep.right).run())
        output(2) must contain("Deploying these dependencies:")
      }
    }
    "work with previously invalid licenses" in new WithMocks() {
      override def running() = {
        val deployWebJar: DeployWebJar = app.injector.instanceOf[DeployWebJar]
        val npm: NPM = app.injector.instanceOf[NPM]

        val output = await(deployWebJar.localDeploy(npm, "material-design-icons", "2.2.3", true, false).toMat(Sink.seq)(Keep.right).run())
        output(4) must contain("Resolved Licenses: CC-BY-4.0")
        output(14) must contain("Deployed!")
      }
    }
    "work with Classic" in new WithMocks() {
      override def running() = {
        val deployWebJar: DeployWebJar = app.injector.instanceOf[DeployWebJar]
        val classic: Classic = app.injector.instanceOf[Classic]

        val output = await(deployWebJar.localDeploy(classic, "swagger-ui", "v5.15.1", false, false).toMat(Sink.seq)(Keep.right).run()).mkString
        output must contain("Resolved Licenses: Apache-2.0")
        output must contain("GroupID = org.webjars")
        output must contain("ArtifactID = swagger-ui")
        output must contain("Version = 5.15.1")
        output must contain("Deployed!")
      }
    }
  }

}
