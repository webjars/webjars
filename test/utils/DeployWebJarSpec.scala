package utils

import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink}
import play.api.test.PlaySpecification


class DeployWebJarSpec extends PlaySpecification with GlobalApplication {

  implicit lazy val materializer: Materializer = application.injector.instanceOf[Materializer]
  lazy val deployWebJar: DeployWebJar = application.injector.instanceOf[DeployWebJar]
  lazy val npm: NPM = application.injector.instanceOf[NPM]

  "DeployWebJar" should {
    "work" in {
      val output = await(deployWebJar.localDeploy(npm, "jquery", "3.2.1", false, None, None, None, true).toMat(Sink.seq)(Keep.right).run())
      output.last must contain("GroupID = org.webjars.npm")
      output.last must contain("ArtifactID = jquery")
      output.last must contain("Version = 3.2.1")
    }
    "fail when the WebJar exists" in {
      await(deployWebJar.localDeploy(npm, "jquery", "3.2.1", false).to(Sink.ignore).run()) must throwAn[IllegalStateException]
    }
    "deploy deps" in {
      val output = await(deployWebJar.localDeploy(npm, "react", "16.8.6", true, None, None, None, true).toMat(Sink.seq)(Keep.right).run())
      output(2) must contain("Deploying these dependencies:")
    }
  }

}
