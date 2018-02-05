package utils

import akka.stream.Materializer
import play.api.test.PlaySpecification

import scala.concurrent.ExecutionContext

class DeployWebJarSpec extends PlaySpecification with GlobalApplication {

  implicit lazy val ec: ExecutionContext = application.injector.instanceOf[ExecutionContext]
  implicit lazy val mat: Materializer = application.injector.instanceOf[Materializer]

  // todo

}
