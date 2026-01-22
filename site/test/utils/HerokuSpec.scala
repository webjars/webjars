package utils

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import play.api.Configuration
import play.api.test._


class HerokuSpec extends PlaySpecification with GlobalApplication {

  implicit lazy val materializer: Materializer = application.injector.instanceOf[Materializer]
  implicit lazy val actorSystem: ActorSystem = application.injector.instanceOf[ActorSystem]
  private lazy val heroku = application.injector.instanceOf[Heroku]
  private lazy val configuration = application.injector.instanceOf[Configuration]
  private lazy val app = configuration.get[String]("deploy.herokuapp")

  "dynoCreate" should {
    if (heroku.maybeApikey.isDefined) {
      "return a stream when attach is true" in {
        val source = heroku.dynoCreate(app, "echo test", "Eco")
        val output = await(source.runReduce(_ + _))
        output must beEqualTo("test")
      }
    }
    else {
      "Heroku Config Not Set" in {
        skipped
      }
    }
  }

}
