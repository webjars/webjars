package utils

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.Configuration
import play.api.test._

import scala.util.Try


class HerokuSpec extends PlaySpecification with GlobalApplication {

  implicit lazy val materializer = application.injector.instanceOf[Materializer]
  implicit lazy val actorSystem = application.injector.instanceOf[ActorSystem]
  lazy val heroku = application.injector.instanceOf[Heroku]
  lazy val configuration = application.injector.instanceOf[Configuration]
  lazy val app = configuration.get[String]("deploy.herokuapp")

  "dynoCreate" should {
    if (Try(heroku.apikey).isSuccess) {
      "return a stream when attach is true" in {
        val source = heroku.dynoCreate(app, "echo test", "Standard-1X")
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
