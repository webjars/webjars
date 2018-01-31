package utils

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Sink
import play.api.Configuration
import play.api.libs.json.JsValue
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
      "return json when attach is false" in {
        val source = heroku.dynoCreate(app, false, "echo test", "Standard-2X")
        val output = await(source.to(Sink.ignore).run())
        output must beSome
      }
      "return a stream when attach is true" in {
        val source = heroku.dynoCreate(app, true, "echo test", "Standard-2X")
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
