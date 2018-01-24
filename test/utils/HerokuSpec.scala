package utils

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.Configuration
import play.api.libs.json.JsValue
import play.api.test._


class HerokuSpec extends PlaySpecification with GlobalApplication {

  implicit lazy val materializer = application.injector.instanceOf[Materializer]
  implicit lazy val actorSystem = application.injector.instanceOf[ActorSystem]
  lazy val heroku = application.injector.instanceOf[Heroku]
  lazy val configuration = application.injector.instanceOf[Configuration]
  lazy val app = configuration.get[String]("deploy.herokuapp")

  "dynoCreate" should {
    "return json when attach is false" in {
      await(heroku.dynoCreate(app, false, "echo test", "Standard-2X")) must beLeft[JsValue]
    }
    "return a stream when attach is true" in {
      val deploy = await(heroku.dynoCreate(app, true, "echo test", "Standard-2X"))
      deploy must beRight
      val source = deploy.toOption.get
      val output = await(source.runReduce(_ + _))
      output must beEqualTo ("test")
    }
  }

}
