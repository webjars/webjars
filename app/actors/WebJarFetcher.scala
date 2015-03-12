package actors


import akka.actor.Actor
import akka.pattern.pipe
import models.WebJar
import models.WebJarCatalog.WebJarCatalog
import utils.MavenCentral

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class WebJarFetcher(catalog: WebJarCatalog) extends Actor {

  override def receive = {
    case FetchWebJars => MavenCentral.fetchWebJars(catalog).pipeTo(sender())
  }

}

case object FetchWebJars