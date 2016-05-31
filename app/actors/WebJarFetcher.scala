package actors


import akka.actor.Actor
import akka.pattern.pipe
import models.WebJarCatalog.WebJarCatalog
import utils.MavenCentral

import scala.concurrent.ExecutionContext.Implicits.global

class WebJarFetcher(catalog: WebJarCatalog) extends Actor {

  override def receive = {
    case FetchWebJars => MavenCentral.fetchWebJars(catalog).pipeTo(sender())
  }

}

case object FetchWebJars