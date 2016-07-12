package actors


import javax.inject.Inject

import akka.actor.Actor
import akka.pattern.pipe
import models.WebJarCatalog.WebJarCatalog
import utils.MavenCentral

import scala.concurrent.ExecutionContext

class WebJarFetcher @Inject() (mavenCentral: MavenCentral) (implicit executionContext: ExecutionContext) extends Actor {

  override def receive = {
    case FetchWebJars(catalog) => mavenCentral.fetchWebJars(catalog).pipeTo(sender())
  }

}

case class FetchWebJars(catalog: WebJarCatalog)
