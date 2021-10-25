package actors


import akka.actor.Actor
import akka.pattern.pipe
import models.WebJarType
import utils.MavenCentral

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class WebJarFetcher @Inject() (mavenCentral: MavenCentral) (implicit executionContext: ExecutionContext) extends Actor {

  override def receive = {
    case FetchWebJars(webJarType) => mavenCentral.fetchWebJars(webJarType).pipeTo(sender()).foreach(identity)
  }

}

case class FetchWebJars(webJarType: WebJarType)
