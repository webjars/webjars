package actors


import javax.inject.Inject

import akka.actor.Actor
import akka.pattern.pipe
import models.WebJarType
import utils.MavenCentral

import scala.concurrent.ExecutionContext

class WebJarFetcher @Inject() (mavenCentral: MavenCentral) (implicit executionContext: ExecutionContext) extends Actor {

  override def receive = {
    case FetchWebJars(webJarType) => mavenCentral.fetchWebJars(webJarType).pipeTo(sender())
  }

}

case class FetchWebJars(webJarType: WebJarType)
