package actors


import org.apache.pekko.actor.Actor
import org.apache.pekko.pattern.pipe
import utils.MavenCentral

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class WebJarFetcher @Inject() (mavenCentral: MavenCentral) (using executionContext: ExecutionContext) extends Actor {

  override def receive = {
    case FetchWebJars(webJarType) => mavenCentral.fetchWebJars(webJarType).pipeTo(sender()).foreach(identity)
  }

}

case class FetchWebJars(groupId: String)
