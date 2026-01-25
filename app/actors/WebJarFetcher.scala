package actors


import org.apache.pekko.actor.Actor
import org.apache.pekko.pattern.pipe
import utils.MavenCentralWebJars
import com.jamesward.zio_mavencentral.MavenCentral

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class WebJarFetcher @Inject()(mavenCentral: MavenCentralWebJars)(using executionContext: ExecutionContext) extends Actor {

  override def receive = {
    case FetchWebJars(webJarType) => mavenCentral.fetchWebJars(webJarType).pipeTo(sender()).foreach(identity)
  }

}

case class FetchWebJars(groupId: MavenCentral.GroupId)
