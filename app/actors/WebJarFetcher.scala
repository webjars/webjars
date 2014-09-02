package actors

import akka.actor.Actor
import akka.pattern.pipe
import models.WebJar
import utils.MavenCentral
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class WebJarFetcher extends Actor {

  var maybeFetchFuture: Option[Future[List[WebJar]]] = None

  override def receive = {
    case FetchWebJars =>

      if (maybeFetchFuture.isEmpty) {
        val fetchFuture = MavenCentral.fetchWebJars()
        fetchFuture.onComplete {
          case _ => self ! FetchComplete
        }
        maybeFetchFuture = Some(fetchFuture)
      }

      maybeFetchFuture.foreach { fetchFuture =>
        fetchFuture pipeTo sender
      }
    case FetchComplete =>
      maybeFetchFuture = None
  }

}

case object FetchWebJars
case object FetchComplete