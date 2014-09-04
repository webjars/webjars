package actors

import akka.actor.Actor
import akka.pattern.pipe
import models.WebJar
import utils.MavenCentral
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class WebJarFetcher extends Actor {

  var maybeFetchFuture: Option[Future[List[WebJar]]] = None

  override def receive = {
    case FetchWebJars =>

      if (maybeFetchFuture.isEmpty) {
        val fetchFuture = MavenCentral.fetchWebJars()
        fetchFuture.onComplete {
          case Success(s) => self ! FetchComplete
          case Failure(f) => self ! FetchRetry
        }
        maybeFetchFuture = Some(fetchFuture)
      }

      maybeFetchFuture.foreach { fetchFuture =>
        fetchFuture pipeTo sender
      }

    case FetchComplete =>
      maybeFetchFuture = None

    case FetchRetry =>
      maybeFetchFuture = None
      self ! FetchWebJars
  }

}

case object FetchWebJars
case object FetchComplete
case object FetchRetry