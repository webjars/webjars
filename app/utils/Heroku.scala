package utils

import java.net.URI
import javax.inject.Inject
import javax.net.ssl.SSLContext

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{Materializer, OverflowStrategy, TLSProtocol, TLSRole}
import akka.stream.TLSProtocol.NegotiateNewSession
import akka.stream.scaladsl.{BidiFlow, BroadcastHub, Flow, Framing, MergeHub, Sink, Source, TLS, Tcp}
import akka.util.ByteString
import play.api.Configuration
import play.api.http.{HeaderNames, Status}
import play.api.libs.json.{JsValue, Json}
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

class Heroku @Inject() (ws: WSClient, config: Configuration) (implicit ec: ExecutionContext, actorSystem: ActorSystem, materializer: Materializer) {

  lazy val apikey = config.get[String]("heroku.apikey")


  private def ws(path: String): WSRequest = {
    ws
      .url("https://api.heroku.com" + path)
      .withHttpHeaders(
        HeaderNames.ACCEPT -> "application/vnd.heroku+json; version=3",
        HeaderNames.AUTHORIZATION -> s"Bearer $apikey"
      )
  }

  def rendezvous(url: String): Source[String, NotUsed] = {
    val maybeUri = Try(new URI(url))

    def connect(uri: URI): Source[String, NotUsed] = {
      val connection = Tcp().outgoingConnection(uri.getHost, uri.getPort)

      val secret = uri.getPath.stripPrefix("/")

      val tls = TLS(SSLContext.getDefault, NegotiateNewSession.withDefaults, TLSRole.client)

      val tlsSupport = BidiFlow.fromFlows(
        Flow[ByteString].map(TLSProtocol.SendBytes),
        Flow[TLSProtocol.SslTlsInbound].collect {
          case TLSProtocol.SessionBytes(_, sb) => sb
        }
      )

      val byteStringToString = BidiFlow.fromFlows(
        Flow[ByteString].via(Framing.delimiter(ByteString("\r\n"), maximumFrameLength = 1024, allowTruncation = true)).map(_.utf8String),
        Flow[String].map(ByteString(_))
      )

      val tlsClient = tlsSupport.atop(tls).join(connection).join(byteStringToString)

      Source.single(secret).via(tlsClient).dropWhile(_ == "rendezvous")
    }

    maybeUri.fold(Source.failed, connect)
  }

  def dynoCreate(app: String, attach: Boolean, command: String, size: String): Future[Either[JsValue, Source[String, NotUsed]]] = {
    val json = Json.obj(
      "attach" -> attach,
      "command" -> command,
      "size" -> size
    )

    ws(s"/apps/$app/dynos").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          (response.json \ "attach_url").asOpt[String].fold {
            Future.successful[Either[JsValue, Source[String, NotUsed]]](Left(response.json))
          } { attachUrl =>
            Future.successful(Right(rendezvous(attachUrl)))
          }
        case _ =>
          Future.failed(new Exception(response.body))
      }
    }
  }

}
