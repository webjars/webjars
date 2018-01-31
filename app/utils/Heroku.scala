package utils

import java.net.URI
import javax.inject.Inject
import javax.net.ssl.SSLContext

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.TLSProtocol.NegotiateNewSession
import akka.stream.scaladsl.{BidiFlow, Flow, Framing, Sink, Source, TLS, Tcp}
import akka.stream.{Materializer, TLSProtocol, TLSRole}
import akka.util.ByteString
import play.api.Configuration
import play.api.http.{HeaderNames, Status}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.{ExecutionContext, Future}
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

  def dynoCreate(app: String, attach: Boolean, command: String, size: String): Source[String, Future[Option[JsValue]]] = {
    val json = Json.obj(
      "attach" -> attach,
      "command" -> command,
      "size" -> size
    )

    val maybeAttachUrlFuture = ws(s"/apps/$app/dynos").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          Future.successful {
            (response.json \ "attach_url").asOpt[String].fold[Source[String, Option[JsValue]]] {
              Source.empty[String].mapMaterializedValue(_ => Some(response.json))
            } { attachUrl =>
              rendezvous(attachUrl).mapMaterializedValue(_ => None)
            }
          }
        case _ => Future.failed(new Exception(response.body))
      }
    }

    Source.fromFutureSource(maybeAttachUrlFuture)
  }

}
