package utils

import java.net.{InetSocketAddress, URI}

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{BidiFlow, Flow, Framing, Source, Tcp}
import akka.util.ByteString
import javax.inject.Inject
import javax.net.ssl.SSLContext
import play.api.Configuration
import play.api.http.{HeaderNames, Status}
import play.api.libs.json.Json
import play.api.libs.ws.{WSClient, WSRequest}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Heroku @Inject() (ws: WSClient, config: Configuration) (implicit ec: ExecutionContext, actorSystem: ActorSystem) {

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
      val secret = uri.getPath.stripPrefix("/")

      val address = InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)

      val engine = () => {
        val engine = SSLContext.getDefault.createSSLEngine
        engine.setUseClientMode(true)
        engine
      }

      val connection = Tcp().outgoingConnectionWithTls(address, engine)

      val byteStringToString = BidiFlow.fromFlows(
        Flow[ByteString].via(Framing.delimiter(ByteString("\r\n"), maximumFrameLength = 16384, allowTruncation = true)).map(_.utf8String),
        Flow[String].map(ByteString(_))
      )

      val tlsClient = connection.join(byteStringToString)

      Source.single(secret).via(tlsClient).dropWhile(_ == "rendezvous")
    }

    maybeUri.fold(Source.failed, connect)
  }

  def dynoCreate(app: String, command: String, size: String): Source[String, Future[NotUsed]] = {
    val json = Json.obj(
      "attach" -> true,
      "command" -> command,
      "size" -> size
    )

    val sourceFuture = ws(s"/apps/$app/dynos").post(json).flatMap { response =>
      response.status match {
        case Status.CREATED =>
          Future.successful {
            (response.json \ "attach_url").asOpt[String].fold[Source[String, NotUsed]] {
              Source.failed[String](new Exception(response.body))
            } { attachUrl =>
              rendezvous(attachUrl)
            }
          }
        case _ => Future.failed(new Exception(response.body))
      }
    }

    Source.futureSource(sourceFuture)
  }

}
