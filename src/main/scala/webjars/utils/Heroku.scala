package webjars.utils

import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream

import java.io.*
import javax.net.ssl.SSLContext

trait Heroku:
  def dynoCreate(app: String, command: String, size: String): ZStream[Scope, Throwable, String]

case class HerokuLive(client: Client, config: AppConfig) extends Heroku:

  private lazy val maybeApikey: Option[String] = config.herokuApikey
  private lazy val apikey: String = maybeApikey.get

  private def rendezvous(url: String): ZStream[Any, Throwable, String] =
    import io.lemonlabs.uri.AbsoluteUrl
    val maybeUri = AbsoluteUrl.parseTry(url)

    maybeUri.fold(
      e => ZStream.fail(e),
      uri => {
        val secret = uri.path.toString().stripPrefix("/")
        val host = uri.host.toString()
        val port = uri.port.get

        ZStream.unwrap {
          ZIO.attempt {
            val sslContext = SSLContext.getDefault
            val socketFactory = sslContext.getSocketFactory
            val socket = socketFactory.createSocket(host, port).asInstanceOf[javax.net.ssl.SSLSocket]
            socket.startHandshake()

            // Send the secret
            val out = socket.getOutputStream
            out.write(secret.getBytes)
            out.flush()

            val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

            ZStream.repeatZIOOption {
              ZIO.attempt(Option(in.readLine())).mapError(Some(_)).flatMap {
                case Some(line) if line == "rendezvous" => ZIO.fail(None)
                case Some(line) => ZIO.succeed(line)
                case None => ZIO.fail(None)
              }
            }.ensuring(ZIO.attempt(socket.close()).ignore)
          }
        }
      }
    )

  def dynoCreate(app: String, command: String, size: String): ZStream[Scope, Throwable, String] =
    val jsonBody = Json.Obj(
      "attach" -> Json.Bool(true),
      "command" -> Json.Str(command),
      "size" -> Json.Str(size)
    )

    ZStream.unwrap:
      defer:
        val request = Request.post(
          URL.decode(s"https://api.heroku.com/apps/$app/dynos").toOption.get,
          Body.fromString(jsonBody.toJson)
        ).addHeader(Header.Accept(MediaType("application", "vnd.heroku+json; version=3")))
         .addHeader(Header.Authorization.Bearer(apikey))
         .addHeader(Header.ContentType(MediaType.application.json))

        val response = client.request(request).run
        response.status match
          case Status.Created =>
            val body = response.body.asString.run
            val json = ZIO.fromEither(body.fromJson[Json].left.map(new Exception(_))).run
            json.asObject.flatMap(_.get("attach_url")).flatMap(_.asString) match
              case Some(attachUrl) => rendezvous(attachUrl)
              case None => ZIO.fail(new Exception(body)).run
          case _ =>
            val body = response.body.asString.run
            ZIO.fail(new Exception(body)).run

object Heroku:
  val live: ZLayer[Client & AppConfig, Nothing, Heroku] = ZLayer.derive[HerokuLive]
