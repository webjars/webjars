package webjars.utils

import webjars.config.AppConfig
import webjars.generated.WebJars as Gen
import zio.*
import zio.http.*

trait WebJars:
  def url(artifact: Gen.Artifact, path: String): URL

class WebJarsLive(appConfig: AppConfig) extends WebJars:
  override def url(artifact: Gen.Artifact, path: String): URL =
    val raw = if appConfig.useWebJarsCdn then Gen.cdnUrl(artifact, path) else Gen.localUrl(artifact, path)
    URL.decode(raw).toOption.get

object WebJars:
  val live: URLayer[AppConfig, WebJars] = ZLayer.fromFunction(WebJarsLive(_))
