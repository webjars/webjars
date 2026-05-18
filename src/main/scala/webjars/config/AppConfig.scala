package webjars.config

import com.typesafe.config.ConfigFactory
import zio.*

case class AppConfig(
  githubAuthToken: Option[String],
  ossGpgKey: Option[String],
  ossGpgPass: Option[String],
  ossDeployUsername: Option[String],
  ossDeployPassword: Option[String],
  fileServiceUrl: String,
  mavenCentralRefreshInterval: Option[Duration],
  useWebJarsCdn: Boolean,
)

object AppConfig:
  val live: ZLayer[Any, Throwable, AppConfig] = ZLayer.fromZIO {
    ZIO.attempt {
      val config = ConfigFactory.load()

      def optString(key: String): Option[String] =
        if config.hasPath(key) then
          val v = config.getString(key)
          if v.nonEmpty then Some(v) else None
        else None

      def optBoolean(key: String): Option[Boolean] =
        if config.hasPath(key) then Some(config.getBoolean(key)) else None

      AppConfig(
        githubAuthToken = optString("github.auth.token"),
        ossGpgKey = optString("oss.gpg-key"),
        ossGpgPass = optString("oss.gpg-pass"),
        ossDeployUsername = optString("oss.deploy.username"),
        ossDeployPassword = optString("oss.deploy.password"),
        fileServiceUrl = optString("webjars.file-service.url").getOrElse("https://webjars-file-service.herokuapp.com"),
        mavenCentralRefreshInterval = Some(1.hour),
        useWebJarsCdn = optBoolean("webjars.cdn").getOrElse(true),
      )
    }
  }
