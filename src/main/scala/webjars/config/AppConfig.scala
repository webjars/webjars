package webjars.config

import com.typesafe.config.ConfigFactory
import zio.*

case class AppConfig(
  githubAuthToken: Option[String],
  ossGpgKey: String,
  ossGpgPass: String,
  ossDeployUsername: String,
  ossDeployPassword: String,
  herokuApikey: Option[String],
  deployHerokuApp: String,
  deployFork: Boolean,
  fileServiceUrl: String,
  mavenCentralLimit: Option[Int],
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

      def requiredString(key: String): String =
        optString(key).getOrElse(throw IllegalStateException(s"Required config '$key' is not set"))

      AppConfig(
        githubAuthToken = optString("github.auth.token"),
        ossGpgKey = requiredString("oss.gpg-key"),
        ossGpgPass = requiredString("oss.gpg-pass"),
        ossDeployUsername = requiredString("oss.deploy.username"),
        ossDeployPassword = requiredString("oss.deploy.password"),
        herokuApikey = optString("heroku.apikey"),
        deployHerokuApp = optString("deploy.herokuapp").getOrElse("webjars-test"),
        deployFork = optBoolean("deploy.fork").getOrElse(false),
        fileServiceUrl = optString("webjars.file-service.url").getOrElse("https://webjars-file-service.herokuapp.com"),
        mavenCentralLimit = None,
        mavenCentralRefreshInterval = Some(1.hour),
        useWebJarsCdn = optBoolean("webjars.cdn").getOrElse(true),
      )
    }
  }
