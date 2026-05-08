package webjars.config

import com.typesafe.config.ConfigFactory
import zio.*

case class AppConfig(
  githubAuthToken: Option[String],
  ossDisableDeploy: Boolean,
  ossGpgKey: Option[String],
  ossGpgPass: Option[String],
  ossDeployUsername: Option[String],
  ossDeployPassword: Option[String],
  herokuApikey: Option[String],
  deployHerokuApp: String,
  deployFork: Boolean,
  fileServiceUrl: String,
  mavenCentralLimit: Option[Int],
  devMode: Boolean,
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

      def optInt(key: String): Option[Int] =
        if config.hasPath(key) then Some(config.getInt(key)) else None

      AppConfig(
        githubAuthToken = optString("github.auth.token"),
        ossDisableDeploy = optBoolean("oss.disable-deploy").getOrElse(false),
        ossGpgKey = optString("oss.gpg-key"),
        ossGpgPass = optString("oss.gpg-pass"),
        ossDeployUsername = optString("oss.deploy.username"),
        ossDeployPassword = optString("oss.deploy.password"),
        herokuApikey = optString("heroku.apikey"),
        deployHerokuApp = optString("deploy.herokuapp").getOrElse("webjars-test"),
        deployFork = optBoolean("deploy.fork").getOrElse(false),
        fileServiceUrl = optString("webjars.file-service.url").getOrElse("https://webjars-file-service.herokuapp.com"),
        mavenCentralLimit = optInt("mavencentral.limit"),
        devMode = sys.env.get("APP_MODE").contains("dev") || !sys.env.contains("PORT"),
      )
    }
  }
