package utils

import play.api.{Application, Play}
import play.api.libs.concurrent.Akka
import play.api.mvc.WithFilters
import play.filters.gzip.GzipFilter
import shade.memcached.{AuthConfiguration, Configuration, Memcached}

object Global extends WithFilters(new GzipFilter()) {

  lazy val memcached = {

    Play.current.configuration

    val maybeUsernamePassword = for {
      username <- Play.current.configuration.getString("memcached.username")
      password <- Play.current.configuration.getString("memcached.password")
    } yield (username, password)

    val baseConfig = Configuration(Play.current.configuration.getString("memcached.servers").get)

    val authConfig = maybeUsernamePassword.map {
      case (username, password) =>
        AuthConfiguration(username, password)
    }

    val memcachedDispatcher = Akka.system(Play.current).dispatchers.lookup("memcached.dispatcher")

    Memcached(Configuration(Play.current.configuration.getString("memcached.servers").get, authConfig), memcachedDispatcher)
  }

  override def onStop(app: Application): Unit = {
    memcached.close()
    super.onStop(app)
  }
}
