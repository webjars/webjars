package utils

import play.api.Play
import play.api.mvc.WithFilters
import play.filters.gzip.GzipFilter
import shade.memcached.{AuthConfiguration, Configuration, Memcached}

import scala.concurrent.ExecutionContext.global

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

    Memcached(Configuration(Play.current.configuration.getString("memcached.servers").get, authConfig), global)
  }

}
