package utils

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import shade.memcached.{AuthConfiguration, Memcached}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class Memcache @Inject() (configuration: Configuration, lifecycle: ApplicationLifecycle) (implicit ec: ExecutionContext) {

  lazy val instance = {
    val maybeAuthConfig = for {
      username <- configuration.getString("memcached.username")
      password <- configuration.getString("memcached.password")
    } yield AuthConfiguration(username, password)

    Memcached(shade.memcached.Configuration(configuration.getString("memcached.servers").get, maybeAuthConfig))
  }

  lifecycle.addStopHook(() => Future.fromTry(Try(instance.close())))

}
