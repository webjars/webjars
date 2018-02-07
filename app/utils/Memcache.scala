package utils

import javax.inject.{Inject, Singleton}

import play.api.Configuration
import play.api.inject.ApplicationLifecycle
import shade.memcached.{AuthConfiguration, Memcached}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class Memcache @Inject() (configuration: Configuration, lifecycle: ApplicationLifecycle) (implicit ec: ExecutionContext) {

  lazy val instance: Memcached = {
    val maybeAuthConfig = for {
      username <- configuration.getOptional[String]("memcached.username")
      password <- configuration.getOptional[String]("memcached.password")
    } yield AuthConfiguration(username, password)

    val memcached = Memcached(shade.memcached.Configuration(addresses = configuration.get[String]("memcached.servers"), authentication = maybeAuthConfig, operationTimeout = 30.seconds))

    lifecycle.addStopHook(() => Future.fromTry(Try(memcached.close())))

    memcached
  }

}
