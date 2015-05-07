package utils

import play.api.libs.concurrent.Akka
import play.api.mvc.WithFilters
import play.api.{Application, Logger, Play}
import play.filters.gzip.GzipFilter
import shade.memcached.{AuthConfiguration, Configuration, Memcached}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Global extends WithFilters(new GzipFilter(1000 * 1024)) {

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


  override def onStart(app: Application): Unit = {
    super.onStart(app)

    Akka.system(app).scheduler.schedule(Duration.Zero, 1.minute, new Runnable {
      override def run(): Unit = Logger.info("Free Mem: " + Runtime.getRuntime.freeMemory() + " Max Mem: " + Runtime.getRuntime.maxMemory() + " Total Mem: " + Runtime.getRuntime.totalMemory())
    })
  }

  override def onStop(app: Application): Unit = {
    memcached.close()
    super.onStop(app)
  }
}
