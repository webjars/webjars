package utils

import play.api.libs.concurrent.Akka
import play.api.mvc.WithFilters
import play.api.{Application, Logger, Play}
import play.filters.gzip.GzipFilter
import shade.memcached.{AuthConfiguration, Configuration, Memcached}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class Global extends WithFilters(new GzipFilter(1000 * 1024)) {

  lazy val memcached = {

    val maybeAuthConfig = for {
      username <- Play.current.configuration.getString("memcached.username")
      password <- Play.current.configuration.getString("memcached.password")
    } yield AuthConfiguration(username, password)

    implicit val memcachedDispatcher = Akka.system(Play.current).dispatchers.lookup("memcached.dispatcher")

    Memcached(Configuration(Play.current.configuration.getString("memcached.servers").get, maybeAuthConfig))
  }

  override def onStart(app: Application): Unit = {
    super.onStart(app)

    Akka.system(app).scheduler.schedule(Duration.Zero, 1.minute) {
      Logger.info("Free Mem: " + Runtime.getRuntime.freeMemory() + " Max Mem: " + Runtime.getRuntime.maxMemory() + " Total Mem: " + Runtime.getRuntime.totalMemory())
    } (ExecutionContext.global)
  }

  override def onStop(app: Application): Unit = {
    memcached.close()
    super.onStop(app)
  }

}

object Global extends Global