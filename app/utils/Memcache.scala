package utils

import java.time.{LocalDateTime, ZoneOffset}
import java.util.concurrent.TimeUnit

import javax.inject.{Inject, Singleton}
import net.spy.memcached.{AddrUtil, ConnectionFactoryBuilder, MemcachedClient}
import net.spy.memcached.auth.AuthDescriptor
import net.spy.memcached.internal.{GetFuture, OperationFuture}
import net.spy.memcached.ops.StatusCode
import net.spy.memcached.transcoders.Transcoder
import play.api.Configuration
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

@Singleton
class Memcache @Inject() (configuration: Configuration, lifecycle: ApplicationLifecycle) (implicit ec: ExecutionContext) {

  import Memcache._

  private lazy val instance: MemcachedClient = {
    System.setProperty("net.spy.log.LoggerImpl", "net.spy.memcached.compat.log.SLF4JLogger")

    val maybeUsername = configuration.getOptional[String]("memcached.username")
    val maybePassword = configuration.getOptional[String]("memcached.password")

    val baseConnectionFactoryBuilder = new ConnectionFactoryBuilder().setProtocol(ConnectionFactoryBuilder.Protocol.BINARY)


    val connectionFactoryBuilder = (maybeUsername, maybePassword) match {
      case (Some(username), Some(password)) =>
        val authDescriptor = AuthDescriptor.typical(username, password)
        baseConnectionFactoryBuilder.setAuthDescriptor(authDescriptor)
      case _ =>
        baseConnectionFactoryBuilder
    }

    val connectionFactory = connectionFactoryBuilder.build()

    val addresses = AddrUtil.getAddresses(configuration.get[String]("memcached.servers"))

    val memcachedClient = new MemcachedClient(connectionFactory, addresses)

    lifecycle.addStopHook(() => Future.fromTry(Try(memcachedClient.shutdown())))

    memcachedClient
  }

  // todo: not sure how DRY these two

  def operationFutureToScalaFuture[A](operationFuture: OperationFuture[A]): Future[A] = {
    val promise = Promise[A]
    // todo: is this racey if the getFuture is already complete?
    operationFuture.addListener { gettableFuture =>
      if (gettableFuture.getStatus.isSuccess)
        promise.complete(Try(gettableFuture.get().asInstanceOf[A]))
      else
        promise.failure(new Throwable(gettableFuture.getStatus.getMessage))
    }

    promise.future
  }

  def getFutureToScalaFuture[A](getFuture: GetFuture[A]): Future[A] = {
    val promise = Promise[A]
    // todo: is this racey if the getFuture is already complete?
    getFuture.addListener { gettableFuture =>
      if (gettableFuture.getStatus.isSuccess)
        promise.complete(Try(gettableFuture.get().asInstanceOf[A]))
      else
        if (gettableFuture.getStatus.getStatusCode == StatusCode.ERR_NOT_FOUND)
          promise.failure(Miss)
        else
          promise.failure(new Throwable(gettableFuture.getStatus.getMessage))
    }

    promise.future
  }

  def get[A](cacheKey: String)(implicit transcoder: Transcoder[A]): Future[A] = {
    getFutureToScalaFuture(instance.asyncGet(cacheKey, transcoder))
  }

  def getWithMiss[A](cacheKey: String)(miss: => Future[A])(implicit transcoder: Transcoder[A]): Future[A] = {
    get(cacheKey).recoverWith {
      case Miss =>
        for {
          value <- miss
          _ <- set(cacheKey, value)
        } yield value
    }
  }

  def set[A](cacheKey: String, value: A, expiration: Expiration = Expiration.Inf)(implicit transcoder: Transcoder[A]): Future[Unit] = {
    Expiration.toMemcache(expiration).flatMap { exp =>
      operationFutureToScalaFuture(instance.set(cacheKey, exp, value, transcoder)).filter(_ == true).map(_ => ())
    }
  }

}

object Memcache {
  case object Miss extends Exception

  sealed trait Expiration
  object Expiration {
    case object Inf extends Expiration
    case class In(duration: Duration) extends Expiration
    case class On(dateTime: LocalDateTime) extends Expiration

    @scala.annotation.tailrec
    def toMemcache(expiration: Expiration): Future[Int] = {
      expiration match {
        case Expiration.Inf =>
          Future.successful(0)
        case Expiration.In(duration) =>
          if (duration.gt(30.days))
            toMemcache(On(LocalDateTime.now().plusSeconds(duration.toSeconds)))
          else if (duration.lt(1.second))
            Future.failed(new Exception("Expiration can't be less than 1 second"))
          else
            Future.successful(duration.toSeconds.toInt)
        case Expiration.On(dateTime) =>
          if (dateTime.isBefore(LocalDateTime.now().plusDays(30)))
            toMemcache(In(Duration(java.time.Duration.between(LocalDateTime.now(), dateTime).getSeconds, TimeUnit.SECONDS)))
          else
            Future.successful(dateTime.toEpochSecond(ZoneOffset.UTC).toInt)
      }
    }
  }
}
