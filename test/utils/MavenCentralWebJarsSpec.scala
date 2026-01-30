package utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.GroupId
import models.WebJar
import org.apache.pekko.util.Timeout
import play.api.test.*
import utils.Adapter.runToFuture
import zio.http.Client

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.xml.Elem

class MavenCentralWebJarsSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  "refresh" should {
    "work" in {
      val mavenCentralWebJars = application.injector.instanceOf[MavenCentralWebJarsLive]
      val valkey = application.injector.instanceOf[Valkey]

      val refresh = mavenCentralWebJars.refreshGroup(MavenCentral.GroupId("org.webjars"), updateNumFiles = false)

      val refreshRefresh = refresh.zip(refresh)

      val (refresh1, refresh2) = await(refreshRefresh.runToFuture(Client.default.orDie ++ valkey.layer)) // use the same layer for both refreshes

      refresh1.size should beEqualTo(mavenCentralWebJars.maybeLimit.get)
      refresh2.size should beEqualTo(0)

      val featuredWebJars = await(mavenCentralWebJars.featuredWebJars(MavenCentral.GroupId("org.webjars"), 2))
      featuredWebJars.size should beEqualTo(2)

      val searchWebJars = await(mavenCentralWebJars.searchWebJars(MavenCentral.GroupId("org.webjars"), "a"))
      searchWebJars.size should beEqualTo(5)
    }
  }

}

class MavenCentralWebJarsMock extends MavenCentralWebJars:
  override def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem] =
    Future.failed(FileNotFoundException("no mock pom"))

  override def featuredWebJars(groupId: GroupId, limit: Port): Future[Seq[WebJar]] =
    Future.successful(Seq.empty)

  override def searchWebJars(groupId: GroupId, query: String): Future[Seq[WebJar]] =
    Future.successful(Seq.empty)
