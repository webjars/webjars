package utils

import com.jamesward.zio_mavencentral.MavenCentral
import models.WebJar
import org.apache.pekko.util.Timeout
import play.api.test.*

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.xml.Elem

// todo: there is some brittle stuff here (maven central search, oss stats, memcache, maven central itself)
//  and we really should better handle integration tests which are super tricky and faked service tests
class MavenCentralWebJarsSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  val limit = 5

  class WithApp extends WithApplication(_.configure("mavencentral.limit" -> limit)) // limit how many subgroups and artifacts are fetched

  "fetchWebJars" should {
    "work for npm" in new WithApp() {
      override def running() = {
        val mavenCentral = app.injector.instanceOf[MavenCentralWebJars]
        val npm = app.injector.instanceOf[NPM]
        val webJars = await(mavenCentral.fetchWebJars(npm.groupId))
        webJars.forall(_.groupId == "org.webjars.npm") should beTrue
        webJars.size should beEqualTo(limit)
        webJars.foldLeft(0)(_ + _.versions.size) should beGreaterThan (0)
      }
    }
  }

  "artifactIds" should {
    "does not include artifact versions in artifacts" in new WithApplication() { // no limit
      override def running() = {
        val mavenCentral = app.injector.instanceOf[MavenCentralWebJarsLive]
        val npm = app.injector.instanceOf[NPM]
        val artifactIds = await(mavenCentral.fetchArtifactIds(npm.groupId))
        artifactIds.items.contains("1.3.26") must beFalse
        artifactIds.items.size must beGreaterThan(5000)
        artifactIds.items.contains("github-com-sindresorhus-copy-text-to-clipboard") must beTrue // long name
      }
    }
  }

  "getNumFiles" should {
    "return none for a nonexistant webjar" in new WithApp() {
      override def running() = {
        val mavenCentral = app.injector.instanceOf[MavenCentralWebJarsLive]
        val gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId("org.webjars"), MavenCentral.ArtifactId("es6-promise-parent"), MavenCentral.Version("4.2.8"))
        await(mavenCentral.getNumFiles(gav)) must beNone
      }
    }
  }

}

class MavenCentralWebJarsMock extends MavenCentralWebJars {
  override def fetchWebJars(groupId: MavenCentral.GroupId): Future[Set[WebJar]] = {
    Future.successful(Set.empty)
  }

  // this makes it so the mock says the artifact has not already been deployed
  override def fetchPom(gav: MavenCentral.GroupArtifactVersion): Future[Elem] = {
    Future.failed(new FileNotFoundException())
  }

  override def webJars(groupId: MavenCentral.GroupId): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }
}
