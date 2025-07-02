package utils

import models.WebJar
import org.apache.pekko.util.Timeout
import play.api.test._
import utils.MavenCentral.{GAV, GroupId}

import java.io.FileNotFoundException
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.xml.Elem

// todo: there is some brittle stuff here (maven central search, oss stats, memcache, maven central itself)
//  and we really should better handle integration tests which are super tricky and faked service tests
class MavenCentralSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

  val limit = 5

  class WithApp extends WithApplication(_.configure("mavencentral.limit" -> limit)) // limit how many subgroups and artifacts are fetched

  "fetchWebJars" should {
    "work for npm" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      val npm = app.injector.instanceOf[NPM]
      val webJars = await(mavenCentral.fetchWebJars(npm.groupId))
      webJars.forall(_.groupId == "org.webjars.npm") should beTrue
      webJars.size should beEqualTo(limit)
      webJars.foldLeft(0)(_ + _.versions.size) should beGreaterThan (0)
    }
  }

  "artifactIds" should {
    "does not include artifact versions in artifacts" in new WithApplication() { // no limit
      val mavenCentral = app.injector.instanceOf[MavenCentralLive]
      val artifactIds = await(mavenCentral.artifactIds("org.webjars.npm"))
      artifactIds.contains("1.3.26") must beFalse
      artifactIds.size must beGreaterThan(5000)
      artifactIds.contains("github-com-sindresorhus-copy-text-to-clipboard") must beTrue // long name
    }
  }

  "getNumFiles" should {
    "return none for a nonexistant webjar" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentralLive]

      await(mavenCentral.getNumFiles(GAV("org.webjars", "es6-promise-parent", "4.2.8"))) must beNone
    }
  }

}

class MavenCentralMock extends MavenCentral {
  override def fetchWebJars(groupId: GroupId): Future[Set[WebJar]] = {
    Future.successful(Set.empty)
  }

  // this makes it so the mock says the artifact has not already been deployed
  override def fetchPom(gav: GAV, maybeUrlPrefix: Option[String]): Future[Elem] = {
    Future.failed(new FileNotFoundException())
  }

  override def webJars(groupId: GroupId): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }
}
