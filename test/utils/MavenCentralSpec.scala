package utils

import com.lumidion.sonatype.central.client.core.{CheckStatusResponse, DeploymentId, DeploymentName, DeploymentState, PublishingType}
import io.lemonlabs.uri.AbsoluteUrl
import models.WebJar
import org.apache.commons.io.IOUtils
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.util.Timeout
import org.bouncycastle.bcpg.{HashAlgorithmTags, PublicKeyAlgorithmTags, PublicKeyPacket, SymmetricKeyAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcaPGPDigestCalculatorProviderBuilder, JcaPGPKeyPair, JcePBESecretKeyEncryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSignature}
import play.api.Environment
import play.api.libs.concurrent.Futures
import play.api.test._
import utils.MavenCentral.{GAV, GroupId}

import java.io.FileNotFoundException
import java.security.{KeyPairGenerator, Security}
import java.time.LocalDateTime
import java.util.{Base64, Date}
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

  "webJarsSorted" should {
    "be ordered correctly" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      if (mavenCentral.maybeOssPassword(app.configuration).isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val classic = app.injector.instanceOf[Classic]
        val webJars = await(mavenCentral.webJarsSorted(Some(classic.groupId), LocalDateTime.of(2019, 1, 1, 1, 1)))
        webJars.map(_.artifactId).take(limit) must beEqualTo(
          List("ace", "acorn", "activity-indicator", "adm-zip", "3rdwavemedia-themes-developer")
        )
      }
    }
  }

  "getStats" should {
    "get the stats for a given date" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      if (mavenCentral.maybeOssPassword(app.configuration).isEmpty) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val classic = app.injector.instanceOf[Classic]
        val statsClassic = await(mavenCentral.getStats(classic.groupId, LocalDateTime.of(2019, 1, 1, 1, 1)))
        statsClassic(("org.webjars", "jquery")) should beEqualTo(193177)
      }
    }
  }

  def generateKey(): String = {
    Security.addProvider(new BouncyCastleProvider())

    val keyPairGenerator = KeyPairGenerator.getInstance("RSA", "BC")
    keyPairGenerator.initialize(2048)
    val keyPair = keyPairGenerator.generateKeyPair()

    val pgpKeyPair = new JcaPGPKeyPair(PublicKeyPacket.VERSION_4, PublicKeyAlgorithmTags.RSA_GENERAL, keyPair, new Date())

    val sha1Calc = new JcaPGPDigestCalculatorProviderBuilder().build().get(HashAlgorithmTags.SHA1)

    val passphrase = "test"
    val identity = "Test User <test@example.com>"

    val signerBuilder = new JcaPGPContentSignerBuilder(pgpKeyPair.getPublicKey.getAlgorithm, HashAlgorithmTags.SHA1)

    val secretKeyEncryptionBuilder = new JcePBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.CAST5, sha1Calc).setProvider("BC").build(passphrase.toArray)

    val secretKey = new PGPSecretKey(PGPSignature.DEFAULT_CERTIFICATION, pgpKeyPair, identity, sha1Calc, null, null, signerBuilder, secretKeyEncryptionBuilder)

    Base64.getEncoder.encodeToString(secretKey.getEncoded)
  }

  class WithAppAndKey extends WithApplication(_.configure("oss.gpg-key" -> generateKey(), "oss.gpg-pass" -> "test"))

  "deploy" should {
    "asc" in new WithAppAndKey() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      mavenCentral.asc("foo".getBytes) must beSome
    }
    "upload" in new WithApp() {
      val mavenCentral = app.injector.instanceOf[MavenCentral]
      if (
        mavenCentral.maybeOssDeployUsername(app.configuration).isEmpty ||
        mavenCentral.maybeOssDeployPassword(app.configuration).isEmpty ||
        mavenCentral.maybeOssGpgKey(app.configuration).isEmpty
      ) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentral = app.injector.instanceOf[MavenCentral]
        val environment = app.injector.instanceOf[Environment]

        val jar = environment.resourceAsStream("foo.jar").map { inputStream =>
          val fileBytes = IOUtils.toByteArray(inputStream)
          inputStream.close()
          fileBytes
        }.get

        val gitUri = AbsoluteUrl.parse("https://githib.com/webjars/webjars.git")
        val sourceUrl = AbsoluteUrl.parse("https://github.com/webjars/webjars")
        val version = "0.0.3" // Instant.now.getEpochSecond.toString
        val licenses = Set[License](LicenseWithNameAndUrl("MIT", AbsoluteUrl.parse("https://opensource.org/licenses/MIT")))

        val packageInfo = PackageInfo("Test WebJar", version, None, gitUri, None, Seq.empty, Map.empty, Map.empty, None)

        val gav = GAV("com.happypathprogramming", "_test", version)

        val pom = templates.xml.pom(gav.groupId, gav.artifactId, gav.version, packageInfo, sourceUrl, Set.empty, Set.empty, licenses).toString()

        val (_, checker) = mavenCentral.upload(gav, jar, pom).get

        await(mavenCentral.waitForDeploymentState(DeploymentState.VALIDATED, checker)(app.injector.instanceOf[Futures], app.injector.instanceOf[ActorSystem]))

//        mavenCentral.publish(deploymentId).get

//        await(mavenCentral.waitForDeploymentState(DeploymentState.PUBLISHING, checker)(app.injector.instanceOf[Futures], app.injector.instanceOf[ActorSystem]))

        success
      }
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

  override def webJarsSorted(maybeGroupId: Option[GroupId], dateTime: LocalDateTime): Future[List[WebJar]] = {
    Future.successful(List.empty)
  }

  override def getStats(groupId: GroupId, dateTime: LocalDateTime): Future[Map[(String, String), Port]] = {
    Future.successful(Map.empty)
  }

  override def asc(toSign: Array[Byte]): Option[String] = {
    None
  }

  // first is in the Validated state, then switches to publishing
  // todo: do it per gav
  override def upload(gav: GAV, jar: Array[Byte], pom: GroupId): Option[(DeploymentId, () => Option[CheckStatusResponse])] = {
    var state: DeploymentState = DeploymentState.VALIDATED

    def checkStatusResponse(): Option[CheckStatusResponse] = {
      val result = Some(CheckStatusResponse(DeploymentId(gav.toString), DeploymentName(gav.toString), state, Vector.empty))
      state = DeploymentState.PUBLISHING
      result
    }

    Some((DeploymentId(gav.toString), checkStatusResponse))
  }

  override def publish(deploymentId: DeploymentId): Option[Unit] =
    Some(())
}
