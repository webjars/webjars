package utils

import com.lumidion.sonatype.central.client.core.{CheckStatusResponse, DeploymentId, DeploymentName, DeploymentState}
import io.lemonlabs.uri.AbsoluteUrl
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

import java.security.{KeyPairGenerator, Security}
import java.util.{Base64, Date}
import scala.concurrent.duration._

// todo: there is some brittle stuff here (maven central search, oss stats, memcache, maven central itself)
//  and we really should better handle integration tests which are super tricky and faked service tests
class MavenCentralDeployerSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 300.seconds

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
    "asc" in new WithAppAndKey {
      val mavenCentralDeployer = app.injector.instanceOf[MavenCentralDeployer]
      mavenCentralDeployer.asc("foo".getBytes) must beSome
    }
    "upload" in new WithApplication {
      val mavenCentralDeployer = app.injector.instanceOf[MavenCentralDeployer]
      if (
        mavenCentralDeployer.maybeOssDeployUsername(app.configuration).isEmpty ||
          mavenCentralDeployer.maybeOssDeployPassword(app.configuration).isEmpty ||
          mavenCentralDeployer.maybeOssGpgKey(app.configuration).isEmpty
      ) {
        skipped("skipped due to missing config")
      }
      else {
        val mavenCentralDeployer = app.injector.instanceOf[MavenCentralDeployer]
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

        val (_, checker) = mavenCentralDeployer.upload(gav, jar, pom).get

        await(mavenCentralDeployer.waitForDeploymentState(DeploymentState.VALIDATED, checker)(app.injector.instanceOf[Futures], app.injector.instanceOf[ActorSystem]))

//        mavenCentral.publish(deploymentId).get

//        await(mavenCentral.waitForDeploymentState(DeploymentState.PUBLISHING, checker)(app.injector.instanceOf[Futures], app.injector.instanceOf[ActorSystem]))

        success
      }
    }
  }

}

class MavenCentralDeployerMock extends MavenCentralDeployer {
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
