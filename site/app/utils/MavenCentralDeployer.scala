package utils

import com.google.inject.ImplementedBy
import com.lumidion.sonatype.central.client.core.PublishingType.USER_MANAGED
import com.lumidion.sonatype.central.client.core.*
import com.lumidion.sonatype.central.client.requests.SyncSonatypeClient
import com.roundeights.hasher.Implicits.*
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import org.apache.pekko.actor.*
import org.apache.pekko.pattern.after
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import play.api.libs.concurrent.Futures
import play.api.libs.concurrent.Futures.FutureOps
import play.api.{Configuration, Logging}

import java.io.ByteArrayOutputStream
import java.util.Base64
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.language.postfixOps

@ImplementedBy(classOf[MavenCentralDeployerLive])
trait MavenCentralDeployer {
  import MavenCentral.*

  def maybeOssDeployUsername(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.username").filter(_.nonEmpty)
  def maybeOssDeployPassword(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.password").filter(_.nonEmpty)
  def maybeOssGpgKey(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-key").filter(_.nonEmpty)
  def maybeOssGpgPass(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-pass").filter(_.nonEmpty)

  def asc(toSign: Array[Byte]): Option[String]

  def upload(gav: GAV, jar: Array[Byte], pom: String): Option[(DeploymentId, () => Option[CheckStatusResponse])]
  def publish(deploymentId: DeploymentId): Option[Unit]

  private val maxwait: FiniteDuration = 1.minute
  private val poll: FiniteDuration = 5.seconds

  def waitForDeploymentState(deploymentState: DeploymentState, f: () => Option[CheckStatusResponse])(using futures: Futures, actorSystem: ActorSystem): Future[Unit] = {
    val future = f() match {
      case Some(checkStatusResponse) if checkStatusResponse.deploymentState == deploymentState => Future.successful(())
      case Some(checkStatusResponse) if checkStatusResponse.deploymentState == DeploymentState.FAILED => Future.failed(new IllegalStateException("Failed to deploy"))
      case _ => after(poll)(waitForDeploymentState(deploymentState, f))
    }

    future.withTimeout(maxwait)
  }

}

@Singleton
class MavenCentralDeployerLive @Inject() (configuration: Configuration) extends MavenCentralDeployer with Logging {
  import MavenCentral.*

  private lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)

  val browser: Browser = JsoupBrowser()

  private def withOssDeployCredentials[T](f: (String, String) => Option[T]): Option[T] = {
    for {
      deployUsername <- maybeOssDeployUsername(configuration)
      deployPassword <- maybeOssDeployPassword(configuration)
      result <- f(deployUsername, deployPassword)
    } yield result
  }

  def asc(toSign: Array[Byte]): Option[String] = {
    maybeOssGpgKey(configuration).map { gpgKey =>
      val keyBytes = Base64.getDecoder.decode(gpgKey)

      val secretKeyRing = new PGPSecretKeyRing(keyBytes, new JcaKeyFingerprintCalculator())

      val pass = maybeOssGpgPass(configuration).getOrElse("").toCharArray

      val privKey = secretKeyRing.getSecretKey.extractPrivateKey(new JcePBESecretKeyDecryptorBuilder().build(pass))

      val signerBuilder = new JcaPGPContentSignerBuilder(secretKeyRing.getPublicKey().getAlgorithm, HashAlgorithmTags.SHA1)

      val sGen = new PGPSignatureGenerator(signerBuilder, secretKeyRing.getPublicKey())
      sGen.init(PGPSignature.BINARY_DOCUMENT, privKey)

      val bos = new ByteArrayOutputStream()
      val armor = new ArmoredOutputStream(bos)
      val bOut = new BCPGOutputStream(armor)

      sGen.update(toSign)
      sGen.generate().encode(bOut)

      bOut.close()
      armor.close()
      bos.close()

      new String(bos.toByteArray)
    }
  }

  import java.io.ByteArrayOutputStream
  import java.util.zip.{ZipEntry, ZipOutputStream}

  private def createZip(files: Map[String, Array[Byte]]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(baos)

    files.foreach { case (filename, content) =>
      zos.putNextEntry(new ZipEntry(filename))
      zos.write(content)
      zos.closeEntry()
    }

    zos.close()
    baos.toByteArray
  }

  private def withSonatypeClient[T](f: SyncSonatypeClient => Option[T]): Option[T] = {
    withOssDeployCredentials { (username, password) =>
      val credentials = SonatypeCredentials(username, password)
      val client = new SyncSonatypeClient(credentials)
      f(client)
    }
  }

  override def upload(gav: GAV, jar: Array[Byte], pom: String): Option[(DeploymentId, () => Option[CheckStatusResponse])] = {
    withSonatypeClient { client =>
      for {
        pomAsc <- asc(pom.getBytes)
        jarAsc <- asc(jar)
      } yield {
        val deploymentName = DeploymentName(gav.toString)

        val files = Map(
          s"${gav.path}.jar" -> jar,
          s"${gav.path}.jar.sha1" -> jar.sha1.hex.getBytes,
          s"${gav.path}.jar.md5" -> jar.md5.hex.getBytes,
          s"${gav.path}.jar.asc" -> jarAsc.getBytes,

          s"${gav.path}.pom" -> pom.getBytes,
          s"${gav.path}.pom.sha1" -> pom.sha1.hex.getBytes,
          s"${gav.path}.pom.md5" -> pom.md5.hex.getBytes,
          s"${gav.path}.pom.asc" -> pomAsc.getBytes,
        )

        val zip = createZip(files)

        val deploymentId = client
          .uploadBundleFromBytes(
            zip,
            deploymentName,
            Some(USER_MANAGED)
          )

        deploymentId -> { () => client.checkStatus(deploymentId) }
      }
    }
  }

  override def publish(deploymentId: DeploymentId): Option[Unit] = {
    if (disableDeploy) {
      None
    }
    else {
      withSonatypeClient { client =>
        client.publishValidatedDeployment(deploymentId)
      }
    }
  }
}

