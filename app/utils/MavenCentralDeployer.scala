package utils

import com.google.inject.ImplementedBy
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.Deploy
import com.roundeights.hasher.Implicits.*
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import play.api.{Configuration, Logging}
import utils.Adapter.*
import zio.http.{Client, Path}
import zio.{ZIO, ZLayer}

import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future
import scala.language.postfixOps

@ImplementedBy(classOf[MavenCentralDeployerLive])
trait MavenCentralDeployer {
  def maybeOssDeployUsername(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.username").filter(_.nonEmpty)
  def maybeOssDeployPassword(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.deploy.password").filter(_.nonEmpty)
  def maybeOssGpgKey(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-key").filter(_.nonEmpty)
  def maybeOssGpgPass(configuration: Configuration): Option[String] = configuration.getOptional[String]("oss.gpg-pass").filter(_.nonEmpty)

  def asc(toSign: Array[Byte]): Option[String]

  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): Future[Unit]
}

@Singleton
class MavenCentralDeployerLive @Inject() (configuration: Configuration) extends MavenCentralDeployer with Logging {

  private lazy val disableDeploy = configuration.getOptional[Boolean]("oss.disable-deploy").getOrElse(false)

  val deployLayer: ZLayer[Client, Nothing, MavenCentral.Deploy.Sonatype] =
    ZLayer.fromZIO:
      for
        username <- ZIO.fromOption(maybeOssDeployUsername(configuration)).orDieWith(_ => RuntimeException("deploy username not set"))
        password <- ZIO.fromOption(maybeOssDeployPassword(configuration)).orDieWith(_ => RuntimeException("deploy password not set"))
        client   <- ZIO.serviceWith[Client](MavenCentral.Deploy.Sonatype.clientMiddleware(username, password))
      yield
        MavenCentral.Deploy.Sonatype(client)


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

  extension (gav: MavenCentral.GroupArtifactVersion)
    def path: Path =
      MavenCentral.artifactPath(gav.groupId, Some(MavenCentral.ArtifactAndVersion(gav.artifactId, Some(gav.version)))) / s"${gav.artifactId}-${gav.version}"

  override def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): Future[Unit] =
    println(s"Deploying $gav")
    val maybeDeploy =
      for
        pomAsc <- asc(pom.getBytes)
        jarAsc <- asc(jar)
      yield
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

        val name = s"${gav.groupId}${gav.artifactId}-${gav.version}.zip"

        if disableDeploy then
          Deploy.upload(name, zip).unit.runToFuture(Client.default.orDie >>> deployLayer)
        else
          Deploy.uploadVerifyAndPublish(name, zip).runToFuture(Client.default.orDie >>> deployLayer)

    maybeDeploy.getOrElse(Future.failed(RuntimeException("Unable to deploy to Maven Central")))
}

