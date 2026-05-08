package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.Deploy
import com.roundeights.hasher.Implicits.*
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.{Client, Path}

import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}

trait MavenCentralDeployer:
  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit]

case class MavenCentralDeployerLive(config: AppConfig) extends MavenCentralDeployer:

  private lazy val disableDeploy = config.ossDisableDeploy

  private val deployLayer: ZLayer[Client, Nothing, MavenCentral.Deploy.Sonatype] =
    ZLayer.fromZIO:
      defer:
        val username = ZIO.fromOption(config.ossDeployUsername).orDieWith(_ => RuntimeException("deploy username not set")).run
        val password = ZIO.fromOption(config.ossDeployPassword).orDieWith(_ => RuntimeException("deploy password not set")).run
        val client = ZIO.serviceWith[Client](MavenCentral.Deploy.Sonatype.clientMiddleware(username, password)).run
        MavenCentral.Deploy.Sonatype(client)

  private[webjars] def asc(toSign: Array[Byte]): Option[String] =
    config.ossGpgKey.map { gpgKey =>
      val keyBytes = Base64.getDecoder.decode(gpgKey)
      val secretKeyRing = new PGPSecretKeyRing(keyBytes, new JcaKeyFingerprintCalculator())
      val pass = config.ossGpgPass.getOrElse("").toCharArray
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

  private def createZip(files: Map[String, Array[Byte]]): Array[Byte] =
    val baos = new ByteArrayOutputStream()
    val zos = new ZipOutputStream(baos)
    files.foreach { case (filename, content) =>
      zos.putNextEntry(new ZipEntry(filename))
      zos.write(content)
      zos.closeEntry()
    }
    zos.close()
    baos.toByteArray

  extension (gav: MavenCentral.GroupArtifactVersion)
    private def path: Path =
      MavenCentral.artifactPath(gav.groupId, Some(MavenCentral.ArtifactAndVersion(gav.artifactId, Some(gav.version)))) / s"${gav.artifactId}-${gav.version}"

  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit] =
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
          Deploy.upload(name, zip).unit.provide(Client.default.orDie >>> deployLayer)
        else
          Deploy.uploadVerifyAndPublish(name, zip).provide(Client.default.orDie >>> deployLayer)

    maybeDeploy.getOrElse(ZIO.fail(RuntimeException("Unable to deploy to Maven Central")))

object MavenCentralDeployer:
  val live: ZLayer[AppConfig, Nothing, MavenCentralDeployer] = ZLayer.derive[MavenCentralDeployerLive]
