package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.roundeights.hasher.Implicits.*
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKeyRing, PGPSignature, PGPSignatureGenerator}
import webjars.config.AppConfig
import zio.*
import zio.http.{Client, Path}

import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}

trait MavenCentralDeployer:
  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Any, Throwable, Unit]

case class MavenCentralDeployerLive(config: AppConfig) extends MavenCentralDeployer:

  private def deployLayer(username: String, password: String): ZLayer[Client, Nothing, MavenCentral.Deploy.Sonatype] =
    MavenCentral.Deploy.Sonatype.fromCredentials(username, password)

  private[webjars] def asc(toSign: Array[Byte]): String =
    val gpgKey = config.ossGpgKey.getOrElse(throw IllegalStateException("Required config 'oss.gpg-key' is not set"))
    val gpgPass = config.ossGpgPass.getOrElse(throw IllegalStateException("Required config 'oss.gpg-pass' is not set"))
    val keyBytes = Base64.getDecoder.decode(gpgKey)
    val secretKeyRing = new PGPSecretKeyRing(keyBytes, new JcaKeyFingerprintCalculator())
    val pass = gpgPass.toCharArray
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
    for
      username <- ZIO.fromOption(config.ossDeployUsername).orElseFail(IllegalStateException("Required config 'oss.deploy.username' is not set"))
      password <- ZIO.fromOption(config.ossDeployPassword).orElseFail(IllegalStateException("Required config 'oss.deploy.password' is not set"))
      _ <- ZIO.fromOption(config.ossGpgKey).orElseFail(IllegalStateException("Required config 'oss.gpg-key' is not set"))
      _ <- ZIO.fromOption(config.ossGpgPass).orElseFail(IllegalStateException("Required config 'oss.gpg-pass' is not set"))
      _ <- ZIO.attempt(println(s"Deploying $gav"))
      pomAsc = asc(pom.getBytes)
      jarAsc = asc(jar)
      files = Map(
        s"${gav.path}.jar"      -> jar,
        s"${gav.path}.jar.sha1" -> jar.sha1.hex.getBytes,
        s"${gav.path}.jar.md5"  -> jar.md5.hex.getBytes,
        s"${gav.path}.jar.asc"  -> jarAsc.getBytes,

        s"${gav.path}.pom"      -> pom.getBytes,
        s"${gav.path}.pom.sha1" -> pom.sha1.hex.getBytes,
        s"${gav.path}.pom.md5"  -> pom.md5.hex.getBytes,
        s"${gav.path}.pom.asc"  -> pomAsc.getBytes,
      )
      zip = createZip(files)
      name = s"${gav.groupId}${gav.artifactId}-${gav.version}.zip"
      _ <- MavenCentral.Deploy.uploadVerifyAndPublish(name, zip).provide(Client.default.orDie >>> deployLayer(username, password))
    yield ()

object MavenCentralDeployer:
  val live: ZLayer[AppConfig, Nothing, MavenCentralDeployer] = ZLayer.derive[MavenCentralDeployerLive]
