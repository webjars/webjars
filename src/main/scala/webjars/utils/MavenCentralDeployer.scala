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

  private val deployLayer: ZLayer[Client, Nothing, MavenCentral.Deploy.Sonatype] =
    MavenCentral.Deploy.Sonatype.fromCredentials(config.ossDeployUsername, config.ossDeployPassword)

  private[webjars] def asc(toSign: Array[Byte]): String =
    val keyBytes = Base64.getDecoder.decode(config.ossGpgKey)
    val secretKeyRing = new PGPSecretKeyRing(keyBytes, new JcaKeyFingerprintCalculator())
    val pass = config.ossGpgPass.toCharArray
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
    println(s"Deploying $gav")
    val pomAsc = asc(pom.getBytes)
    val jarAsc = asc(jar)
    val files = Map(
      s"${gav.path}.jar"      -> jar,
      s"${gav.path}.jar.sha1" -> jar.sha1.hex.getBytes,
      s"${gav.path}.jar.md5"  -> jar.md5.hex.getBytes,
      s"${gav.path}.jar.asc"  -> jarAsc.getBytes,

      s"${gav.path}.pom"      -> pom.getBytes,
      s"${gav.path}.pom.sha1" -> pom.sha1.hex.getBytes,
      s"${gav.path}.pom.md5"  -> pom.md5.hex.getBytes,
      s"${gav.path}.pom.asc"  -> pomAsc.getBytes,
    )

    val zip = createZip(files)
    val name = s"${gav.groupId}${gav.artifactId}-${gav.version}.zip"

    MavenCentral.Deploy.uploadVerifyAndPublish(name, zip).provide(Client.default.orDie >>> deployLayer)

object MavenCentralDeployer:
  val live: ZLayer[AppConfig, Nothing, MavenCentralDeployer] = ZLayer.derive[MavenCentralDeployerLive]
