package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import com.roundeights.hasher.Implicits.*
import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.Path

import java.io.ByteArrayOutputStream
import java.util.zip.{ZipEntry, ZipOutputStream}

/** Parameterized on its env type so the mock can be `MavenCentralDeployer[Any]`
 *  (no env) while the production impl is `MavenCentralDeployer[Sonatype]`.
 *  Consumers depend on `MavenCentralDeployer[A]` and propagate `A` through
 *  their own env requirements. Pattern borrowed from skillsjars `Deployer[Env]`. */
trait MavenCentralDeployer[Env]:
  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[Env, Throwable, Unit]

  /** Sign with the OSS GPG key. Returns `None` to indicate the signer is
   *  unavailable in this environment (typically the mock without a key in
   *  the env) — callers can choose to skip the `.asc` files in that case. */
  def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]]

/** Live impl — delegates signing to [[MavenCentral.Signer]] (built once at
 *  layer construction time from `oss.gpg-*` config). */
case class MavenCentralDeployerLive(signer: MavenCentral.Signer) extends MavenCentralDeployer[MavenCentral.Deploy.Sonatype]:

  override def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]] =
    signer.ascSign(toSign)

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

  def publish(gav: MavenCentral.GroupArtifactVersion, jar: Array[Byte], pom: String): ZIO[MavenCentral.Deploy.Sonatype, Throwable, Unit] =
    for
      pomAscOpt <- ascSign(Chunk.fromArray(pom.getBytes))
      jarAscOpt <- ascSign(Chunk.fromArray(jar))
      baseFiles  = Map(
                     s"${gav.path}.jar"      -> jar,
                     s"${gav.path}.jar.sha1" -> jar.sha1.hex.getBytes,
                     s"${gav.path}.jar.md5"  -> jar.md5.hex.getBytes,
                     s"${gav.path}.pom"      -> pom.getBytes,
                     s"${gav.path}.pom.sha1" -> pom.sha1.hex.getBytes,
                     s"${gav.path}.pom.md5"  -> pom.md5.hex.getBytes,
                   )
      ascFiles   = pomAscOpt.map(asc => s"${gav.path}.pom.asc" -> asc.toArray).toMap ++
                     jarAscOpt.map(asc => s"${gav.path}.jar.asc" -> asc.toArray).toMap
      zip        = createZip(baseFiles ++ ascFiles)
      name       = s"${gav.groupId}${gav.artifactId}-${gav.version}.zip"
      _         <- ZIO.logInfo(s"Deploying $gav")
      _         <- MavenCentral.Deploy.uploadVerifyAndPublish(name, zip)
    yield ()

object MavenCentralDeployer:

  /** Validates `oss.gpg-key` at layer construction time and delegates the
   *  actual key parsing to [[MavenCentral.Signer.make]]. If either step
   *  fails the layer build fails so the app refuses to start rather than
   *  failing at deploy time. */
  val live: ZLayer[AppConfig, Throwable, MavenCentralDeployer[MavenCentral.Deploy.Sonatype]] =
    ZLayer.fromZIO(ZIO.service[AppConfig]).flatMap { env =>
      val config = env.get[AppConfig]
      config.ossGpgKey match
        case None =>
          ZLayer.fail(IllegalStateException("Required config 'oss.gpg-key' is not set"))
        case Some(key) =>
          MavenCentral.Signer.make(key, config.ossGpgPass) >>>
            ZLayer.fromZIO(ZIO.service[MavenCentral.Signer].map(MavenCentralDeployerLive(_)))
    }
