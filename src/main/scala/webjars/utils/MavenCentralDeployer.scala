package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.utils.ArchiveCreator.ArchiveFile
import webjars.utils.Deployable.ArchiveStream
import zio.*
import zio.direct.*
import zio.http.Path
import zio.stream.*

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

/** Parameterized on its env type so the mock can be `MavenCentralDeployer[Any]`
 *  (no env) while the production impl is `MavenCentralDeployer[Sonatype]`.
 *  Consumers depend on `MavenCentralDeployer[A]` and propagate `A` through
 *  their own env requirements. Pattern borrowed from skillsjars `Deployer[Env]`. */
trait MavenCentralDeployer[Env]:
  def publish(gav: MavenCentral.GroupArtifactVersion, jar: ArchiveStream, pom: String): ZIO[Env, Throwable, Unit]

  /** Sign with the OSS GPG key. Returns `None` to indicate the signer is
   *  unavailable in this environment (typically the mock without a key in
   *  the env) — callers can choose to skip the `.asc` files in that case. */
  def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]]

/** Live impl — delegates signing to [[MavenCentral.Signer]] (built once at
 *  layer construction time from `oss.gpg-*` config). */
case class MavenCentralDeployerLive(signer: MavenCentral.Signer) extends MavenCentralDeployer[MavenCentral.Deploy.Sonatype]:

  override def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]] =
    signer.ascSign(toSign)

  extension (gav: MavenCentral.GroupArtifactVersion)
    private def path: Path =
      MavenCentral.artifactPath(gav.groupId, Some(MavenCentral.ArtifactAndVersion(gav.artifactId, Some(gav.version)))) / s"${gav.artifactId}-${gav.version}"

  private def bytesFile(name: String, bytes: Chunk[Byte]): ArchiveFile =
    ArchiveFile(name, ZStream.fromChunk(bytes), Some(bytes.length.toLong))

  private def checksum(algorithm: String, bytes: Chunk[Byte]): Task[Chunk[Byte]] =
    ZIO.attempt:
      val digest = MessageDigest.getInstance(algorithm)
      bytes.foreach(digest.update)
      Chunk.fromArray(HexFormat.of().formatHex(digest.digest()).getBytes(StandardCharsets.US_ASCII))

  private[webjars] def bundle(
    gav: MavenCentral.GroupArtifactVersion,
    jar: ArchiveStream,
    pom: String,
    pomAsc: Option[Chunk[Byte]],
    pomSha1: Chunk[Byte],
    pomMd5: Chunk[Byte],
  ): ArchiveStream =
    ZStream.unwrap:
      for
        jarSigner <- signer.newSession
        jarSha1   <- ZIO.attempt(MessageDigest.getInstance("SHA-1"))
        jarMd5    <- ZIO.attempt(MessageDigest.getInstance("MD5"))
      yield
        val pomBytes = Chunk.fromArray(pom.getBytes(StandardCharsets.UTF_8))
        val prefix = List(
          bytesFile(s"${gav.path}.pom", pomBytes),
          bytesFile(s"${gav.path}.pom.sha1", pomSha1),
          bytesFile(s"${gav.path}.pom.md5", pomMd5),
        ) ++ pomAsc.map(bytesFile(s"${gav.path}.pom.asc", _))

        val observedJar = jar.tapChunks: chunk =>
          ZIO.attempt:
            chunk.foreach: byte =>
              jarSha1.update(byte)
              jarMd5.update(byte)
          *> jarSigner.update(chunk)

        val jarEntry = ArchiveFile(s"${gav.path}.jar", observedJar)

        val suffix = ZStream.unwrap:
          for
            signature <- jarSigner.finish
            sha1 <- ZIO.attempt:
              Chunk.fromArray(HexFormat.of().formatHex(jarSha1.digest()).getBytes(StandardCharsets.US_ASCII))
            md5 <- ZIO.attempt:
              Chunk.fromArray(HexFormat.of().formatHex(jarMd5.digest()).getBytes(StandardCharsets.US_ASCII))
          yield ZStream(
            bytesFile(s"${gav.path}.jar.sha1", sha1),
            bytesFile(s"${gav.path}.jar.md5", md5),
            bytesFile(s"${gav.path}.jar.asc", signature),
          )

        ArchiveCreator.archive(ZStream.fromIterable(prefix) ++ ZStream(jarEntry) ++ suffix)

  def publish(gav: MavenCentral.GroupArtifactVersion, jar: ArchiveStream, pom: String): ZIO[MavenCentral.Deploy.Sonatype, Throwable, Unit] =
    defer:
      val pomBytes = ZIO.succeed(Chunk.fromArray(pom.getBytes(StandardCharsets.UTF_8))).run
      val pomAsc = ascSign(pomBytes).run
      val pomSha1 = checksum("SHA-1", pomBytes).run
      val pomMd5 = checksum("MD5", pomBytes).run
      val zip = bundle(gav, jar, pom, pomAsc, pomSha1, pomMd5)
      val name = s"${gav.groupId}${gav.artifactId}-${gav.version}.zip"
      ZIO.logInfo(s"Deploying $gav").run
      MavenCentral.Deploy.uploadVerifyAndPublish(name, zip).run

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
