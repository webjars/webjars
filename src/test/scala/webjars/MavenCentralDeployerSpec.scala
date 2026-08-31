package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.MavenCentralDeployerLive
import zio.*
import zio.compress.ZipUnarchiver
import zio.stream.ZStream
import zio.test.*

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.HexFormat

object MavenCentralDeployerSpec extends ZIOSpecDefault:

  private val signature = Chunk.fromArray("signature".getBytes(StandardCharsets.US_ASCII))

  private object FakeSigner extends MavenCentral.Signer:
    def ascSign(toSign: Chunk[Byte]): IO[Throwable, Option[Chunk[Byte]]] = ZIO.some(signature)
    override def newSession: IO[Throwable, MavenCentral.Signer.Session] =
      ZIO.succeed(new MavenCentral.Signer.Session:
        def update(bytes: Chunk[Byte]): IO[Throwable, Unit] = ZIO.unit
        def finish: IO[Throwable, Chunk[Byte]] = ZIO.succeed(signature)
      )

  private def hex(algorithm: String, bytes: Chunk[Byte]): String =
    val digest = MessageDigest.getInstance(algorithm)
    bytes.foreach(digest.update)
    HexFormat.of().formatHex(digest.digest())

  def spec = suite("MavenCentralDeployer")(
    test("streams the jar once and appends hashes and signature after it"):
      for
        pulls <- Ref.make(0)
        jarBytes = Chunk.fromArray("jar-bytes".getBytes(StandardCharsets.US_ASCII))
        jar = ZStream.fromZIO(pulls.update(_ + 1)).drain ++ ZStream.fromChunk(jarBytes)
        gav = MavenCentral.gav("org.example", "demo", "1.0.0")
        deployer = MavenCentralDeployerLive(FakeSigner)
        bundle = deployer.bundle(
          gav,
          jar,
          "<pom/>",
          None,
          Chunk.fromArray("pom-sha1".getBytes(StandardCharsets.US_ASCII)),
          Chunk.fromArray("pom-md5".getBytes(StandardCharsets.US_ASCII)),
        )
        before <- pulls.get
        entries <- bundle
          .via(ZipUnarchiver.unarchive)
          .mapZIO { case (entry, content) => content.runCollect.map(entry.name -> _) }
          .runCollect
        after <- pulls.get
        byName = entries.toMap
        jarName = byName.keys.find(_.endsWith(".jar"))
        sha1Name = byName.keys.find(_.endsWith(".jar.sha1"))
        md5Name = byName.keys.find(_.endsWith(".jar.md5"))
        ascName = byName.keys.find(_.endsWith(".jar.asc"))
      yield assertTrue(
        before == 0,
        after == 1,
        jarName.flatMap(byName.get).contains(jarBytes),
        sha1Name.flatMap(byName.get).exists(bytes => String(bytes.toArray, StandardCharsets.US_ASCII) == hex("SHA-1", jarBytes)),
        md5Name.flatMap(byName.get).exists(bytes => String(bytes.toArray, StandardCharsets.US_ASCII) == hex("MD5", jarBytes)),
        ascName.flatMap(byName.get).contains(signature),
      )
  )
