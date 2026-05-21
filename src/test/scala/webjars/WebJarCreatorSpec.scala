package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.WebJarCreator
import zio.*
import zio.compress.*
import zio.stream.*
import zio.test.*

import java.net.URI
import java.nio.file.FileSystems
import java.util.zip.GZIPInputStream

object WebJarCreatorSpec extends ZIOSpecDefault:

  // Build a synthetic tar archive in-memory from (name, content) pairs using
  // the zio-compress TarArchiver pipeline.
  private def buildTar(entries: Seq[(String, String)]): ZIO[Any, Throwable, Chunk[Byte]] =
    ZStream
      .fromIterable(entries.map { case (name, content) =>
        val bytes = Chunk.fromArray(content.getBytes)
        val entry: ArchiveEntry[Some, Any] = ArchiveEntry(
          name = name,
          uncompressedSize = Some(bytes.length.toLong),
        )
        (entry, ZStream.fromChunk(bytes))
      })
      .via(TarArchiver.archive)
      .runCollect

  def spec = suite("WebJarCreator")(
    test("isExcluded with **") {
      val excludes = Set("**/foo")
      assertTrue(
        WebJarCreator.isExcluded(excludes, "foo", true),
        !WebJarCreator.isExcluded(excludes, "test", false),
        !WebJarCreator.isExcluded(excludes, "test/test", false),
        WebJarCreator.isExcluded(excludes, "foo/test", false),
        WebJarCreator.isExcluded(excludes, "foo/foo", true),
        WebJarCreator.isExcluded(excludes, "foo/foo/test", false),
        WebJarCreator.isExcluded(excludes, "foo/foo/foo", true),
      )
    },
    test("isExcluded with *") {
      val excludes = Set("*.foo")
      assertTrue(
        WebJarCreator.isExcluded(excludes, "asdf.foo", false),
        WebJarCreator.isExcluded(excludes, "foo/adsf.foo", false),
        !WebJarCreator.isExcluded(excludes, "foo/test", false),
      )
    },
    test("isExcluded with ** and *") {
      val excludes = Set("**/.*")
      assertTrue(
        WebJarCreator.isExcluded(excludes, ".foo", false),
        WebJarCreator.isExcluded(excludes, "foo/.foo", false),
        !WebJarCreator.isExcluded(excludes, "foo/test", false),
      )
    },
    test("removeGlobPath") {
      assertTrue(
        WebJarCreator.removeGlobPath("*", "asdf/foo").contains("foo"),
        WebJarCreator.removeGlobPath("*/", "asdf/foo").contains("foo"),
        WebJarCreator.removeGlobPath("asdf/", "asdf/").isEmpty,
        WebJarCreator.removeGlobPath("asdf/", "asdf/foo").contains("foo"),
        WebJarCreator.removeGlobPath("zxcv/", "asdf/foo").isEmpty,
        WebJarCreator.removeGlobPath("*/dist/", "asdf/dist/foo").contains("foo"),
        WebJarCreator.removeGlobPath("asdf/dist", "asdf/dist/foo").contains("foo"),
        WebJarCreator.removeGlobPath("asdf/dist/", "asdf/dist/foo").contains("foo"),
        WebJarCreator.removeGlobPath("zxcv/dist/", "asdf/dist/foo").isEmpty,
        WebJarCreator.removeGlobPath("*/dist", "asdf/").isEmpty,
        WebJarCreator.removeGlobPath("*/dist", "asdf/dist/asdf/foo").contains("asdf/foo"),
        WebJarCreator.removeGlobPath("*/dist", "asdf/dist/asdf/foo/").contains("asdf/foo/"),
        WebJarCreator.removeGlobPath("*/", "Select-2.0.4/js/dataTables.select.min.js").contains("js/dataTables.select.min.js"),
      )
    },
    test("create a WebJar from a tgz") {
      ZIO.attemptBlocking {
        val url = new URI("https://registry.npmjs.org/npm/-/npm-2.10.0.tgz").toURL
        val inputStream = url.openConnection().getInputStream
        new GZIPInputStream(inputStream).readAllBytes()
      }.flatMap { bytes =>
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), Some("*/"), Set("node_modules"), "test", "Test", Set.empty, MavenCentral.GroupId("test"), MavenCentral.ArtifactId("test"), MavenCentral.Version("2.10.0"), "test").map { webJar =>
          assertTrue(webJar.length > 0)
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(120)),
    test("deal with different tgz base dirs") {
      ZIO.attemptBlocking {
        val url = new URI("https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz").toURL
        val inputStream = url.openConnection().getInputStream
        new GZIPInputStream(inputStream).readAllBytes()
      }.flatMap { bytes =>
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), Some("*/"), Set("node_modules"), "", "Test", Set.empty, MavenCentral.GroupId("org.webjars.npm"), MavenCentral.ArtifactId("react-redux"), MavenCentral.Version("4.4.32"), "react-redux/4.4.32/").flatMap { webJar =>
          ZStream.fromChunk(Chunk.fromArray(webJar))
            .via(ZipUnarchiver.unarchive)
            .map(_._1.name)
            .runCollect
            .map { allNames =>
              assertTrue(allNames.contains("META-INF/resources/webjars/react-redux/4.4.32/package.json"))
            }
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(60)),
    test("handle packages where the contents are in the base dir") {
      ZIO.attemptBlocking {
        val url = new URI("https://registry.npmjs.org/@types/react-redux/-/react-redux-4.4.32.tgz").toURL
        val inputStream = url.openConnection().getInputStream
        new GZIPInputStream(inputStream).readAllBytes()
      }.flatMap { bytes =>
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), None, Set("node_modules"), "", "Test", Set.empty, MavenCentral.GroupId("org.webjars.npm"), MavenCentral.ArtifactId("react-redux"), MavenCentral.Version("4.4.32"), "react-redux/4.4.32/").flatMap { webJar =>
          ZStream.fromChunk(Chunk.fromArray(webJar))
            .via(ZipUnarchiver.unarchive)
            .map(_._1.name)
            .runCollect
            .map { allNames =>
              assertTrue(allNames.contains("META-INF/resources/webjars/react-redux/4.4.32/react-redux/package.json"))
            }
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(60)),
    test("create subdirectories for contents") {
      ZIO.attemptBlocking {
        val url = new URI("https://registry.npmjs.org/@types/react-router/-/react-router-2.0.41.tgz").toURL
        val inputStream = url.openConnection().getInputStream
        new GZIPInputStream(inputStream).readAllBytes()
      }.flatMap { bytes =>
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), Some("*/"), Set("node_modules"), "", "Test", Set.empty, MavenCentral.GroupId("org.webjars.npm"), MavenCentral.ArtifactId("react-router"), MavenCentral.Version("2.0.41"), "react-router/2.0.41/").flatMap { webJar =>
          ZStream.fromChunk(Chunk.fromArray(webJar))
            .via(ZipUnarchiver.unarchive)
            .filter(_._1.name == "META-INF/resources/webjars/react-router/2.0.41/lib/")
            .map(_._1)
            .runHead
            .map { maybeLib =>
              assertTrue(maybeLib.exists(_.isDirectory))
            }
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(60)),
    test("handle non gzip tgzs") {
      ZIO.attemptBlocking {
        val url = new URI("https://registry.npmjs.org/@types/escodegen/-/escodegen-0.0.2.tgz").toURL
        url.openConnection().getInputStream.readAllBytes()
      }.flatMap { bytes =>
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), Some("*/"), Set("node_modules"), "", "Test", Set.empty, MavenCentral.GroupId("org.webjars.npm"), MavenCentral.ArtifactId("escodegen"), MavenCentral.Version("0.0.2"), "escodegen/0.0.2/").flatMap { webJar =>
          ZStream.fromChunk(Chunk.fromArray(webJar))
            .via(ZipUnarchiver.unarchive)
            .map(_._1.name)
            .runCollect
            .map { allNames =>
              assertTrue(allNames.contains("META-INF/resources/webjars/escodegen/0.0.2/package.json"))
            }
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(60)),
    test("multi-base") {
      ZIO.attemptBlocking {
        val url = new URI("https://github.com/moment/moment/archive/2.30.1.zip").toURL
        url.openConnection().getInputStream.readAllBytes()
      }.flatMap { bytes =>
        val baseDir = Some("*/dist,*/min")
        WebJarCreator.createWebJar(ZStream.fromChunk(Chunk.fromArray(bytes)), baseDir, Set.empty, "", "Test", Set.empty, MavenCentral.GroupId(""), MavenCentral.ArtifactId(""), MavenCentral.Version(""), "moment/").flatMap { webJar =>
          ZStream.fromChunk(Chunk.fromArray(webJar))
            .via(ZipUnarchiver.unarchive)
            .map(_._1.name)
            .runCollect
            .map { allNames =>
              assertTrue(
                allNames.contains("META-INF/resources/webjars/moment/moment.min.js"),
                allNames.contains("META-INF/resources/webjars/moment/moment.js"),
              )
            }
        }
      }
    } @@ TestAspect.timeout(zio.Duration.fromSeconds(120)),
    test("normalizes `./` path segments and dedupes (issue #2220)") {
      // Mirrors the malformed `hookable-5.5.3` npm tarball, which contains both
      // `package/./dist/index.cjs` and `package/dist/index.cjs`. Without
      // normalization, the resulting webjar contains entries like
      // `META-INF/resources/webjars/test/1.0.0/./dist/index.cjs`, which causes
      // JDK ZipFileSystem to reject the jar (JDK-8283486). The duplicates also
      // make the zip ambiguous to consumers.
      val cjsContent = "module.exports = {};"
      val mjsContent = "export default {};"
      val pkgJson = """{"name":"test","version":"1.0.0"}"""

      buildTar(
        Seq(
          "package/./dist/index.cjs" -> cjsContent,
          "package/dist/index.cjs" -> cjsContent,
          "package/package.json" -> pkgJson,
          "package/dist/index.mjs" -> mjsContent,
        )
      ).flatMap { tarBytes =>
        WebJarCreator
          .createWebJar(
            ZStream.fromChunk(tarBytes),
            Some("*/"),
            Set.empty,
            "<pom/>",
            "Test",
            Set.empty,
            MavenCentral.GroupId("org.webjars.npm"),
            MavenCentral.ArtifactId("test"),
            MavenCentral.Version("1.0.0"),
            "test/1.0.0/",
          )
          .flatMap { webJar =>
            ZStream
              .fromChunk(Chunk.fromArray(webJar))
              .via(ZipUnarchiver.unarchive)
              .map(_._1.name)
              .runCollect
              .flatMap { allNames =>
                val asList = allNames.toList
                // Spot-check that the JDK can open the jar as a file system
                // (this is the actual symptom reported in issue #2220).
                ZIO.attemptBlocking {
                  val tmp = java.nio.file.Files.createTempFile("webjar-issue-2220-", ".jar")
                  try {
                    java.nio.file.Files.write(tmp, webJar)
                    val uri = new URI("jar:" + tmp.toUri.toString)
                    val fs = FileSystems.newFileSystem(uri, java.util.Collections.emptyMap[String, Any]())
                    fs.close()
                  } finally {
                    java.nio.file.Files.deleteIfExists(tmp)
                  }
                }.as {
                  assertTrue(
                    // No `./` segments anywhere in any entry name
                    asList.forall(name => !name.split('/').contains(".")),
                    // No duplicate entries
                    asList.distinct == asList,
                    // The expected normalized entry is present
                    asList.contains("META-INF/resources/webjars/test/1.0.0/dist/index.cjs"),
                    asList.contains("META-INF/resources/webjars/test/1.0.0/dist/index.mjs"),
                    asList.contains("META-INF/resources/webjars/test/1.0.0/package.json"),
                  )
                }
              }
          }
      }
    },
  )
