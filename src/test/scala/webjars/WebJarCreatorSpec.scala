package webjars

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.WebJarCreator
import zio.*
import zio.compress.*
import zio.stream.*
import zio.test.*

import java.net.URI
import java.util.zip.GZIPInputStream

object WebJarCreatorSpec extends ZIOSpecDefault:

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
  )
