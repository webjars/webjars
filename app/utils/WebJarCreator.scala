package utils

import org.apache.commons.compress.archivers.jar.JarArchiveOutputStream
import org.apache.commons.compress.archivers.zip.ZipArchiveEntry
import org.apache.commons.compress.archivers.{ArchiveEntry, ArchiveInputStream, ArchiveStreamFactory}
import org.apache.commons.io.IOUtils
import org.eclipse.jgit.ignore.IgnoreNode
import play.api.Logging

import java.io._
import java.nio.file.{Files, Path, Paths}
import java.util.spi.ToolProvider

object WebJarCreator extends Logging {

  private def createDir(path: Path, jar: JarArchiveOutputStream): Unit = {
    // make sure the dir ends with a "/"
    val formattedDir = path.toString.stripSuffix("/") + "/"
    val ze = new ZipArchiveEntry(formattedDir)
    jar.putArchiveEntry(ze)
    jar.closeArchiveEntry()
  }

  private def createDirs(dir: String, jar: JarArchiveOutputStream): Unit = {
    val paths = Iterator.iterate(Paths.get(dir))(_.getParent).takeWhile(_ != null).toSeq.reverse
    paths.foreach(createDir(_, jar))
  }

  private def createFileEntry(path: String, jar: JarArchiveOutputStream, contents: String): Unit = {
    val ze = new ZipArchiveEntry(path)
    jar.putArchiveEntry(ze)
    jar.write(contents.getBytes)
    jar.closeArchiveEntry()
  }

  private def createFileEntry(path: String, jar: JarArchiveOutputStream, contents: Path): Unit = {
    val ze = new ZipArchiveEntry(path)
    jar.putArchiveEntry(ze)
    jar.write(Files.readAllBytes(contents))
    jar.closeArchiveEntry()
  }

  def isExcluded(excludes: Set[String], name: String, isDirectory: Boolean): Boolean = {
    val ignoreNode = new IgnoreNode()
    val excludesInputStream = new ByteArrayInputStream(excludes.mkString("\n").getBytes)
    ignoreNode.parse(excludesInputStream)
    ignoreNode.isIgnored(name, isDirectory) match {
      case IgnoreNode.MatchResult.IGNORED => true
      case IgnoreNode.MatchResult.NOT_IGNORED => false
      case IgnoreNode.MatchResult.CHECK_PARENT | IgnoreNode.MatchResult.CHECK_PARENT_NEGATE_FIRST_MATCH =>
        val parent = name.split("/").dropRight(1).mkString("/")
        if (parent == "")
          false
        else
          isExcluded(excludes, parent, true)
    }
  }

  def removeGlobPath(glob: String, path: String): Option[String] = {
    val globParts = glob.split('/')
    val pathParts = path.split('/')

    // to match the glob, the path must have at more parts than the glob
    // this excludes the root
    if (pathParts.length <= globParts.length) {
      None
    }
    else {
      val parts = globParts.zip(pathParts)

      val removeGlob = parts.forall { case (g, s) =>
        g == "*" || g == s
      }

      Option.when(removeGlob) {
        val reassembled = pathParts.drop(globParts.length).mkString("/")
        if (path.endsWith("/")) {
          reassembled + "/"
        }
        else {
          reassembled
        }
      }
    }
  }

  def makeOpenModule(moduleName: String): Option[Path] = {
    val normalizedModuleName = moduleName.replaceAll("[^a-zA-Z0-9.]", ".")
    val moduleContents = s"open module $normalizedModuleName {}"
    val dir = Files.createTempDirectory("module-temp")
    val file = Path.of(dir.toString, "module-info.java")
    Files.writeString(file, moduleContents)
    val javac = ToolProvider.findFirst("javac").orElseThrow
    val exitCode = javac.run(System.out, System.err, "--release", "9", file.toString)
    Option.when(exitCode == 0)(Path.of(dir.toString, "module-info.class"))
  }

  def createWebJar[E <: ArchiveEntry](in: InputStream, maybeBaseDirGlob: Option[String], exclude: Set[String], pom: String, webJarName: String, licenses: Set[License], groupId: String, artifactId: String, version: String, pathPrefix: String): Array[Byte] = {
    require(groupId.nonEmpty)
    require(artifactId.nonEmpty)

    val byteArrayOutputStream = new ByteArrayOutputStream()

    val bufferedByteArrayOutputStream = new BufferedOutputStream(byteArrayOutputStream)

    val jar = new ArchiveStreamFactory().createArchiveOutputStream[JarArchiveOutputStream](ArchiveStreamFactory.JAR, bufferedByteArrayOutputStream)

    val bufferedInputStream = new BufferedInputStream(in)

    val archive = new ArchiveStreamFactory().createArchiveInputStream[ArchiveInputStream[E]](bufferedInputStream)

    val webJarPrefix = s"META-INF/resources/webjars/$pathPrefix"

    createDirs(webJarPrefix, jar)

    createFileEntry(s"META-INF/maven/$groupId/$artifactId/pom.xml", jar, pom)

    // https://docs.osgi.org/specification/osgi.core/7.0.0/framework.module.html
    val manifestLicense = licenses.flatMap { license =>
      (license.maybeName, license.maybeUrl) match {
        case (Some(name), Some(url)) =>
          Some(s"$name; $url")
        case (Some(name), None) =>
          Some(name)
        case (None, Some(url)) =>
          Some(url)
        case _ =>
          None
      }
    }.mkString(",")

    val manifest =
      s"""Manifest-Version: 1.0
         |Bundle-Description: WebJar for $webJarName
         |Bundle-License: $manifestLicense
         |Bundle-SymbolicName: $groupId.$artifactId
         |Bundle-Name: $webJarName
         |Bundle-Version: $version
         |Bundle-ManifestVersion: 2
         |Build-Jdk-Spec: 1.8
         |Multi-Release: true
         |Created-By: webjars.org
         |""".stripMargin

    createFileEntry("META-INF/MANIFEST.MF", jar, manifest)

    makeOpenModule(s"$groupId.$artifactId").fold(
      logger.error("Could not build module-info.class")
    ) { openModule =>
      createFileEntry("META-INF/versions/9/module-info.class", jar, openModule)
    }

    val properties = s"""
        |#Generated by WebJar Sync
        |version=$version
        |groupId=$groupId
        |artifactId=$artifactId
       """.stripMargin

    createFileEntry(s"META-INF/maven/$groupId/$artifactId/pom.properties", jar, properties)

    // todo: if the globber doesn't match on any files, likely there is a bug and we should produce an error
    // copy zip to jar
    LazyList.continually(archive.getNextEntry).takeWhile(_ != null).foreach { ze =>
      val maybeName = maybeBaseDirGlob.fold[Option[String]](Some(ze.getName)) { baseDirGlob =>
        removeGlobPath(baseDirGlob, ze.getName)
      }

      maybeName.foreach { name =>
        if (!isExcluded(exclude, name, ze.isDirectory)) {
          val path = webJarPrefix + name
          val nze = new ZipArchiveEntry(path)
          jar.putArchiveEntry(nze)
          if (!ze.isDirectory) {
            IOUtils.copy(archive, jar)
          }
          jar.closeArchiveEntry()
        }
      }
    }

    archive.close()
    bufferedInputStream.close()
    in.close()

    jar.close()
    bufferedByteArrayOutputStream.close()
    byteArrayOutputStream.close()

    byteArrayOutputStream.toByteArray
  }

}
