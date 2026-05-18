package webjars.utils

import zio.*
import zio.direct.*
import zio.http.URL

trait Maven:
  def convertNpmDependenciesToMaven(dependencies: Map[String, String]): ZIO[Scope, Throwable, Map[String, String]]

case class MavenLive(git: Git, semVer: SemVer) extends Maven:

  def convertNpmDependenciesToMaven(dependencies: Map[String, String]): ZIO[Scope, Throwable, Map[String, String]]  =

    def resolveNameAndVersion(name: String, versionOrUrl: String): ZIO[Scope, Throwable, (String, String)] =
      val urlTry = URL.parseTry(versionOrUrl).filter { url =>
        !url.path.encode.endsWith(".git") && !url.scheme.exists(_.encode == "git")
      }

      urlTry.map { url =>
        resolveTarballDependency(url)
      }.getOrElse {
        if git.isGit(versionOrUrl) then resolveGitDependency(name, versionOrUrl)
        else git.artifactId(name).map(_ -> versionOrUrl)
      }

    def resolveTarballDependency(url: URL): ZIO[Scope, Throwable, (String, String)] =
      if url.path.encode.contains("/tarball/") then
        val host = url.host.getOrElse("").replaceAll("[^\\w\\d]", "-")
        val parts = url.path.encode.split("\\/tarball\\/")
        val path = parts(0).replaceAll("[^\\w\\d]", "-")
        ZIO.succeed((host + path) -> parts(1))
      else
        ZIO.fail(new Exception(s"Could not get a version from the dependency string: ${url.encode}"))

    def resolveGitDependency(name: String, versionOrUrl: String): ZIO[Scope, Throwable, (String, String)] =
      defer:
        val (url, maybeVersion) = ZIO.succeed {
          if versionOrUrl.contains("#") then
            val parts = versionOrUrl.split("#")
            (parts.head, parts.lastOption)
          else
            (versionOrUrl, None)
        }.run

        val artifactId = git.artifactId(url).run.toLowerCase
        maybeVersion match
          case Some(version) => artifactId -> version
          case None =>
            val versions = git.versions(url).run
            versions.headOption match
              case Some(latestVersion) => artifactId -> latestVersion
              case None =>
                val commits = git.versionsOnBranch(url, "master").run
                commits.headOption match
                  case Some(latestCommit) => artifactId -> s"0.0.0-$latestCommit"
                  case None => ZIO.fail(new Exception(s"The dependency definition $name -> $versionOrUrl was not valid because it looked like a git repo reference but no version was specified.")).run

    def resolveVersionRange(artifactId: String, version: String): ZIO[Scope, Throwable, (String, String)] =
      defer:
        if version.contains("#") then
          ZIO.fail(new Exception(s"Invalid version specified in dependency: $artifactId $version")).run
        else
          val maybeRange = semVer.validRange(version).run
          maybeRange match
            case Some(range) => artifactId -> ZIO.fromTry(SemVer.toMaven(range)).run
            case None => artifactId -> version

    val maybeMavenDeps = dependencies.map { case (name, versionOrUrl) =>
      resolveNameAndVersion(name, versionOrUrl).flatMap((resolveVersionRange).tupled)
    }

    ZIO.collectAll(maybeMavenDeps).map(_.toMap)

object Maven:
  val live: ZLayer[Git & SemVer, Nothing, Maven] = ZLayer.derive[MavenLive]
