package webjars.utils

import org.eclipse.jgit.api.Git as GitApi
import org.eclipse.jgit.api.ResetCommand.ResetType
import webjars.utils.Deployable.Version
import zio.*
import zio.direct.*
import zio.http.*

import java.io.{File, InputStream}
import java.nio.charset.CodingErrorAction
import java.nio.file.{Files, Path}
import scala.io.{Codec, Source}
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

trait Git:
  def isGit(packageNameOrGitRepo: String): Boolean
  def resolveRedirect(httpUrl: String): ZIO[Scope, Throwable, String]
  def gitUrl(gitRepo: String): ZIO[Scope, Throwable, String]
  def artifactId(nameOrUrlish: String): ZIO[Scope, Throwable, String]
  def versions(gitRepo: String): ZIO[Scope, Throwable, Set[String]]
  def versionsOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, Seq[String]]
  def cloneOrCheckout(gitRepo: String, version: Version, retry: Boolean = true): ZIO[Scope, Throwable, File]
  def file(uri: URL, version: Version, fileName: String): ZIO[Scope, Throwable, String]
  def file(gitRepo: String, tagCommitOrBranch: Version, fileName: String): ZIO[Scope, Throwable, String]
  def tar(gitRepo: String, version: Version, excludes: Set[String]): ZIO[Scope, Throwable, InputStream]

case class GitLive(client: Client) extends Git:

  private def deleteRecursively(file: File): Unit =
    if file.isDirectory then
      file.listFiles().foreach(deleteRecursively)
    file.delete()

  val cacheDir: Path = Files.createTempDirectory("git")
  cacheDir.toFile.deleteOnExit()

  def isGit(packageNameOrGitRepo: String): Boolean =
    packageNameOrGitRepo.contains("/") && !packageNameOrGitRepo.startsWith("@")

  def resolveRedirect(httpUrl: String): ZIO[Scope, Throwable, String] =
    defer:
      val url = URL.unsafeParse(httpUrl)
      val response = client.batched(Request.get(url)).run
      response.status match
        case Status.MovedPermanently | Status.Found =>
          response.header(Header.Location) match
            case Some(location) =>
              val redirUrl = location.url.encode
              val resolved = if redirUrl.startsWith("/") then
                val parsed = URL.unsafeParse(httpUrl)
                parsed.scheme.fold("")(_.encode) + "://" + parsed.host.getOrElse("") + redirUrl
              else
                redirUrl
              resolveRedirect(resolved).run
            case None =>
              ZIO.fail(new Exception("Could not get redir location")).run
        case Status.Ok =>
          httpUrl
        case _ =>
          ZIO.fail(new Exception(s"Could not get HEAD for url: $httpUrl")).run

  def gitUrl(gitRepo: String): ZIO[Scope, Throwable, String] =
    val resolvedUrl = if gitRepo.contains("://") then
      gitRepo.replace("git://", "https://")
    else if gitRepo.contains("github:") then
      gitRepo.replace("github:", "https://github.com/")
    else
      s"https://github.com/$gitRepo"

    val jgitReadyUrl = resolvedUrl.replace("git+", "")

    if jgitReadyUrl.startsWith("http") then
      resolveRedirect(jgitReadyUrl)
    else
      ZIO.succeed(jgitReadyUrl)

  def artifactId(nameOrUrlish: String): ZIO[Scope, Throwable, String] =
    defer:
      if isGit(nameOrUrlish) then
        val resolved = gitUrl(nameOrUrlish).run
        val url = ZIO.fromTry(URL.parseTry(resolved.stripSuffix(".git"))).run
        val host = url.host.getOrElse("").replaceAll("\\W", "-")
        val path = url.path.encode.replaceAll("\\W", "-")
        host + path
      else
        nameOrUrlish.replace("@", "").replace("/", "__")

  def versions(gitRepo: String): ZIO[Scope, Throwable, Set[String]] =
    defer:
      val url = gitUrl(gitRepo).run
      ZIO.fromTry(Try {
        val tags = GitApi.lsRemoteRepository()
          .setRemote(url)
          .setTags(true)
          .call()
        tags.asScala.map(_.getName.stripPrefix("refs/tags/")).toSet
      }).run

  def versionsOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, Seq[String]] =
    defer:
      gitUrl(gitRepo).run
      val baseDir = cloneOrCheckout(gitRepo, s"origin/$branch").run
      ZIO.fromTry(Try {
        val commits = GitApi.open(baseDir).log().call()
        commits.asScala.toSeq.map(_.getId.abbreviate(10).name())
      }).run

  def cloneOrCheckout(gitRepo: String, version: Version, retry: Boolean = true): ZIO[Scope, Throwable, File] =
    gitUrl(gitRepo).flatMap { url =>
      val baseDir = new File(cacheDir.toFile, url)

      def cloneOrPull: ZIO[Any, Throwable, File] =
        if !baseDir.exists() then
          ZIO.fromTry(Try {
            GitApi.cloneRepository()
              .setURI(url).setDirectory(baseDir)
              .setCloneAllBranches(true).setNoCheckout(true)
              .call()
            baseDir
          })
        else
          ZIO.fromTry(Try {
            GitApi.open(baseDir).fetch().setRemote("origin").call()
            baseDir
          })

      def checkout: ZIO[Any, Throwable, File] =
        defer:
          cloneOrPull.run
          val ref = ZIO.fromTry(Try {
            val found = GitApi.open(baseDir).getRepository.findRef(version)
            if found != null then found.getName else version
          }).run
          ZIO.fromTry(Try {
            GitApi.open(baseDir).reset().setMode(ResetType.HARD).setRef(ref).call()
            baseDir
          }).run

      checkout.catchAll { t =>
        ZIO.succeed(deleteRecursively(baseDir)) *>
          (if retry then cloneOrCheckout(gitRepo, version, retry = false)
           else ZIO.fail(t))
      }
    }

  def file(uri: URL, version: Version, fileName: String): ZIO[Scope, Throwable, String] =
    file(uri.encode, version, fileName)

  def file(gitRepo: String, tagCommitOrBranch: Version, fileName: String): ZIO[Scope, Throwable, String] =
    defer:
      val baseDir = cloneOrCheckout(gitRepo, tagCommitOrBranch).run
      val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
      ZIO.fromTry(Using(Source.fromFile(new File(baseDir, fileName))(decoder))(_.mkString)).run

  def tar(gitRepo: String, version: Version, excludes: Set[String]): ZIO[Scope, Throwable, InputStream] =
    defer:
      val baseDir = cloneOrCheckout(gitRepo, version).run
      val resolvedExcludes = excludes.map(new File(baseDir, _))
      val allExcludes = Set(new File(baseDir, ".git")) ++ resolvedExcludes
      ArchiveCreator.tarDir(baseDir, allExcludes).run

object Git:
  val live: ZLayer[Client, Nothing, Git] = ZLayer.derive[GitLive]
