package webjars.utils

import com.jamesward.zio_git.*
import webjars.utils.Deployable.{ArchiveStream, Version}
import webjars.utils.ResilientHttp.batchedResilient
import zio.*
import zio.direct.*
import zio.http.*
import zio.stream.*

import java.nio.charset.StandardCharsets

trait Git:
  def isGit(packageNameOrGitRepo: String): Boolean
  def resolveRedirect(httpUrl: String): ZIO[Scope, Throwable, String]
  def gitUrl(gitRepo: String): ZIO[Scope, Throwable, String]
  def artifactId(nameOrUrlish: String): ZIO[Scope, Throwable, String]
  def versions(gitRepo: String): ZIO[Scope, Throwable, Set[String]]
  def versionsOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, Seq[String]]
  def latestCommitOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, String]
  def file(uri: URL, version: Version, fileName: String): ZIO[Scope, Throwable, String]
  def file(gitRepo: String, tagCommitOrBranch: Version, fileName: String): ZIO[Scope, Throwable, String]
  def archive(gitRepo: String, version: Version, excludes: Set[String]): ArchiveStream

/**
 * Git repository reader backed by [[com.jamesward.zio_git.GitHttp]] — a
 * read-only, no-auth smart-HTTP (`git-upload-pack`) client — rather than jgit.
 *
 * The pure/naming helpers ([[isGit]], [[artifactId]]) and the HTTP-redirect
 * resolution ([[resolveRedirect]], [[gitUrl]]) are unchanged. The repo-reading
 * operations no longer clone to disk:
 *
 *   - [[versions]] lists tags from the ref advertisement.
 *   - [[versionsOnBranch]] fetches the provider's normal history pack; hosted
 *     providers commonly cache it, giving this latency-bounded endpoint better
 *     performance than dynamically generating a filtered pack.
 *   - [[latestCommitOnBranch]] resolves only the branch tip, without history.
 *   - [[file]] resolves the committish (annotated tags are peeled) and reads
 *     the single file from that commit's tree, in memory.
 *   - [[archive]] reads the whole tree at the resolved commit and creates a
 *     deterministic ZIP stream.
 */
case class GitLive(client: Client) extends Git:

  // A GitHttp is just a thin wrapper around the shared zio-http Client, so we
  // build one here rather than requiring it as a constructor arg — keeps the
  // `GitLive(client)` shape the tests and `Git.live` rely on.
  private val gitHttp: GitHttp = GitHttp(client)

  private def toThrowable(error: GitError): Throwable = error match
    case GitError.Transport(cause) => cause
    case other                     => new Exception(other.toString)

  /** Resolve `gitRepo` to a canonical, redirect-followed https URL and parse it
   *  into a zio-git [[RepoUrl]] (the smart-HTTP base). */
  private def repoUrl(gitRepo: String): ZIO[Scope, Throwable, RepoUrl] =
    gitUrl(gitRepo).flatMap: url =>
      ZIO.fromEither(RepoUrl.parse(url)).mapError(e => new Exception(s"Invalid git url: $url ($e)"))

  def isGit(packageNameOrGitRepo: String): Boolean =
    packageNameOrGitRepo.contains("/") && !packageNameOrGitRepo.startsWith("@")

  def resolveRedirect(httpUrl: String): ZIO[Scope, Throwable, String] =
    defer:
      val url = URL.unsafeParse(httpUrl)
      val response = client.batchedResilient(Request.get(url)).run
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

  // Compose the canonical https/git-ready URL form of a git repo reference WITHOUT following
  // any HTTP redirects. Used for naming (artifactId) so that the result is a stable, deterministic
  // function of the input — Maven Central coordinates are immutable, so we cannot let GitHub
  // owner-renames change a webjar's identity after the fact.
  private def composeGitUrl(gitRepo: String): String =
    val resolvedUrl = if gitRepo.contains("://") then
      gitRepo.replace("git://", "https://")
    else if gitRepo.contains("github:") then
      gitRepo.replace("github:", "https://github.com/")
    else
      s"https://github.com/$gitRepo"

    resolvedUrl.replace("git+", "")

  def gitUrl(gitRepo: String): ZIO[Scope, Throwable, String] =
    val readyUrl = composeGitUrl(gitRepo)

    if readyUrl.startsWith("http") then
      resolveRedirect(readyUrl)
    else
      ZIO.succeed(readyUrl)

  def artifactId(nameOrUrlish: String): ZIO[Scope, Throwable, String] =
    defer:
      if isGit(nameOrUrlish) then
        val composed = composeGitUrl(nameOrUrlish)
        val url = ZIO.fromTry(URL.parseTry(composed.stripSuffix(".git"))).run
        val host = url.host.getOrElse("").replaceAll("\\W", "-")
        val path = url.path.encode.replaceAll("\\W", "-")
        host + path
      else
        nameOrUrlish.replace("@", "").replace("/", "__")

  def versions(gitRepo: String): ZIO[Scope, Throwable, Set[String]] =
    defer:
      val repo = repoUrl(gitRepo).run
      gitHttp.tags(repo).mapBoth(toThrowable, _.map(_.name).toSet).run

  def versionsOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, Seq[String]] =
    defer:
      val repo = repoUrl(gitRepo).run
      val commits = gitHttp.fullBranchLog(repo, branch, HistoryFetchMode.ServerDefault).mapError(toThrowable).run
      commits.map(_.id.hex.take(10))

  def latestCommitOnBranch(gitRepo: String, branch: String): ZIO[Scope, Throwable, String] =
    defer:
      val repo = repoUrl(gitRepo).run
      val commit = gitHttp.resolveCommit(repo, Some(branch)).mapError(toThrowable).run
      commit.hex.take(10)

  def file(uri: URL, version: Version, fileName: String): ZIO[Scope, Throwable, String] =
    file(uri.encode, version, fileName)

  def file(gitRepo: String, tagCommitOrBranch: Version, fileName: String): ZIO[Scope, Throwable, String] =
    repoUrl(gitRepo).flatMap: repo =>
      val program =
        for
          commit <- gitHttp.resolveCommittish(repo, tagCommitOrBranch)
          files  <- gitHttp.readFiles(repo, commit)
          bytes  <- ZIO.fromOption(files.get(fileName))
                      .orElseFail(GitError.ObjectNotFound(commit))
        yield String(bytes.toArray, StandardCharsets.UTF_8)
      program.mapError(toThrowable)

  def archive(gitRepo: String, version: Version, excludes: Set[String]): ArchiveStream =
    ZStream.unwrapScoped:
      defer:
        val repo = repoUrl(gitRepo).run
        val commit = gitHttp.resolveCommittish(repo, version).mapError(toThrowable).run
        // zio-git currently resolves the smart-HTTP pack into an in-memory
        // object map before exposing tree files. ArchiveCreator itself streams;
        // this is the remaining repository-source materialization boundary.
        val files = gitHttp.readFiles(repo, commit).mapError(toThrowable).run
        ArchiveCreator.archiveFiles(applyExcludes(files, excludes))

  /** Drop entries whose top-level path segment is an excluded name (e.g.
   *  `node_modules`). Git trees never contain a `.git` entry, so — unlike the
   *  old disk-clone path — there's nothing else to strip. */
  private def applyExcludes(files: Map[String, Chunk[Byte]], excludes: Set[String]): Map[String, Chunk[Byte]] =
    if excludes.isEmpty then files
    else files.filterNot: (path, _) =>
      excludes.exists(ex => path == ex || path.startsWith(s"$ex/"))

object Git:
  val live: ZLayer[Client, Nothing, Git] = ZLayer.derive[GitLive]
