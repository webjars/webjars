package webjars

import com.jamesward.zio_http_guard.CrawlerLimiter
import com.jamesward.zio_mavencentral.MavenCentral
import com.jamesward.zio_mavencentral.MavenCentral.{ArtifactId, GroupArtifactVersion, GroupId, Version}
import webjars.config.AppConfig
import webjars.utils.*
import zio.*
import zio.direct.*
import zio.http.Client
import zio.redis.Redis
import zio.stream.ZStream

object Deploy extends ZIOAppDefault:

  // -- pure helpers (unit-tested in DeployHelpersSpec) ----------------------

  /** Parse `org.webjars:jquery-ui:1.14.2` (or with a leading `v` on the
   *  version) into a `GroupArtifactVersion`. Returns `None` on any other
   *  shape. The version is taken verbatim — `vless`-stripping happens
   *  downstream in `Deployable.releaseVersion`. */
  def parseGav(s: String): Option[GroupArtifactVersion] =
    s.split(':') match
      case Array(g, a, v) if g.nonEmpty && a.nonEmpty && v.nonEmpty =>
        Some(GroupArtifactVersion(GroupId(g), ArtifactId(a), Version(v)))
      case _ =>
        None

  /** Given the upstream version we want to re-publish and the set of
   *  versions already on Maven Central for the artifact, return the next
   *  release version to publish under.
   *
   *   - If the upstream version isn't on Maven Central yet, publish it
   *     unchanged.
   *   - Otherwise, pick the smallest `<upstream>+N` (N ≥ 1) that isn't
   *     already published. We scan all `<upstream>+M` entries (only those
   *     where `M` parses as a positive integer) and use `max + 1`,
   *     defaulting to `+1` when none are found.
   *
   *  SemVer 2.0 build metadata (the `+N` suffix) doesn't affect precedence,
   *  so consumers still resolve "latest 1.14.2" to the highest `+N`. */
  def nextReleaseVersion(upstreamVersion: String, existingVersions: Set[String]): String =
    val upstream = upstreamVersion.stripPrefix("v")
    if !existingVersions.contains(upstream) then upstream
    else
      val plusPrefix = s"$upstream+"
      val maxN = existingVersions.iterator
        .collect { case v if v.startsWith(plusPrefix) => v.stripPrefix(plusPrefix) }
        .flatMap(_.toIntOption)
        .filter(_ >= 1)
        .maxOption
        .getOrElse(0)
      s"$upstream+${maxN + 1}"

  // -- orchestration --------------------------------------------------------

  /** Query Maven Central for the existing versions of an artifact. A
   *  not-found response (artifact has never been published) collapses to
   *  an empty set so the caller can publish the upstream version unchanged. */
  def existingVersions(groupId: GroupId, artifactId: ArtifactId): ZIO[Client, Throwable, Set[String]] =
    MavenCentral.searchVersions(groupId, artifactId)
      .map(_.value.iterator.map(_.toString).toSet)
      .catchAll {
        case _: MavenCentral.GroupIdOrArtifactIdNotFoundError => ZIO.succeed(Set.empty[String])
        case t: Throwable                                     => ZIO.fail(t)
      }

  /** Stream the redeploy of a GAV: resolve a `Deployable` for the groupId,
   *  derive the next release version from the existing versions, and hand
   *  off to `DeployWebJar.deploy` which builds + publishes the artifact.
   *
   *  Tests pass `existingVersions` directly so they don't hit Maven Central. */
  def streamRedeploy[Env](
    gav: GroupArtifactVersion,
    allDeployables: AllDeployables,
    deployWebJar: DeployWebJar[Env],
    existingVersions: Set[String],
  ): ZStream[Scope & Client & Redis & Env, Throwable, String] =
    allDeployables.fromGroupId(gav.groupId) match
      case None =>
        ZStream.fail(new IllegalArgumentException(
          s"groupId '${gav.groupId}' is not a known deployable (known: ${allDeployables.groupIds().mkString(", ")})"
        ))
      case Some(deployable) =>
        // Keep the user-supplied version as the upstream — `Deployable.archive`
        // for GitHub-based artifacts expects the literal tag (e.g. `v5.15.1`),
        // and `nextReleaseVersion` strips a leading `v` internally before
        // matching against the existing-versions set on Maven Central.
        val upstream = gav.version.toString
        val release = nextReleaseVersion(upstream, existingVersions)
        ZStream(s"Redeploying ${gav.groupId}:${gav.artifactId}: upstream=$upstream → release=$release") ++
          deployWebJar.deploy(deployable, gav.artifactId.toString, upstream, Some(release))

  // -- ZIOAppDefault runtime ------------------------------------------------

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = Logging.bootstrap

  /** Pulls the GAV from the live Maven Central catalog (so we don't have to
   *  carry around a separate "what's already published" cache) and runs
   *  the full streaming redeploy, logging each progress message. Fails the
   *  app with a non-zero exit if the deploy stream errors. */
  private def runRedeploy(gav: GroupArtifactVersion): ZIO[Scope & Client & Redis & AllDeployables & DeployWebJar[MavenCentral.Deploy.Sonatype] & MavenCentral.Deploy.Sonatype, Throwable, Unit] =
    defer:
      val allDeployables = ZIO.service[AllDeployables].run
      val deployWebJar   = ZIO.service[DeployWebJar[MavenCentral.Deploy.Sonatype]].run
      val existing       = existingVersions(gav.groupId, gav.artifactId).run
      streamRedeploy(gav, allDeployables, deployWebJar, existing)
        .tap(msg => ZIO.logInfo(msg))
        .runDrain
        .run

  def run: ZIO[ZIOAppArgs & Scope, Any, Any] =
    val parseArgs: ZIO[ZIOAppArgs, IllegalArgumentException, GroupArtifactVersion] =
      for
        args   <- ZIOAppArgs.getArgs
        gavStr <- ZIO.fromOption(args.headOption).orElseFail(
                    new IllegalArgumentException("Usage: deploy <groupId>:<artifactId>:<version>")
                  )
        gav    <- ZIO.fromOption(parseGav(gavStr)).orElseFail(
                    new IllegalArgumentException(s"Invalid GAV (expected groupId:artifactId:version): $gavStr")
                  )
      yield gav

    // Build heavy layers (Sonatype + GPG signer) AFTER arg parsing succeeds —
    // otherwise an invalid invocation fails on missing OSS / GPG config rather
    // than printing the actual usage error.
    parseArgs.flatMap { gav =>
      runRedeploy(gav).provideSome[Scope](
        AppConfig.live,
        Client.default,
        Cache.live,
        Valkey.live,
        Git.live,
        GitHub.live,
        SemVer.live,
        Maven.live,
        LicenseDetector.live,
        SourceLocator.live,
        WebJarsFileService.live,
        NPM.live,
        Classic.live,
        AllDeployables.live,
        MavenCentralDeployer.live,
        MavenCentral.Deploy.Sonatype.Live,
        MavenCentralWebJars.live,
        DeployWebJar.live[MavenCentral.Deploy.Sonatype],
        SearchIndex.live,
      )
    }
