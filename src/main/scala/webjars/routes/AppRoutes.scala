package webjars.routes

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.config.AppConfig
import webjars.models.WebJar
import webjars.utils.*
import webjars.views.*
import webjars.views.partials.WebJarList
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*
import zio.redis.Redis
import zio.stream.ZStream

import java.io.FileNotFoundException
import java.net.URLDecoder
import scala.util.hashing.MurmurHash3

case class ExistsResponse(
  deployable: Boolean,
  versions: List[String],
  error: Option[String],
)

object ExistsResponse:
  given JsonEncoder[ExistsResponse] = DeriveJsonEncoder.gen[ExistsResponse]

case class AppRoutes[DeployerEnv](
  git: Git,
  cache: Cache,
  mavenCentral: MavenCentralWebJars,
  deployWebJar: DeployWebJar[DeployerEnv],
  deployJobs: DeployJobs[DeployerEnv],
  webJarsFileService: WebJarsFileService,
  config: AppConfig,
  sourceLocator: SourceLocator,
  classic: Classic,
  allDeployables: AllDeployables,
  webJars: WebJars,
  popularMetrics: PopularMetrics,
  popularRanking: PopularRanking,
  searchIndex: SearchIndex,
):

  private val INDEXING_MSG = "Indexing artifacts — try again in a moment."

  private def maybeCached(request: Request, seq: Seq[WebJar])(f: Seq[WebJar] => Response): Response =
    val hash = MurmurHash3.seqHash(seq)
    val etag = "\"" + hash + "\""
    val ifNoneMatch = request.header(Header.IfNoneMatch)
    val etagMatches = ifNoneMatch.exists { inm =>
      inm.renderedValue.contains(etag)
    }
    if etagMatches then
      Response(Status.NotModified)
    else
      f(seq).addHeader(Header.Custom("ETag", etag))

  def allWebJarsData: UIO[List[WebJar]] = searchIndex.snapshot

  // Render a `template2` `Dom` as an HTML 200 response. Equivalent to
  // `Response.html(dom)` but keeps the call sites symmetrical with
  // `jsonResponse(...)` below.
  private def htmlResponse(dom: zio.http.template2.Dom): Response =
    Response.html(dom)

  private def htmlResponse(dom: zio.http.template2.Dom, status: Status): Response =
    Response.html(dom, status)

  private def jsonResponse(json: String): Response =
    Response(
      Status.Ok,
      Headers(Header.ContentType(MediaType.application.json).untyped),
      Body.fromString(json)
    )

  private def acceptsJson(request: Request): Boolean =
    request.header(Header.Accept).exists { accept =>
      accept.renderedValue.contains("application/json")
    }

  private def acceptsEventStream(request: Request): Boolean =
    request.header(Header.Accept).exists { accept =>
      accept.renderedValue.contains("text/event-stream")
    }

  private def movedPermanently(url: String): Response =
    Response(Status.MovedPermanently, Headers(Header.Location(URL.decode(url).toOption.get)))

  private def corsHeaders(response: Response): Response =
    response.addHeader(Header.AccessControlAllowOrigin.All)

  val routes: Routes[Client & Redis & DeployerEnv, Nothing] = Routes(

    // Home page
    Method.GET / "" -> handler { (request: Request) =>
      popularRanking.snapshot.map { popularWebJars =>
        if popularWebJars.isEmpty then
          htmlResponse(IndexPage(webJars, Right(INDEXING_MSG)))
        else
          maybeCached(request, popularWebJars)(wj => htmlResponse(IndexPage(webJars, Left(wj))))
      }
    },

    // Popular WebJars (HTML or JSON)
    Method.GET / "popular" -> handler { (request: Request) =>
      popularRanking.snapshot.map { popularWebJars =>
        if acceptsJson(request) then
          import webjars.models.WebJar.given
          jsonResponse(popularWebJars.toJson)
        else if popularWebJars.isEmpty then
          htmlResponse(WebJarList(Right(INDEXING_MSG)))
        else
          maybeCached(request, popularWebJars)(wj => htmlResponse(WebJarList(Left(wj))))
      }
    },

    // Search WebJars — reads from the in-memory SearchIndex; rejects empty queries.
    Method.GET / "search" -> handler { (request: Request) =>
      val query    = request.url.queryParams.getAll("query").headOption.getOrElse("").trim
      val groupIds = request.url.queryParams.getAll("groupId").toSet

      if query.isEmpty then
        ZIO.succeed(Response(Status.BadRequest, body = Body.fromString("query parameter is required")))
      else
        searchIndex.snapshot.flatMap { all =>
          if all.isEmpty then
            ZIO.succeed {
              if acceptsJson(request) then
                import webjars.models.WebJar.given
                jsonResponse(Seq.empty[WebJar].toJson)
              else
                htmlResponse(WebJarList(Right(INDEXING_MSG)))
            }
          else
            val q = query.toLowerCase
            val filtered = all.filter { wj =>
              (groupIds.isEmpty || groupIds.contains(wj.groupId)) &&
                (wj.artifactId.toLowerCase.contains(q) || wj.name.toLowerCase.contains(q))
            }
            popularMetrics.recordSearchAppearance(
              filtered.map(wj => (MavenCentral.GroupId(wj.groupId), MavenCentral.ArtifactId(wj.artifactId)))
            ) *> ZIO.succeed {
              if acceptsJson(request) then
                import webjars.models.WebJar.given
                jsonResponse(filtered.toJson)
              else
                htmlResponse(WebJarList(Left(filtered)))
            }
        }
    },

    // List by groupId (JSON) — reads from the in-memory SearchIndex.
    Method.GET / "list" / string("groupId") -> handler { (groupId: String, request: Request) =>
      searchIndex.snapshot.map { all =>
        import webjars.models.WebJar.given
        if all.isEmpty then
          Response(Status.ServiceUnavailable, Headers(Header.ContentType(MediaType.application.json).untyped), Body.fromString(s"""{"message":"$INDEXING_MSG"}"""))
        else
          val filtered = all.filter(_.groupId == groupId)
          maybeCached(request, filtered)(wj => jsonResponse(wj.toJson))
      }
    },

    // Redirect classic/npm list to index
    Method.GET / "classic" -> handler(Response.redirect(URL.root)),
    Method.GET / "npm" -> handler(Response.redirect(URL.root)),

    // All WebJars page (HTML or JSON) — reads from the in-memory SearchIndex.
    Method.GET / "all" -> handler { (request: Request) =>
      allWebJarsData.map { allWebJars =>
        val response =
          if allWebJars.isEmpty then
            if acceptsJson(request) then
              import webjars.models.WebJar.given
              Response(Status.ServiceUnavailable, Headers(Header.ContentType(MediaType.application.json).untyped), Body.fromString(Seq.empty[WebJar].toJson))
            else
              htmlResponse(AllPage(webJars, Right(INDEXING_MSG)), Status.ServiceUnavailable)
          else
            maybeCached(request, allWebJars) { wj =>
              if acceptsJson(request) then
                import webjars.models.WebJar.given
                jsonResponse(wj.toJson)
              else
                htmlResponse(AllPage(webJars, Left(wj)))
            }
        corsHeaders(response)
      }
    },

    // Documentation
    Method.GET / "documentation" -> handler(htmlResponse(DocumentationPage(webJars))),

    // Package exists. Always refreshes the artifact's Maven Central cache
    // entry, returning an [[ExistsResponse]]:
    //   - `deployable`: true if `deployable.versions(name)` succeeded
    //   - `versions`: current versions on Maven Central (empty if absent)
    //   - `error`: populated only for *transient* failures
    //     (rate-limit / 5xx / etc.) so the UI can show "unavailable" instead
    //     of the raw upstream message. Permanent "not deployable" cases
    //     leave `error` empty; the UI builds a context-aware message.
    // Type-agnostic — works the same for NPM and Classic.
    Method.GET / "exists" -> handler { (request: Request) =>
      val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
      val name = request.url.queryParams.getAll("name").headOption.getOrElse("")

      // Walk the cause chain for a non-404 ServerError, which we treat as
      // a transient failure (rate-limited, 5xx, etc.). 404s are "not found"
      // and considered permanent.
      def isTransient(t: Throwable): Boolean = t match
        case ServerError(_, 404)    => false
        case _: ServerError         => true
        case _                      => Option(t.getCause).exists(isTransient)

      allDeployables.fromName(webJarType).fold {
        ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' does not exist")))
      } { deployable =>
        val versionsCheck = ZIO.scoped(deployable.versions(name)).either
        // For the MC refresh path we want an artifactId even when the
        // deployable's own resolution fails (e.g., a Classic name like
        // `dgrid` that isn't in webjars-classic metadata but DOES exist
        // on Maven Central as `org.webjars:dgrid`). Fall back to using
        // the name as the artifactId — best-effort; if it's wrong MC just
        // returns no versions.
        val artifactIdEffect =
          ZIO.scoped(deployable.artifactId(name))
        val mcRefresh =
          artifactIdEffect
            .flatMap(aid => mavenCentral.refreshArtifactNow(deployable.groupId, aid))
            .orElseSucceed(List.empty)
        versionsCheck.zipPar(mcRefresh).flatMap { (versionsResult, mcVersions) =>
          val errorField: ZIO[Any, Nothing, Option[String]] = versionsResult match
            case Right(_) => ZIO.none
            case Left(t)  =>
              if isTransient(t) then
                ZIO.logWarning(s"/exists transient failure for $webJarType:$name — ${t.getMessage}").as(Some("Deployment is unavailable at this time"))
              else
                ZIO.none
          errorField.map: maybeError =>
            jsonResponse(ExistsResponse(
              deployable = versionsResult.isRight,
              versions   = mcVersions.map(_.number),
              error      = maybeError,
            ).toJson)
        }
      }
    },

    // Package versions
    Method.GET / "versions" -> handler { (request: Request) =>
      val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
      val name = request.url.queryParams.getAll("name").headOption.getOrElse("")
      val maybeBranch = request.url.queryParams.getAll("branch").headOption

      allDeployables.fromName(webJarType).fold {
        ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' does not exist")))
      } { deployable =>
        ZIO.scoped {
          val versionsEffect = maybeBranch.fold {
            cache.get[Seq[String]](s"$webJarType-versions-$name", 1.hour) {
              deployable.versions(name).map(_.toSeq.sorted(VersionStringOrdering).reverse)
            }
          } { branch =>
            cache.get[Seq[String]](s"$webJarType-versions-$name-$branch", 1.hour) {
              git.versionsOnBranch(name, branch)
            }
          }

          versionsEffect.map { versions =>
            jsonResponse(versions.toJson)
          }
        }.catchAll { _ =>
          ZIO.succeed(Response(Status.InternalServerError))
        }
      }
    },

    // Deploy WebJar
    Method.POST / "deploy" -> handler { (request: Request) =>
      handleDeploy(request)
    },

    Method.GET / "deploy" -> handler { (request: Request) =>
      handleDeploy(request)
    },

    // Create WebJar
    Method.POST / "create" -> handler { (request: Request) =>
      ZIO.scoped {
        val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
        val nameOrUrlish = request.url.queryParams.getAll("nameOrUrlish").headOption.getOrElse("")
        val version = request.url.queryParams.getAll("version").headOption.getOrElse("")

        request.body.asString.flatMap { bodyStr =>
          val bodyJson = bodyStr.fromJson[zio.json.ast.Json].toOption
          val licenseOverride = bodyJson.flatMap { json =>
            json.asObject.flatMap(_.get("license")).flatMap(_.asObject).map { licenses =>
              licenses.toMap.collect { case (name, v) if v.asString.isDefined =>
                val url = v.asString.get
                URL.parseOption(url).fold[License](LicenseWithName(name))(u => LicenseWithNameAndUrl(name, u))
              }.toSet
            }
          }
          val groupIdOverride = bodyJson.flatMap { json =>
            json.asObject.flatMap(_.get("groupId")).flatMap(_.asString)
          }

          allDeployables.fromName(webJarType).fold {
            ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' can not be created")))
          } { deployable =>
            deployWebJar.create(deployable, nameOrUrlish, version, licenseOverride, groupIdOverride.map(MavenCentral.GroupId(_))).map { case (artifactId, bytes) =>
              val filename = artifactId.toString + ".jar"
              Response(
                Status.Ok,
                Headers(
                  Header.ContentType(MediaType.application.`octet-stream`).untyped,
                  Header.ContentDisposition.attachment(filename).untyped,
                  Header.ContentLength(bytes.length.toLong).untyped,
                ),
                Body.fromChunk(Chunk.fromArray(bytes))
              )
            }.catchAll(e => ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(e.getMessage))))
          }
        }
      }.catchAll { e =>
        ZIO.succeed(Response(Status.InternalServerError, body = Body.fromString(e.getMessage)))
      }
    },

    // Create Classic WebJar
    Method.POST / "create" / "classic" -> handler { (request: Request) =>
      ZIO.scoped {
        val nameOrUrlish = request.url.queryParams.getAll("nameOrUrlish").headOption.getOrElse("")
        val version = request.url.queryParams.getAll("version").headOption.getOrElse("")

        defer:
          val bodyText = request.body.asString.run
          if bodyText.isEmpty then
            Response(Status.BadRequest, body = Body.fromString("Expected text/plain body with properties file content"))
          else
            Classic.parseMetadata(nameOrUrlish, bodyText) match
              case scala.util.Success(metadata) =>
                val releaseVersion = MavenCentral.Version(version.stripPrefix("v"))
                (defer:
                  val packageInfo = classic.infoFromMetadata(metadata, version, None).run
                  val licenses = classic.licensesFromMetadata(metadata, version, packageInfo).run
                  val sourceUrl = sourceLocator.sourceUrl(packageInfo.sourceConnectionUri).run
                  val pom = PomTemplate(classic.groupId, metadata.id, releaseVersion, packageInfo, sourceUrl, Set.empty, Set.empty, licenses)
                  val archive = classic.archiveFromMetadata(metadata, version).run
                  val maybeBaseDirGlob = classic.maybeBaseDirGlobFromMetadata(metadata).run
                  val bytes = WebJarCreator.createWebJar(ZStream.fromInputStream(archive), maybeBaseDirGlob, Set.empty, pom, packageInfo.name, licenses, classic.groupId, metadata.id, releaseVersion, s"${metadata.id}/$releaseVersion/").run
                  val filename = metadata.id.toString + ".jar"
                  ZIO.succeed(Response(
                    Status.Ok,
                    Headers(
                      Header.ContentType(MediaType.application.`octet-stream`).untyped,
                      Header.ContentDisposition.attachment(filename).untyped,
                      Header.ContentLength(bytes.length.toLong).untyped,
                    ),
                    Body.fromChunk(Chunk.fromArray(bytes))
                  )).run
                ).catchAll(e => ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(e.getMessage)))).run
              case scala.util.Failure(e) =>
                Response(Status.BadRequest, body = Body.fromString(s"Invalid properties: ${e.getMessage}"))
      }.catchAll { e =>
        ZIO.succeed(Response(Status.InternalServerError, body = Body.fromString(e.getMessage)))
      }
    },

    // List files (with and without groupId)
    Method.GET / "listfiles" / string("artifactId") / string("version") -> handler { (artifactId: String, version: String, request: Request) =>
      handleListFiles("org.webjars", artifactId, URLDecoder.decode(version, "UTF-8"), request)
    },

    Method.GET / "listfiles" / string("groupId") / string("artifactId") / string("version") -> handler { (groupId: String, artifactId: String, version: String, request: Request) =>
      handleListFiles(groupId, artifactId, URLDecoder.decode(version, "UTF-8"), request)
    },

    // File redirect
    Method.GET / "files" / "org.webjars" / string("artifactId") / string("version") / trailing -> handler { (artifactId: String, version: String, path: Path, _: Request) =>
      movedPermanently(s"https://webjars-file-service.herokuapp.com/files/org.webjars/$artifactId/$version/${path.dropLeadingSlash}")
    },

    Method.GET / "files" / "org.webjars.bower" / string("artifactId") / string("version") / trailing -> handler { (artifactId: String, version: String, path: Path, _: Request) =>
      movedPermanently(s"https://webjars-file-service.herokuapp.com/files/org.webjars.bower/$artifactId/$version/${path.dropLeadingSlash}")
    },

    Method.GET / "files" / "org.webjars.npm" / string("artifactId") / string("version") / trailing -> handler { (artifactId: String, version: String, path: Path, _: Request) =>
      movedPermanently(s"https://webjars-file-service.herokuapp.com/files/org.webjars.npm/$artifactId/$version/${path.dropLeadingSlash}")
    },

    Method.GET / "files" / string("artifactId") / string("version") / trailing -> handler { (artifactId: String, version: String, path: Path, _: Request) =>
      movedPermanently(s"https://webjars-file-service.herokuapp.com/files/org.webjars/$artifactId/$version/${path.dropLeadingSlash}")
    },

    // File OPTIONS (CORS)
    Method.OPTIONS / "files" / trailing -> handler { (_: Path, _: Request) =>
      Response.ok.addHeader(Header.AccessControlAllowOrigin.All)
        .addHeader(Header.AccessControlAllowHeaders("Content-Type"))
    },

    // CORS preflight
    Method.OPTIONS / trailing -> handler { (_: Path, _: Request) =>
      Response.ok
        .addHeader(Header.AccessControlAllowOrigin.All)
        .addHeader(Header.AccessControlAllowMethods(Method.GET))
    },
  )

  private def handleDeploy(request: Request): ZIO[Client & Redis & DeployerEnv, Nothing, Response] =
    val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
    val nameOrUrlish = request.url.queryParams.getAll("nameOrUrlish").headOption.getOrElse("")
    val version = request.url.queryParams.getAll("version").headOption.getOrElse("")

    allDeployables.fromName(webJarType).fold {
      ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' can not be deployed")))
    } { deployable =>
      val stream: ZStream[Client & Redis & DeployerEnv, Nothing, String] = deployJobs.deploy(deployable, nameOrUrlish, version)
      val bytes =
        if acceptsEventStream(request) then stream.flatMap(msg => ZStream.fromIterable(s"data: $msg\n\n".getBytes))
        else                                stream.flatMap(msg => ZStream.fromIterable(msg.getBytes))
      val contentType =
        if acceptsEventStream(request) then MediaType.text.`event-stream` else MediaType.text.plain
      Body.fromStreamChunkedEnv(bytes).orDie.map: body =>
        Response(Status.Ok, Headers(Header.ContentType(contentType).untyped), body)
    }

  private def handleListFiles(groupId: String, artifactId: String, version: String, request: Request): ZIO[Redis, Nothing, Response] =
    val gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId(groupId), MavenCentral.ArtifactId(artifactId), MavenCentral.Version(version))
    popularMetrics.recordListFilesClick(MavenCentral.GroupId(groupId), MavenCentral.ArtifactId(artifactId)) *>
    ZIO.scoped {
      cache.get[List[String]](s"listfiles-$groupId-$artifactId-$version", 1.day) {
        webJarsFileService.getFileList(gav)
      }.map { fileList =>
        if acceptsJson(request) then
          corsHeaders(jsonResponse(fileList.toJson))
        else
          corsHeaders(htmlResponse(FileListPage(groupId, artifactId, version, fileList)))
      }
    }.catchAll {
      case _: FileNotFoundException =>
        ZIO.succeed(corsHeaders(Response(Status.NotFound, body = Body.fromString(s"WebJar Not Found $groupId : $artifactId : $version"))))
      case e: Throwable =>
        ZIO.succeed(corsHeaders(Response(Status.InternalServerError, body = Body.fromString(s"Problems retrieving WebJar ($groupId : $artifactId : $version) - ${e.getMessage}"))))
    }

object AppRoutes:
  def live[DeployerEnv : Tag]: ZLayer[Git & Cache & MavenCentralWebJars & DeployWebJar[DeployerEnv] & DeployJobs[DeployerEnv] & WebJarsFileService & AppConfig & SourceLocator & Classic & AllDeployables & WebJars & PopularMetrics & PopularRanking & SearchIndex, Nothing, AppRoutes[DeployerEnv]] =
    ZLayer.derive[AppRoutes[DeployerEnv]]
