package webjars.routes

import com.jamesward.zio_mavencentral.MavenCentral
import io.lemonlabs.uri.AbsoluteUrl
import webjars.config.AppConfig
import webjars.models.WebJar
import webjars.utils.*
import webjars.views.*
import webjars.views.partials.WebJarList
import zio.*
import zio.direct.*
import zio.http.*
import zio.json.*
import zio.stream.ZStream

import java.io.FileNotFoundException
import java.net.URLDecoder
import scala.util.hashing.MurmurHash3

case class AppRoutes(
  git: Git,
  cache: Cache,
  mavenCentral: MavenCentralWebJars,
  deployWebJar: DeployWebJar,
  deployJobs: DeployJobs,
  webJarsFileService: WebJarsFileService,
  config: AppConfig,
  sourceLocator: SourceLocator,
  classic: Classic,
  allDeployables: AllDeployables,
  webJars: WebJars,
):

  private val MAX_POPULAR_WEBJARS = 20

  private val WEBJAR_FETCH_ERROR = """
    Looks like there was an error fetching the WebJars.
    Until the issue is resolved you can search on search.maven.org for <a href="https://search.maven.org/search?q=g:org.webjars">Classic WebJars</a> or <a href="https://search.maven.org/search?q=g:org.webjars.npm">NPM WebJars</a>.
    If this problem persists please <a href="https://github.com/webjars/webjars/issues/new">file an issue</a>.
    """

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

  def allPopular: ZIO[Any, Throwable, Seq[WebJar]] =
    val reqs = allDeployables.groupIds().map: groupId =>
      mavenCentral.featuredWebJars(groupId, MAX_POPULAR_WEBJARS)
    ZIO.collectAll(reqs).map(_.flatten.toSeq)

  def allWebJarsData: ZIO[Any, Throwable, Seq[WebJar]] =
    val reqs = allDeployables.groupIds().map: groupId =>
      mavenCentral.searchWebJars(groupId, None)
    ZIO.collectAll(reqs).map(_.flatten.toSeq)

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

  val routes: Routes[Any, Nothing] = Routes(

    // Home page
    Method.GET / "" -> handler { (request: Request) =>
      allPopular.map { popularWebJars =>
        maybeCached(request, popularWebJars)(wj => htmlResponse(IndexPage(webJars, Left(wj))))
      }.catchAll { e =>
        ZIO.logError(s"index WebJar fetch failed: ${e.getMessage}") *>
        ZIO.succeed(htmlResponse(IndexPage(webJars, Right(WEBJAR_FETCH_ERROR)), Status.InternalServerError))
      }
    },

    // Popular WebJars (HTML or JSON)
    Method.GET / "popular" -> handler { (request: Request) =>
      allPopular.map { popularWebJars =>
        if acceptsJson(request) then
          import webjars.models.WebJar.given
          jsonResponse(popularWebJars.toJson)
        else
          maybeCached(request, popularWebJars)(wj => htmlResponse(WebJarList(Left(wj))))
      }.catchAll { _ =>
        ZIO.succeed {
          if acceptsJson(request) then
            Response(Status.InternalServerError, Headers(Header.ContentType(MediaType.application.json).untyped), Body.fromString(WEBJAR_FETCH_ERROR))
          else
            htmlResponse(WebJarList(Right(WEBJAR_FETCH_ERROR)), Status.InternalServerError)
        }
      }
    },

    // Search WebJars
    Method.GET / "search" -> handler { (request: Request) =>
      val query = request.url.queryParams.getAll("query").headOption.getOrElse("")
      val groupIds = request.url.queryParams.getAll("groupId")

      val searchFutures = groupIds.map: groupId =>
        mavenCentral.searchWebJars(MavenCentral.GroupId(groupId), Some(query))

      ZIO.collectAll(searchFutures).map(_.flatten.toSeq).map { matchingWebJars =>
        if acceptsJson(request) then
          import webjars.models.WebJar.given
          jsonResponse(matchingWebJars.toJson)
        else
          htmlResponse(WebJarList(Left(matchingWebJars)))
      }.catchAll { e =>
        ZIO.logError(s"searchWebJars failed: ${e.getMessage}") *>
        ZIO.succeed {
          if acceptsJson(request) then
            import webjars.models.WebJar.given
            jsonResponse(Seq.empty[WebJar].toJson)
          else
            htmlResponse(WebJarList(Right(WEBJAR_FETCH_ERROR)), Status.InternalServerError)
        }
      }
    },

    // List by groupId (JSON)
    Method.GET / "list" / string("groupId") -> handler { (groupId: String, request: Request) =>
      mavenCentral.searchWebJars(MavenCentral.GroupId(groupId), None).map { matchingWebJars =>
        import webjars.models.WebJar.given
        maybeCached(request, matchingWebJars)(wj => jsonResponse(wj.toJson))
      }.catchAll { _ =>
        import webjars.models.WebJar.given
        ZIO.succeed(Response(Status.InternalServerError, Headers(Header.ContentType(MediaType.application.json).untyped), Body.fromString(Seq.empty[WebJar].toJson)))
      }
    },

    // Redirect classic/npm list to index
    Method.GET / "classic" -> handler(Response.redirect(URL.root)),
    Method.GET / "npm" -> handler(Response.redirect(URL.root)),

    // All WebJars page (HTML or JSON)
    Method.GET / "all" -> handler { (request: Request) =>
      allWebJarsData.map { allWebJars =>
        val response = maybeCached(request, allWebJars) { wj =>
          if acceptsJson(request) then
            import webjars.models.WebJar.given
            jsonResponse(wj.toJson)
          else
            htmlResponse(AllPage(webJars, Left(wj)))
        }
        corsHeaders(response)
      }.catchAll { e =>
        ZIO.logError(s"allWebJars fetch error: ${e.getMessage}") *>
        ZIO.succeed {
          val response = if acceptsJson(request) then
            Response(Status.InternalServerError, Headers(Header.ContentType(MediaType.application.json).untyped), Body.fromString("[]"))
          else
            htmlResponse(AllPage(webJars, Right(WEBJAR_FETCH_ERROR)), Status.InternalServerError)
          corsHeaders(response)
        }
      }
    },

    // Documentation
    Method.GET / "documentation" -> handler(htmlResponse(DocumentationPage(webJars))),

    // Package exists
    Method.GET / "exists" -> handler { (request: Request) =>
      val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
      val name = request.url.queryParams.getAll("name").headOption.getOrElse("")

      allDeployables.fromName(webJarType).fold {
        ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' does not exist")))
      } { deployable =>
        ZIO.scoped {
          deployable.versions(name).map { _ =>
            Response(Status.Ok, Headers(Header.ContentType(MediaType.application.xml).untyped), Body.fromString("<root></root>"))
          }
        }.catchAll { e =>
          ZIO.succeed(Response(Status.InternalServerError, body = Body.fromString(e.getMessage)))
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
                AbsoluteUrl.parseOption(url).fold[License](LicenseWithName(name))(u => LicenseWithNameAndUrl(name, u))
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

  private def handleDeploy(request: Request): ZIO[Any, Nothing, Response] =
    val webJarType = request.url.queryParams.getAll("webJarType").headOption.getOrElse("")
    val nameOrUrlish = request.url.queryParams.getAll("nameOrUrlish").headOption.getOrElse("")
    val version = request.url.queryParams.getAll("version").headOption.getOrElse("")

    allDeployables.fromName(webJarType).fold {
      ZIO.succeed(Response(Status.BadRequest, body = Body.fromString(s"Specified WebJar type '$webJarType' can not be deployed")))
    } { deployable =>
      val stream: ZStream[Any, Nothing, String] = deployJobs.deploy(deployable, nameOrUrlish, version)

      if acceptsEventStream(request) then
        ZIO.succeed {
          Response(
            Status.Ok,
            Headers(Header.ContentType(MediaType.text.`event-stream`).untyped),
            Body.fromStreamChunked(
              stream.flatMap(msg => ZStream.fromIterable(s"data: $msg\n\n".getBytes))
            )
          )
        }
      else
        ZIO.succeed {
          Response(
            Status.Ok,
            Headers(Header.ContentType(MediaType.text.plain).untyped),
            Body.fromStreamChunked(
              stream.flatMap(msg => ZStream.fromIterable(msg.getBytes))
            )
          )
        }
    }

  private def handleListFiles(groupId: String, artifactId: String, version: String, request: Request): ZIO[Any, Nothing, Response] =
    val gav = MavenCentral.GroupArtifactVersion(MavenCentral.GroupId(groupId), MavenCentral.ArtifactId(artifactId), MavenCentral.Version(version))
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
  val live: ZLayer[Git & Cache & MavenCentralWebJars & DeployWebJar & DeployJobs & WebJarsFileService & AppConfig & SourceLocator & Classic & AllDeployables & WebJars, Nothing, AppRoutes] = ZLayer.derive[AppRoutes]
