# Technical Specification: WebJars Migration

## Context

The WebJars application is being migrated from Play Framework to a pure ZIO stack. The app currently uses Play Framework 3.0.10 with Scala 3, Guice DI, Play WS, Twirl templates, Pekko Streams, and a hybrid ZIO/Future async model. The goal is to replace the Play Framework layer with ZIO HTTP while keeping the core domain logic, external integrations (JGit, BouncyCastle, zio-mavencentral), and infrastructure (Redis, Testcontainers) intact. Archive handling (ZIP, TAR, GZIP, JAR) migrates from Apache Commons Compress to zio-streams-compress.

---

## Technology Migration Map

### Confirmed Targets (user-specified)
- **Scala 3** (already in use — keep)
- **ZIO** (core effect system, replacing Futures)
- **ZIO HTTP** (replacing Play Framework)
- **ZIO HTTP template2** (`zio.http.template2._`, replacing Twirl)
- **Redis** via zio-redis (already in use — keep)
- **Testcontainers** (already in use — keep)
- **WebJars** (frontend dependency mechanism — keep)

### Full Migration Table

| Category | Current | Target | Notes |
|----------|---------|--------|-------|
| **Web Framework** | Play Framework 3.0.10 (`sbt-plugin`) | ZIO HTTP (`dev.zio::zio-http`) | Complete replacement |
| **Template Engine** | Twirl (`.scala.html` files) | `zio.http.template2._` | Type-safe Scala DSL for HTML |
| **Routing** | `conf/routes` file | ZIO HTTP `Routes` DSL | Programmatic route definitions |
| **DI Framework** | Google Guice (`@Inject`, `@Singleton`, `@ImplementedBy`) | ZIO `ZLayer` | Native ZIO dependency injection |
| **HTTP Client** | Play WS (`WSClient`) | ZIO HTTP `Client` | Already partially used via zio-mavencentral |
| **JSON Serialization** | Play JSON (`play.api.libs.json`) | zio-json (`zio.json`) | Compile-time derivation |
| **Streaming / SSE** | Pekko Streams (`Source`, `Flow`, `Sink`) | ZIO Streams (`ZStream`) | ZIO HTTP has native SSE support |
| **Actor System** | Apache Pekko (via Play) | Removed entirely | ZIO fibers replace actors |
| **Local Cache** | Caffeine via Play `SyncCacheApi` | Caffeine (direct) or `zio-cache` | Remove Play wrapper, keep Caffeine |
| **Distributed Cache** | zio-redis 1.1.13 | zio-redis (keep) | No change needed |
| **Background Jobs** | ZIO with `Adapter.runToFuture` bridge | Pure ZIO (`ZIO.repeat`, `Schedule`) | Remove Future bridge |
| **Testing** | specs2 | ZIO Test | Native ZIO test framework |
| **Testcontainers** | testcontainers-scala 0.44.1 | testcontainers-scala (keep) | Integrate with ZIO Test |
| **Configuration** | HOCON (`application.conf`) via Play `Configuration` | zio-config | Typed config case classes |
| **Logging** | Logback (via Play) | Logback + zio-logging | Keep Logback backend |
| **SCSS Compilation** | sbt-sassify 1.5.2 | sbt-sassify (keep) | Works independently of Play |
| **Asset Fingerprinting** | sbt-digest 2.1.0 | sbt-digest (keep) | Works independently of Play |
| **Asset Compression** | sbt-gzip 2.0.0 | sbt-gzip (keep) | Works independently of Play |
| **Static Asset Serving** | Play `Assets` controller + `webjars-play` plugin | ZIO HTTP static file serving from classpath | Custom middleware for WebJars |
| **Archive Handling** | Apache Commons Compress 1.28.0 | zio-streams-compress 1.1.3 | ZIP, TAR, GZIP via ZIO Streams |
| **Serialization (Redis)** | zio-schema + ProtobufCodec | zio-schema + ProtobufCodec (keep) | No change |

### Libraries That Stay Unchanged

| Library | Version | Purpose |
|---------|---------|---------|
| zio-mavencentral | 0.5.3 | Maven Central search, POM fetching, deployment (all Maven Central interactions) |
| zio-redis | 1.1.13 | Redis/Valkey client |
| JGit | 7.5.0 | Git operations |
| BouncyCastle (bcpg, bcutil) | 1.83 | GPG signing |
| scala-uri | 4.2.0 | URI parsing |
| hasher | 1.2.3 | MD5/SHA1 checksums |
| fastparse | 3.1.1 | Parser combinators |
| testcontainers-scala | 0.44.1 | Container-based testing |

---

## Detailed Migration Notes

### 1. Play Framework → ZIO HTTP

**Current:** Play controllers extend `BaseController`, use `Action.async { Future[Result] }`, content negotiation via `render { case Accepts.Html() => ... }`.

**Target:** ZIO HTTP `Routes` with `Handler` functions returning `ZIO[R, E, Response]`. Content negotiation via inspecting `Accept` header manually or using ZIO HTTP's endpoint API.

**Key changes:**
- `conf/routes` → Programmatic `Routes` definition
- `Action.async { implicit request => ... }` → `Handler.fromFunctionZIO[Request] { request => ... }`
- `Ok(views.html.index(...))` → `Response.html(dom)` using template2
- `Ok(Json.toJson(data))` → `Response.json(data.toJson)` using zio-json
- Play's `Request` → ZIO HTTP's `Request`
- `Results.Redirect` → `Response.redirect`
- `Results.Status(400)` → `Response(status = Status.BadRequest)`

### 2. Twirl Templates → zio.http.template2

**Current:** 50+ `.scala.html` files in `app/views/` with Twirl syntax (`@`, `@for`, `@if`, `@defining`).

**Target:** Scala functions returning `Dom` using `zio.http.template2._` DSL.

**Example migration:**
```scala
// Before (Twirl): views/partials/navbar.scala.html
@()
<nav class="navbar">
  <a href="/">WebJars</a>
  <a href="/documentation">Documentation</a>
</nav>

// After (template2): views/Navbar.scala
def navbar: Dom =
  nav(classAttr := "navbar",
    a(hrefAttr := "/", "WebJars"),
    a(hrefAttr := "/documentation", "Documentation")
  )
```

**Template2 capabilities:**
- HTML elements: `html`, `head`, `body`, `div`, `p`, `a`, `ul`, `li`, `h1`-`h6`, `nav`, `form`, `input`, `table`, etc.
- Attributes: `classAttr`, `hrefAttr`, `styleAttr`, `idAttr`, etc. (suffixed with `Attr`)
- CSS: `css := "class-name"` shorthand
- Composition: `Dom` values compose with nesting and `++`
- Implicit conversions: `String` → `Dom.Text`, `Iterable[Dom]` → `Dom.Fragment`
- Endpoint integration: `Endpoint(...).out[Dom](MediaType.text.html)`

### 3. Guice DI → ZIO ZLayer

**Current:** `@Inject()` constructor injection, `@Singleton` scoping, `@ImplementedBy` trait binding, `Module.scala` for Guice overrides in tests.

**Target:** ZIO `ZLayer` for dependency graph construction. Services become ZIO service traits with companion objects providing `ZLayer`.

**Pattern:**
```scala
// Before (Guice)
@Singleton
class GitHub @Inject() (ws: WSClient, config: Configuration, cache: Cache) { ... }

// After (ZLayer)
trait GitHub:
  def currentUrls(url: String): ZIO[Any, Throwable, (String, String, String)]
  // ...

object GitHub:
  val live: ZLayer[Client & AppConfig & Cache, Nothing, GitHub] =
    ZLayer.fromFunction(GitHubLive(_, _, _))
```

### 4. Play WS → ZIO HTTP Client

**Current:** `WSClient` injected via Guice, used for all outbound HTTP (npm registry, GitHub API, Heroku API, license detector, file service, semver service).

**Target:** `zio.http.Client` provided via `ZLayer`. Already used by zio-mavencentral.

**Key changes:**
- `ws.url(url).get()` → `Client.request(Request.get(url))`
- `ws.url(url).withHeaders(...).post(body)` → `Client.request(Request.post(url, body).addHeaders(...))`
- Response handling: `.map(_.body)` → `.flatMap(_.body.asString)`
- Redirect following: Custom `resolveRedir` → ZIO HTTP Client with redirect config

### 5. Pekko Streams → ZIO Streams

**Current:** `Source[ByteString, _]` for SSE deployment streaming, `Tcp` for Heroku rendezvous protocol, `Framing` for message parsing.

**Target:** `ZStream[Any, Throwable, Byte]` for streaming, `ServerSentEvent` for SSE.

**SSE migration:**
```scala
// Before (Pekko)
Ok.chunked(source.map(_.utf8String).via(EventSource.flow))

// After (ZIO)
Response(body = Body.fromStream(
  stream.map(msg => ServerSentEvent(data = msg))
))
```

### 6. Play JSON → zio-json

**Current:** Play JSON with `given Format[T] = Json.format[T]`, custom `Reads`/`Writes`, `Json.toJson`, `Json.parse`.

**Target:** zio-json with `derives JsonEncoder, JsonDecoder`, `toJson`, `fromJson`.

**Key changes:**
- `given Format[WebJar] = Json.format[WebJar]` → `case class WebJar(...) derives JsonEncoder, JsonDecoder`
- `Json.toJson(data)` → `data.toJson`
- `request.body.asJson` → `request.body.to[T]` (via zio-schema or manual decode)
- Custom readers for NPM's irregular JSON → Custom `JsonDecoder` instances

### 7. Apache Commons Compress → zio-streams-compress

**Current:** `org.apache.commons:commons-compress:1.28.0` used in `WebJarCreator.scala` (JAR creation), `ArchiveCreator.scala` (TAR creation), `NPM.scala` (GZIP decompression of npm tarballs), and `Git.scala` (TAR archiving).

**Target:** `dev.zio::zio-streams-compress-zip`, `dev.zio::zio-streams-compress-tar`, `dev.zio::zio-streams-compress-gzip` (version 1.1.3).

**Key changes:**
- JAR creation: Currently uses `JarArchiveOutputStream` → Use ZIP archiver from zio-streams-compress with JAR manifest handling
- TAR creation: Currently uses `TarArchiveOutputStream` → Use `TarArchiver.archive` via ZIO Streams pipeline
- GZIP decompression: Currently uses `GZIPInputStream` → Use `GzipDecompressor.decompress` as a ZStream pipeline
- TAR extraction: Currently uses `TarArchiveInputStream` → Use `TarUnarchiver.unarchive` with `ZStream`

**Archive creation example:**
```scala
// Before (Commons Compress)
val tarOut = new TarArchiveOutputStream(outputStream)
tarOut.setLongFileMode(TarArchiveOutputStream.LONGFILE_POSIX)
// ... add entries manually

// After (zio-streams-compress)
ZStream(archiveEntries: _*)
  .via(TarArchiver.archive)
  .via(GzipCompressor.compress)
  .run(ZSink.fromOutputStream(outputStream))
```

### 8. Configuration → zio-config

**Current:** Play `Configuration` object, `config.get[String]("key")`, `config.getOptional[String]("key")`.

**Target:** Typed config case classes with zio-config.

```scala
case class AppConfig(
  github: GitHubConfig,
  oss: OssConfig,
  heroku: HerokuConfig,
  fileService: FileServiceConfig,
  // ...
)

case class GitHubConfig(authToken: Option[String])
case class OssConfig(gpgKey: Option[String], gpgPass: Option[String], ...)

object AppConfig:
  val live: ZLayer[Any, Config.Error, AppConfig] = ZLayer.fromZIO(
    ZIO.config[AppConfig](AppConfig.descriptor)
  )
```

### 9. Testing → ZIO Test

**Current:** specs2 `PlaySpecification`, `WithApplication`, `FakeRequest`, Guice overrides for mocking.

**Target:** ZIO Test `ZIOSpec`, `ZLayer` test overrides, `TestContainers` integration.

```scala
// Before (specs2)
class ApplicationSpec extends PlaySpecification with GlobalApplication {
  "index" in new WithApplication(weightedApp) {
    val result = route(weightedApp, FakeRequest(GET, "/")).get
    status(result) must beEqualTo(OK)
  }
}

// After (ZIO Test)
object ApplicationSpec extends ZIOSpec[TestEnvironment & AppDeps] {
  def spec = suite("Application")(
    test("index returns 200") {
      for {
        response <- routes.runZIO(Request.get(URL.root))
      } yield assertTrue(response.status == Status.Ok)
    }
  )

  override val bootstrap = testLayers
}
```

### 10. Static Assets & WebJars Serving

**Current:** Play `Assets` controller with sbt-digest fingerprinting, `webjars-play` plugin mounts at `/webjars/`.

**Target:** Custom ZIO HTTP middleware for:
- Serving static files from classpath resources (`public/`)
- Serving WebJar assets from classpath (`META-INF/resources/webjars/`)
- Content-type detection
- Cache headers for fingerprinted assets

**WebJars serving approach:**
```scala
val webJarsRoutes: Routes[Any, Nothing] =
  Routes(
    Method.GET / "webjars" / string("library") / string("version") / trailing ->
      handler { (library: String, version: String, path: Path, req: Request) =>
        val resource = s"META-INF/resources/webjars/$library/$version/$path"
        // Serve from classpath
      }
  )
```

### 11. CORS

**Current:** Custom `CorsAction` case class wrapping Play actions.

**Target:** ZIO HTTP built-in CORS middleware.

```scala
val corsConfig = CorsConfig(
  allowedOrigins = _ => true,
  allowedMethods = Set(Method.GET)
)
val app = routes @@ Middleware.cors(corsConfig)
```

### 12. ETag Caching

**Current:** Custom `maybeCached` function using `MurmurHash3`, returns 304 on match.

**Target:** Same logic as ZIO HTTP middleware or inline in handlers.

---

## Build System Changes

### SBT Plugins

| Plugin | Status |
|--------|--------|
| `sbt-plugin` (Play) | **Remove** |
| `sbt-digest` | Keep |
| `sbt-gzip` | Keep |
| `sbt-sassify` | Keep |

### Dependencies to Remove

| Dependency | Reason |
|-----------|--------|
| `org.playframework:sbt-plugin` | Replaced by ZIO HTTP |
| `ws` (Play WS) | Replaced by ZIO HTTP Client |
| `caffeine` (Play cache) | Replace with direct Caffeine or zio-cache |
| `guice` (Play DI) | Replaced by ZIO ZLayer |
| `filters` (Play filters) | Replaced by ZIO HTTP middleware |
| `webjars-play` | Custom WebJars serving |
| `specs2` | Replaced by ZIO Test |
| `org.apache.commons:commons-compress` | Replaced by zio-streams-compress |

### Dependencies to Add

| Dependency | Purpose |
|-----------|---------|
| `dev.zio::zio-http:3.x` | Web framework + HTTP client + template2 |
| `dev.zio::zio-json:0.x` | JSON serialization |
| `dev.zio::zio-config:4.x` | Configuration |
| `dev.zio::zio-config-typesafe:4.x` | HOCON config source |
| `dev.zio::zio-test:2.x` | Test framework |
| `dev.zio::zio-test-sbt:2.x` | SBT test runner |
| `dev.zio::zio-logging:2.x` | Logging integration |
| `dev.zio::zio-logging-slf4j2:2.x` | SLF4J bridge |
| `dev.zio::zio-streams-compress-zip:1.1.3` | ZIP/JAR archive handling |
| `dev.zio::zio-streams-compress-tar:1.1.3` | TAR archive handling |
| `dev.zio::zio-streams-compress-gzip:1.1.3` | GZIP compression/decompression |

---

## Application Architecture (Post-Migration)

```
src/main/scala/
  Main.scala                    # ZIOAppDefault entry point
  config/
    AppConfig.scala             # Typed configuration
  routes/
    AppRoutes.scala             # All route definitions
    CorsMiddleware.scala        # CORS middleware
    ETagMiddleware.scala        # ETag caching middleware
    StaticAssets.scala          # Static file + WebJars serving
  views/
    MainLayout.scala            # Base layout (head, navbar, footer)
    IndexPage.scala             # Home page
    AllPage.scala               # Browse all WebJars
    DocumentationPage.scala     # Documentation page
    FileListPage.scala          # File listing
    partials/
      Navbar.scala
      Footer.scala
      Icons.scala
      WebJarList.scala
      NewWebJarModal.scala
      FileListModal.scala
    sections/
      Hero.scala
      Popular.scala
    documentations/
      Usage.scala
      SpringBoot.scala
      PlayFramework.scala
      ... (one per framework)
  models/
    WebJar.scala                # Domain models with zio-json codecs
    PackageInfo.scala
    License.scala
  services/
    Deployable.scala            # Trait + NPM/Classic implementations
    AllDeployables.scala
    DeployWebJar.scala
    MavenCentralDeployer.scala
    MavenCentralWebJars.scala
    WebJarsCache.scala          # Redis cache (unchanged)
    Valkey.scala                # Redis connection (unchanged)
    Cache.scala                 # Local cache wrapper
  utils/
    Git.scala
    GitHub.scala
    NPM.scala
    Classic.scala
    Maven.scala
    SemVer.scala
    Heroku.scala
    LicenseDetector.scala
    SourceLocator.scala
    WebJarCreator.scala
    ArchiveCreator.scala
    WebJarsFileService.scala
    Bitbucket.scala
    VersionStringOrdering.scala

src/test/scala/
  ... (mirror structure, ZIO Test specs)
```

---

## Verification

After migration, validate with:
1. `./sbt compile` — no compilation errors
2. `./sbt test` — all ZIO Test specs pass (with Testcontainers for Redis)
3. `./test-integration.sh` — all 44 external integration tests pass against the running server
4. Manual smoke test of HTML pages, JSON endpoints, WebJar creation, and SSE deploy streaming
