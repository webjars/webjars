# WebJars Functional Specification

This document is the complete functional specification for the WebJars application. It captures all behavior, data types, services, integrations, and endpoints as they exist in the current Play Framework codebase. This serves as the authoritative reference for the migration to ZIO, ZIO HTTP, and Scala 3.

---

## 1. Application Overview

WebJars is a web application that packages client-side web libraries (JavaScript, CSS, etc.) as Maven-compatible JAR files for use in JVM-based projects. It handles the full WebJar lifecycle: **discovery, validation, packaging, and publishing to Maven Central**.

The application supports two package sources:
- **NPM** (`org.webjars.npm`) — packages from the NPM registry and git repositories
- **Classic** (`org.webjars`) — packages defined via `.properties` metadata in the `webjars/webjars-classic` GitHub repository

---

## 2. Data Types

### 2.1 Core Domain Models

```
WebJar
  groupId: String             -- Maven groupId (e.g. "org.webjars.npm")
  artifactId: String          -- Maven artifactId (e.g. "jquery")
  name: String                -- Human-readable name
  sourceUrl: String           -- Source repository URL
  versions: Seq[WebJarVersion]

WebJarVersion
  number: String              -- Version string (e.g. "3.7.1")
  numFiles: Option[Int]       -- Number of files in the WebJar JAR (lazily populated)
```

`WebJarVersion` ordering is reverse `VersionStringOrdering` (newest first).

### 2.2 Package Info

```
PackageInfo
  name: String
  version: String
  maybeHomepageUrl: Option[AbsoluteUrl]
  sourceConnectionUri: AbsoluteUrl
  maybeIssuesUrl: Option[AbsoluteUrl]
  metadataLicenses: Seq[LicenseMetadata]
  dependencies: Map[String, String]
  optionalDependencies: Map[String, String]
  maybeTag: Option[String]
```

Derived property:
- `maybeGitHubUrl` — extracted from `sourceConnectionUri` or `maybeHomepageUrl` if the domain is `github.com`

### 2.3 License Types

```
enum LicenseMetadata:
  case SpdxLicense(id: String)
  case ProvidedLicense(license: License)
  case UnresolvedLicense

sealed trait License:
  maybeName: Option[String]
  maybeUrl: Option[AbsoluteUrl]

case class LicenseWithName(name: String) extends License
case class LicenseWithUrl(url: AbsoluteUrl) extends License
case class LicenseWithNameAndUrl(name: String, url: AbsoluteUrl) extends License
```

### 2.4 Classic Metadata

```
sealed trait Metadata:
  id: MavenCentral.ArtifactId

case class MetadataNormal(id, name, repo, download: Option[String], requireJsMain: Option[String], baseDir: Option[String])
case class MetadataNpm(id, packageName, licenseName: Option[String], licenseUrl: Option[String])
```

Parsed from Java `.properties` files. `MetadataNormal` is used for GitHub-hosted packages. `MetadataNpm` is used when the `npm` property is set, delegating to the NPM registry.

### 2.5 Error Types

```
LicenseNotFoundException(message, cause)   -- License detection failed
NoValidLicenses()                          -- No license files found
MissingMetadataException(json, errors)     -- Required fields missing from package.json
ServerError(message, status)               -- HTTP error from external API
UnauthorizedError(message)                 -- Auth failure
```

### 2.6 Version Strings

`VersionStringOrdering` handles comparison of non-standard version strings:
- Normalizes `+` to `-`, handles `alpha`/`beta`/`rc` pre-releases
- Date-based versions (`MM.DD.YYYY`)
- SHA hash detection (sorted to the beginning)
- Pad unequal-length versions with zeros for comparison

---

## 3. HTTP Endpoints

### 3.1 Pages

| Method | Path | Description | Content Negotiation |
|--------|------|-------------|---------------------|
| `GET` | `/` | Home page with popular WebJars (top 20 per group) | HTML only |
| `GET` | `/all` | Browse all WebJars, CORS enabled | HTML or JSON |
| `GET` | `/documentation` | Framework integration documentation | HTML only |

### 3.2 WebJar Queries

| Method | Path | Description | Content Negotiation |
|--------|------|-------------|---------------------|
| `GET` | `/popular` | Top 20 WebJars per group | HTML or JSON |
| `GET` | `/search?query=&groupId=` | Search WebJars by query across specified groups | HTML or JSON |
| `GET` | `/list/:groupId` | List all WebJars for a groupId | JSON only (with ETag) |

### 3.3 Package Operations

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/exists?webJarType=&name=` | Check if package exists (returns XML `<root/>` or error) |
| `GET` | `/versions?webJarType=&name=&branch=` | Get available versions as JSON array. Results cached 1 hour. Branch parameter triggers git branch listing |

### 3.4 Deploy & Create

| Method | Path | Description |
|--------|------|-------------|
| `POST` or `GET` | `/deploy?webJarType=&nameOrUrlish=&version=` | Deploy WebJar to Maven Central. Streaming response with real-time progress |
| `POST` | `/create?webJarType=&nameOrUrlish=&version=` | Create WebJar JAR and return as download. Accepts optional JSON body with `license` map and `groupId` override |
| `POST` | `/create/classic?nameOrUrlish=&version=` | Create Classic WebJar from `.properties` body (`text/plain`) |

**Deploy response content types:**
- `text/plain` — chunked text, one message per line
- `text/event-stream` — Server-Sent Events (EventSource protocol)
- Keep-alive: space character every 25 seconds

**Create response:** Binary JAR download with `Content-Disposition: attachment`

### 3.5 File Operations

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/listfiles/:artifactId/:version` | List files (groupId defaults to `org.webjars`) |
| `GET` | `/listfiles/:groupId/:artifactId/:version` | List files for specific group | HTML or JSON |
| `GET` | `/files/org.webjars/:artifactId/:version/*file` | 301 redirect to file service |
| `GET` | `/files/org.webjars.bower/:artifactId/:version/*file` | 301 redirect to file service |
| `GET` | `/files/org.webjars.npm/:artifactId/:version/*file` | 301 redirect to file service |
| `GET` | `/files/:artifactId/:version/*file` | 301 redirect to file service (defaults to `org.webjars`) |
| `OPTIONS` | `/files/*` | CORS preflight for file endpoints |

File redirects point to: `https://webjars-file-service.herokuapp.com/files/{groupId}/{artifactId}/{version}/{file}`

### 3.6 Static Assets & CORS

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/assets/*file` | Versioned static assets (digest + gzip in production) |
| `GET` | `/favicon.ico` | Favicon |
| `GET` | `/robots.txt` | Robots file |
| `GET` | `/files/robots.txt` | Robots file (alternate path) |
| `OPTIONS` | `/*file` | CORS preflight — responds with `Access-Control-Allow-Origin: *` and `Access-Control-Allow-Methods: GET` |
| `->` | `/webjars` | WebJars Play plugin routes (serves WebJar assets) |

### 3.7 ETag Caching

The `maybeCached` function applies ETag caching to responses:
- Computes `MurmurHash3.seqHash` of the response data
- Returns `304 Not Modified` if `If-None-Match` header matches
- Disabled in `Dev` mode
- Applied to: `/`, `/all`, `/popular`, `/list/:groupId`

### 3.8 CORS

`CorsAction` wrapper adds `Access-Control-Allow-Origin: *` to responses.
Applied to: `/all`, `/listfiles`, `/files` OPTIONS

---

## 4. Services

### 4.1 Deployable Trait

The `Deployable` trait defines the interface for package sources. Two implementations exist: `NPM` and `Classic`.

```
trait Deployable:
  name: String
  groupId: MavenCentral.GroupId
  artifactId(nameOrUrlish): Future[MavenCentral.ArtifactId]
  releaseVersion(maybeVersion, packageInfo): MavenCentral.Version
  excludes(nameOrUrlish): Future[Set[String]]
  metadataFile: Option[String]
  maybeBaseDirGlob(nameOrUrlish): Future[Option[String]]
  pathPrefix(artifactId, releaseVersion, packageInfo): String
  info(nameOrUrlish, version, maybeSourceUri): Future[PackageInfo]
  mavenDependencies(dependencies): Future[Set[(GroupArtifact, String)]]
  archive(nameOrUrlish, version): Future[InputStream]
  file(nameOrUrlish, version, filename): Future[String]
  versions(nameOrUrlish): Future[Set[String]]
  licenses(nameOrUrlish, version, packageInfo): Future[Set[License]]
  depGraph(packageInfo, deps): Future[Map[String, String]]
```

**Version handling:** The `vless` extension strips leading `v` prefixes. The `vwith` extension adds a `v` prefix.

**License resolution strategy** (in `Deployable.licenses`):
1. Process metadata licenses (SPDX IDs, provided licenses, URLs, file references)
2. For SPDX expressions with `OR`, split into individual licenses
3. For URLs, fetch content and detect license
4. For file references (`file://`), read from package archive
5. Fallback: try typical license files (`LICENSE`, `LICENSE.txt`, `license.md`, `LICENSE-MIT`)
6. If all fail, throw `LicenseNotFoundException`

### 4.2 AllDeployables

Registry of all deployable types. Provides lookups by name and groupId.

```
AllDeployables:
  fromGroupId(groupId): Option[Deployable]    -- matches Classic or NPM
  groupIds(): Set[MavenCentral.GroupId]        -- {"org.webjars", "org.webjars.npm"}
  fromName(name): Option[Deployable]           -- case-insensitive "Classic" or "NPM"
```

### 4.3 NPM

**Group ID:** `org.webjars.npm`

**Registry base URL:** `https://registry.npmjs.org`

**Package info resolution:**
1. If the name is a git URL (contains `/` and doesn't start with `@`), clone the repo and read `package.json`
2. Otherwise, fetch version-specific metadata from npm registry: `GET /{package}/{version}`
3. Parse `PackageInfo` from the JSON (custom `Reads` handles diverse `repository`, `license`, `bugs` formats)
4. Strip optional dependencies from the main dependency map
5. Resolve GitHub URL redirects to get current homepage, source URI, and issues URL

**NPM `repository` URL normalization** (`NPM.uriIsh`):
- `git+https://` → strip `git+` prefix
- `gist:ID` → `https://gist.github.com/ID.git`
- `bitbucket:org/repo` → `https://bitbucket.org/org/repo.git`
- `github:org/repo` → `https://github.com/org/repo.git`
- `gitlab:org/repo` → `https://gitlab.com/org/repo.git`
- SSH-style `host:path` → `ssh://host/path`
- Bare `org/repo` → `https://github.com/org/repo.git`

**Scoped packages:** Supported (e.g. `@scope/package`). Scoped tgz URL: `/{scope}/{package}/-/{package}-{version}.tgz`

**Archive fetching:**
- Git repos: clone and create tar archive
- NPM packages: download `.tgz` from registry, decompress with GZIPInputStream (fallback to raw stream if not gzipped)

**Excludes:** `{"node_modules"}`

**Base dir glob:** `Some("*/")`

**Dependency graph resolution:**
- Recursively resolves all transitive dependencies
- Uses semver service to resolve version ranges to specific versions
- 10-minute timeout

**Maven dependency conversion:**
- Delegates to `Maven.convertNpmDependenciesToMaven`
- All dependencies use `org.webjars.npm` groupId

### 4.4 Classic

**Group ID:** `org.webjars`

**Metadata source:** `.properties` files from `https://github.com/webjars/webjars-classic` (configurable branch, defaults to `main`)

**Two metadata types:**
1. `MetadataNormal` — GitHub-hosted. Properties: `name`, `repo`, `download` (optional), `requirejs.main` (optional), `base.dir` (optional)
2. `MetadataNpm` — NPM-backed. Properties: `npm`, `license.name` (optional), `license.url` (optional)

**License resolution:**
- `MetadataNormal`: calls GitHub License API (`GET /repos/{repo}/license`), extracts SPDX ID
- `MetadataNpm`: uses `license.name` and `license.url` from properties

**Archive fetching:**
- `MetadataNormal`: downloads from `download` URL (with `${version}` substitution) or falls back to `https://github.com/{repo}/archive/{version}.zip`. If URL ends in `.tgz`, wraps in GZIPInputStream
- `MetadataNpm`: delegates to NPM archive

**Excludes:** empty set

**Dependency graph:** always returns empty map (Classic WebJars have no dependency resolution)

**Maven dependencies:** always returns empty set

### 4.5 DeployWebJar

Orchestrates the full deployment pipeline. Supports two modes:

**Local deploy pipeline:**
1. Fetch package info
2. Determine groupId, artifactId, releaseVersion
3. Deploy dependencies recursively (if `deployDependencies` is true)
4. Check that WebJar hasn't already been deployed (fetches POM from Maven Central; skip if `force` is true)
5. Resolve licenses (with optional override)
6. Convert dependencies and optional dependencies to Maven format
7. Resolve source URL via `SourceLocator`
8. Generate POM XML from template
9. Fetch archive from package source
10. Determine excludes, path prefix, base dir glob
11. Create WebJar JAR via `WebJarCreator`
12. Publish to Maven Central via `MavenCentralDeployer`
13. Stream progress messages throughout

**Fork deploy:** Creates a Heroku dyno running the `deploy` CLI command, streams output via rendezvous protocol. Triggered when `deploy.fork` config is true and `preventFork` is false.

**Create (no publish):** Same as steps 1-11 of local deploy, returns JAR bytes. Supports `licenseOverride` and `groupIdOverride`.

**Create Classic:** Accepts `.properties` text body, parses metadata, builds JAR from metadata without going through the full Deployable pipeline.

**CLI entry point:** `DeployWebJar.deploy` main method — interactive or argument-based. Bootstraps a Play application via GuiceApplicationBuilder.

### 4.6 MavenCentralDeployer

Publishes WebJar artifacts to Maven Central (Sonatype).

**Bundle creation:**
1. Generate GPG signatures (`.asc`) for both JAR and POM using BouncyCastle
2. Generate SHA1 and MD5 checksums for JAR and POM
3. Package all files into a ZIP bundle:
   - `{path}.jar`, `.jar.sha1`, `.jar.md5`, `.jar.asc`
   - `{path}.pom`, `.pom.sha1`, `.pom.md5`, `.pom.asc`
4. Upload via `zio-mavencentral` library's `Deploy.uploadVerifyAndPublish` (or `Deploy.upload` only if `oss.disable-deploy` is true)

**GPG key:** Base64-encoded PGP secret key ring from `oss.gpg-key` config. Passphrase from `oss.gpg-pass`.

**Authentication:** Sonatype username/password from `oss.deploy.username` / `oss.deploy.password`.

### 4.7 MavenCentralWebJars

Manages the cached index of all WebJars from Maven Central.

**Interface:**
```
trait MavenCentralWebJars:
  fetchPom(gav): Future[Elem]
  featuredWebJars(groupId, limit): Future[Seq[WebJar]]
  searchWebJars(groupId, query: Option[String]): Future[Seq[WebJar]]
```

**Cache refresh cycle** (runs every 1 hour, started on construction unless in Test mode):
1. For each group ID, fetch artifact list from Maven Central
2. Filter out known non-WebJar artifacts (`webjars-*`, `bower`, `bowergithub`, `2.11.2`)
3. Identify missing artifacts (not in Redis cache) and refresh them
4. Identify updated artifacts (using `MavenCentral.isModifiedSince`) and refresh them
5. For each refreshed artifact: fetch versions, fetch POM for latest version to get name and source URL, store in Redis
6. Asynchronously (forked daemon) fill in `numFiles` for each version via the file service

**Dev mode limit:** Configurable via `mavencentral.limit` or defaults to 5 in Dev mode.

**Artifact filtering from Maven Central:** Excludes artifacts where ID starts with `webjars-`, equals `bower`, `bowergithub`, or `2.11.2`.

### 4.8 WebJarsCache (Redis/Valkey)

Stores WebJar metadata in Redis using hash maps. One hash per groupId, with artifactId as field key.

```
WebJarMeta
  name: String
  sourceUrl: String
  versions: List[WebJarVersion]
  lastModified: ZonedDateTime
```

**Serialization:** Protocol Buffers via `zio-schema` codec.

**Operations:**
- `getArtifact(groupArtifact)` — single artifact lookup
- `getArtifacts(groupId, limit?, query?)` — list all artifacts, optionally filtered by query (case-insensitive match on artifactId or name) and limited
- `setArtifactDetails(groupArtifact, meta)` — store/update artifact
- `updateVersion(groupArtifact, version, numFiles)` — update file count for a specific version

### 4.9 Valkey (Redis Connection)

Manages the Redis/Valkey connection lifecycle.

**Connection configuration:**
- URL from `REDIS_URL` environment variable (format: `redis://:[password]@host:port`)
- Fallback: `localhost:6379`
- SSL enabled, certificate verification disabled
- Authentication with password extracted from URI user info

**Authentication daemon:** Re-authenticates every 5 seconds to handle connection drops (catches `NOAUTH` errors).

**Lifecycle:** Connection scope is closed on application stop via `ApplicationLifecycle`.

### 4.10 Cache (Caffeine)

In-memory cache with stale fallback.

**Behavior:**
1. On cache miss: call `onMiss`, store result with specified expiration, also store with 2x expiration as stale backup
2. On cache hit: return cached value
3. On `onMiss` failure: return stale cached value if available, otherwise propagate failure
4. Uses a sentinel string to track whether the primary cache entry has expired

### 4.11 WebJarCreator

Creates OSGi-compliant JAR files from package archives.

**JAR structure:**
```
META-INF/MANIFEST.MF                                    -- OSGi manifest
META-INF/maven/{groupId}/{artifactId}/pom.xml           -- Maven POM
META-INF/maven/{groupId}/{artifactId}/pom.properties    -- Maven properties
META-INF/resources/webjars/{pathPrefix}/...             -- WebJar content files
```

**Manifest headers:**
- `Bundle-Description`, `Bundle-License`, `Bundle-SymbolicName`, `Bundle-Name`, `Bundle-Version`, `Bundle-ManifestVersion`
- `Build-Jdk-Spec: 1.8`, `Created-By: webjars.org`

**File filtering:**
- `maybeBaseDirGlob`: removes the base directory prefix from archive entries (e.g. `*/` strips the top-level directory)
- `excludes`: uses `.gitignore`-style pattern matching via JGit's `IgnoreNode`

**Path prefix:** Default is `{artifactId}/{releaseVersion}/`

### 4.12 Git

Git repository operations using JGit.

**Functions:**
- `isGit(name)` — returns true if name contains `/` and doesn't start with `@`
- `gitUrl(repo)` — resolves git URLs: normalizes protocols, follows redirects, converts shorthand (`github:`, `git://` → `https://`)
- `artifactId(nameOrUrlish)` — converts git URLs to artifact IDs by replacing non-word chars with `-`
- `versions(repo)` — lists remote tags via `git ls-remote`
- `versionsOnBranch(repo, branch)` — lists commit SHAs on a branch (abbreviated to 10 chars)
- `cloneOrCheckout(repo, version)` — clones repo (or fetches if already cached), hard-resets to specified version. Retries once on failure by deleting and re-cloning
- `file(repo, version, filename)` — reads a file from a checked-out repo
- `tar(repo, version, excludes)` — creates tar archive of checked-out repo, excluding `.git` and specified dirs

**Cache directory:** Temporary directory, deleted on JVM exit.

### 4.13 GitHub

GitHub API integration.

**Functions:**
- `currentUrls(url)` — follows redirects to get current homepage, git URL, and issues URL. Cached for 1 day
- `raw(gitHubUrl, version, filename)` — fetches raw file content: `{url}/raw/{version}/{filename}`
- `tags(repo)` — fetches all tags with pagination (`Link` header)
- `allPages(path)(mapFunction)` — paginated GitHub API helper

**URL utilities (companion object):**
- `gitHubUrl(url)` — extracts normalized `https://github.com/{org}/{repo}` from various GitHub URL formats
- `gitHubGitUrl(url)` — appends `.git` to GitHub URL
- `gitHubIssuesUrl(url)` — appends `/issues` to GitHub URL

**Authentication:** Optional `github.auth.token` config, used as Bearer token.

### 4.14 Heroku

Fork-based deployment via Heroku API.

**`dynoCreate(app, command, size)`:**
1. `POST /apps/{app}/dynos` with `{attach: true, command, size}`
2. Extract `attach_url` from response
3. Connect via rendezvous protocol

**Rendezvous protocol:**
1. Parse URL to get host, port, and secret (URL path)
2. Connect via TLS TCP socket
3. Send secret as first message
4. Stream response lines (drop initial "rendezvous" line)

**Default dyno size:** `Standard-2X`

### 4.15 SemVer

Semantic version range operations via external service at `https://semver.webjars.org`.

**Functions:**
- `validRange(s)` — validates and normalizes a semver range string
- `maxSatisfying(versions, range)` — finds the maximum version satisfying a range. Partitions into batches of 256 if the version set is too large for a single query string

**`SemVer.toMaven(s)`** (static) — converts semver ranges to Maven version ranges:
- `>=X` → `[X,)`
- `>X` → `(X,)`
- `<X` → `(,X)`
- `<=X` → `(,X]`
- `||` (OR) → comma-separated Maven ranges wrapped in brackets
- Empty or `*` → `>=0`

### 4.16 Maven

Converts NPM dependencies to Maven format.

**`convertNpmDependenciesToMaven`:**
- Handles tarball URLs (extracts host+path as artifactId, tarball version)
- Handles git URLs (derives artifactId from URL, resolves version from tags or branch commits)
- Handles bare package names (encodes as artifactId)
- Converts semver ranges to Maven ranges via `SemVer.validRange` + `SemVer.toMaven`

### 4.17 LicenseDetector

Detects licenses from file contents.

**External service:** `POST https://oss-license-detector.herokuapp.com/` with license file content as body.

**URL license detection:**
- For GitHub URLs: rewrites to `raw.githubusercontent.com` (strips `/blob/` from path)
- Fetches content, verifies `Content-Type` starts with `text/`
- Posts content to license detection service

**Typical license files:** `LICENSE`, `LICENSE.txt`, `license.md`, `LICENSE-MIT`

### 4.18 SourceLocator

Resolves source repository URLs by normalizing to HTTPS and following redirects.

### 4.19 WebJarsFileService

External microservice integration for file listing.

**Base URL:** Configurable via `webjars.file-service.url` (default: `https://webjars-file-service.herokuapp.com`)

**Endpoints called:**
- `GET /listfiles/{groupId}/{artifactId}/{version}` → `List[String]`
- `GET /numfiles/{groupId}/{artifactId}/{version}` → `Int`

### 4.20 Adapter (ZIO-to-Future Bridge)

```
extension [R, E <: Throwable, A] (zio: ZIO[R, E, A])
  def runToFuture(layer: ZLayer[Any, Nothing, R]): Future[A]
```

Runs a ZIO effect using the default runtime, providing the given layer.

```
extension (ws: WSClient)
  def resolveRedir(httpUrl: String): Future[String]
```

Follows HTTP redirects (301, 302) recursively until 200 is reached. Handles relative redirects.

### 4.21 ArchiveCreator

Creates TAR archives from directories, excluding specified files/directories. Uses Apache Commons Compress with POSIX long file mode.

### 4.22 Bitbucket

URL utilities for Bitbucket repositories:
- `bitbucketUrl(url)` — validates and normalizes Bitbucket URLs
- `bitbucketIssuesUrl(url)` — appends `/issues` to Bitbucket URL

---

## 5. POM Template

Generated from `app/templates/pom.scala.xml` with parameters:
- `groupId`, `artifactId`, `version`
- `PackageInfo` (name, URLs, licenses, dependencies)
- `sourceUrl`
- `mavenDependencies`, `optionalMavenDependencies`
- `licenses`

---

## 6. Views & Frontend

### 6.1 Layout

- `main.scala.html` — base layout with head, navbar, content, footer
- `navbar.scala.html` — navigation bar
- `footer.scala.html` — page footer
- `icons.scala.html` — SVG icon definitions

### 6.2 Pages

- `index.scala.html` — home page with hero section and popular WebJars grid
- `all.scala.html` — browse all WebJars
- `documentation.scala.html` — framework integration docs
- `filelist.scala.html` — file listing for a WebJar version

### 6.3 Partials & Sections

- `hero.scala.html` — hero section with search
- `popular.scala.html` — popular WebJars grid
- `webJarList.scala.html` — WebJar list component (accepts `Either[Seq[WebJar], String]` for data or error)
- `newWebJarModal.scala.html` — modal for deploying a new WebJar
- `fileListModal.scala.html` — modal for browsing files

### 6.4 Documentation Partials

Framework-specific integration guides: Spring Boot, Spring MVC, Grails, Tapestry, Servlet 2, Servlet 3, JSF, Dropwizard, Play Framework, Wicket, Ring, Dandelion, Xitrum, Pippo, Vert.x, Ktor, Quarkus, and general usage.

### 6.5 Frontend Assets

**JavaScript:**
- `index.js` — home page functionality (search, filtering, WebJar list interactions)
- `docs.js` — documentation page interactions
- `color-modes.js` — light/dark theme switching

**CSS:**
- `main.scss` — custom styles with Bootstrap 5 customization

**Client-side dependencies (served via WebJars Play plugin):**
- jQuery 3.7.1
- Bootstrap 5.3.3 (via `select2-bootstrap-5-theme`)
- Select2 4.0.13
- HighlightJS 11.11.1
- jquery.typewatch 2.1.0

---

## 7. Caching Architecture

### 7.1 In-Memory (Caffeine)

Used for short-lived caching with stale fallback:
- Classic metadata: 1 hour
- Package versions: 1 hour
- GitHub URL resolution: 1 day

### 7.2 Redis/Valkey

Used for the WebJar index:
- Stores all artifact metadata (name, sourceUrl, versions, lastModified) as hash fields
- Protobuf serialization via `zio-schema`
- Refreshed every 1 hour by background daemon

### 7.3 HTTP ETags

Applied to list endpoints (`/`, `/all`, `/popular`, `/list/:groupId`):
- MurmurHash3 of the response data
- `304 Not Modified` on match
- Disabled in Dev mode

---

## 8. Background Jobs

### 8.1 Maven Central Index Refresh

- **Trigger:** On application start (except Test mode)
- **Schedule:** Every 1 hour via `ZIO.repeat(Schedule.spaced(1.hour))`
- **Process:** For each groupId, refresh missing and updated artifacts in Redis cache
- **File count filling:** Forked as daemon, does not block the main refresh

### 8.2 Redis Auth Daemon

- Re-authenticates to Redis every 5 seconds
- Handles `NOAUTH` errors from connection drops

---

## 9. Configuration

### 9.1 Environment Variables

| Variable | Purpose |
|----------|---------|
| `APPLICATION_SECRET` | Play HTTP secret key |
| `PORT` | HTTP port (Heroku) |
| `REDIS_URL` | Redis/Valkey connection URL (`redis://:[password]@host:port`) |
| `GITHUB_TOKEN` | GitHub API authentication token |
| `OSS_GPG_KEY` | Base64-encoded PGP secret key ring |
| `OSS_GPG_PASS` | GPG key passphrase |
| `OSS_DEPLOY_USERNAME` | Sonatype/Maven Central username |
| `OSS_DEPLOY_PASSWORD` | Sonatype/Maven Central password |
| `OSS_DISABLE_DEPLOY` | If true, upload only (no verify/publish) |
| `HEROKU_API_KEY` | Heroku API key for fork deployment |
| `DEPLOY_HEROKU_APP` | Heroku app name for fork deployment (default: `webjars-test`) |
| `DEPLOY_FORK` | Enable fork-based deployment |
| `WEBJARS_FILE_SERVICE_URL` | File service URL (default: `https://webjars-file-service.herokuapp.com`) |
| `USE_WEBJARS_CDN` | Enable CDN for WebJar assets |
| `MEMCACHEDCLOUD_SERVERS` | Legacy memcached servers |
| `MEMCACHEDCLOUD_USERNAME` | Legacy memcached username |
| `MEMCACHEDCLOUD_PASSWORD` | Legacy memcached password |

### 9.2 Application Config Defaults

- HTTP timeouts (WS client): 5 minutes connection and idle
- Trusted proxies: all (`0.0.0.0/0`, `::/0`)
- GZip chunked threshold: 1MB
- Allowed hosts filter: disabled
- Language: `en`
- File service URL: `https://webjars-file-service.herokuapp.com`
- Deploy Heroku app: `webjars-test`

---

## 10. Production Deployment

### 10.1 Procfile

**Web process:**
```
web: target/universal/stage/bin/webjars -XX:+PrintFlagsFinal -J-Xmx512m -Dhttp.port=${PORT}
     -Dplay.filters.enabled.0=play.filters.https.RedirectHttpsFilter
     -Dplay.filters.enabled.1=play.filters.gzip.GzipFilter
     -Dplay.filters.enabled.2=play.filters.csrf.CSRFFilter
     -Dplay.filters.enabled.3=play.filters.headers.SecurityHeadersFilter
```

**Deploy process:**
```
deploy: unset JAVA_TOOL_OPTIONS; target/universal/stage/bin/deploy
```

### 10.2 Production Filters

1. `RedirectHttpsFilter` — redirect HTTP to HTTPS
2. `GzipFilter` — response compression
3. `CSRFFilter` — CSRF protection for form submissions
4. `SecurityHeadersFilter` — security headers

### 10.3 Asset Pipeline

- GZIP compression (`gzip` stage)
- Content digest for cache busting (`digest` stage)

---

## 11. External Service Dependencies

| Service | Base URL | Purpose |
|---------|----------|---------|
| Maven Central | via `zio-mavencentral` library | Search artifacts, fetch POMs, publish |
| NPM Registry | `https://registry.npmjs.org` | Package metadata and tarballs |
| GitHub API | `https://api.github.com` | Repository info, tags, license, raw files |
| GitHub Raw | `https://raw.githubusercontent.com` | Raw file content |
| Semver Service | `https://semver.webjars.org` | Version range validation and resolution |
| License Detector | `https://oss-license-detector.herokuapp.com` | License detection from file content |
| File Service | `https://webjars-file-service.herokuapp.com` | File listing and counting |
| Heroku API | `https://api.heroku.com` | Fork deployment (dyno creation) |
| Sonatype | via `zio-mavencentral` `Deploy` | Maven Central publishing |

---

## 12. Library Dependencies

| Library | Purpose |
|---------|---------|
| `zio-mavencentral` | Maven Central search, POM fetching, deployment |
| `zio-redis` | Redis/Valkey client |
| `zio`, `zio-http` | Async effects, HTTP client (used by zio-mavencentral) |
| `zio-schema` | Protobuf serialization for Redis cache |
| `scala-uri` | URL parsing and manipulation |
| `org.eclipse.jgit` | Git operations (clone, fetch, reset, ls-remote) |
| `bcpg-jdk18on`, `bcutil-jdk18on` | BouncyCastle GPG signing |
| `hasher` | MD5 and SHA1 checksums |
| `commons-compress` | Archive reading/writing (JAR, TAR, ZIP) |
| `fastparse` | Parser combinators (used by scala-uri) |
| `caffeine` | In-memory caching |
| `specs2` | Test framework |
| `testcontainers-scala-core` | Integration testing with containers |

---

## 13. Test Coverage

Test suites exist for:
- `ApplicationSpec` — controller integration tests
- `NPMSpec` — NPM package operations
- `ClassicSpec` — Classic WebJar operations
- `GitSpec` — Git operations
- `GitHubSpec` — GitHub API operations
- `MavenSpec` — NPM-to-Maven dependency conversion
- `SemVerSpec` — Semantic version operations
- `CacheSpec` — Caffeine cache behavior
- `WebJarsCacheSpec` — Redis cache operations
- `WebJarsFileServiceSpec` — File service integration
- `MavenCentralWebJarsSpec` — Maven Central index refresh
- `MavenCentralDeployerSpec` — Maven Central publishing
- `DeployWebJarSpec` — Deployment pipeline
- `LicenseDetectorSpec` — License detection
- `HerokuSpec` — Heroku fork deployment
- `WebJarCreatorSpec` — JAR creation
- `ArchiveCreatorSpec` — Archive creation
- `SourceLocatorSpec` — Source URL resolution
- `WebJarVersionOrderingSpec` — Version string comparison
- `GlobalApplication` — Shared test application setup
