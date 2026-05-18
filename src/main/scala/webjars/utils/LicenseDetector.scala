package webjars.utils

import webjars.config.AppConfig
import zio.*
import zio.direct.*
import zio.http.*

trait LicenseDetector:
  def licenseDetect(contents: String): ZIO[Scope, Throwable, LicenseWithName]
  def licenseDetect(url: URL): ZIO[Scope, Throwable, LicenseWithNameAndUrl]
  val typicalLicenseFiles: Set[String]

case class LicenseDetectorLive(client: Client, maybeGitHubToken: Option[String] = None) extends LicenseDetector:

  def licenseDetect(contents: String): ZIO[Scope, Throwable, LicenseWithName] =
    defer:
      val req = Request.post(
        URL.unsafeParse("https://oss-license-detector.herokuapp.com/"),
        Body.fromString(contents)
      ).addHeader(Header.ContentType(MediaType.text.plain))
      val response = client.batched(req).run
      response.status match
        case Status.Ok =>
          LicenseWithName(response.body.asString.run)
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(body)).run

  def licenseDetect(url: URL): ZIO[Scope, Throwable, LicenseWithNameAndUrl] =
    val isGitHub = url.host.exists(_.endsWith("github.com"))
    val rawLicenseUrl = if isGitHub then
      val filteredPath = url.path.segments.filter(_ != "blob").foldLeft(Path.root)(_ / _)
      url.host("raw.githubusercontent.com").path(filteredPath)
    else
      url

    val baseRequest = Request.get(rawLicenseUrl)
    val request = if rawLicenseUrl.host.exists(_.endsWith("github.com")) then
      maybeGitHubToken.fold(baseRequest)(token =>
        baseRequest.addHeader(Header.Authorization.Bearer(token))
      )
    else
      baseRequest

    defer:
      val response = client.batched(request).run
      response.status match
        case Status.Ok if response.header(Header.ContentType).exists(_.mediaType.mainType == "text") =>
          val body = response.body.asString.run
          val licenseWithName = licenseDetect(body).run
          LicenseWithNameAndUrl(licenseWithName.name, url)
        case Status.Ok =>
          ZIO.fail(new Exception(s"License at ${url.encode} was not plain text")).run
        case _ =>
          val body = response.body.asString.run
          ZIO.fail(new Exception(s"Could not fetch license at ${url.encode} - response was: $body")).run

  val typicalLicenseFiles: Set[String] = Set("LICENSE", "LICENSE.txt", "license.md", "LICENSE-MIT")

object LicenseDetector:
  val live: ZLayer[Client & AppConfig, Nothing, LicenseDetector] =
    ZLayer.fromFunction((client: Client, config: AppConfig) => LicenseDetectorLive(client, config.githubAuthToken))
