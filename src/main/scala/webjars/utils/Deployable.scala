package webjars.utils

import com.jamesward.zio_mavencentral.MavenCentral
import webjars.utils.Deployable.{NameOrUrlish, Version}
import zio.*
import zio.direct.*
import zio.http.URL
import zio.stream.*

import java.io.{FileNotFoundException, InputStream}

trait Deployable:

  val licenseDetector: LicenseDetector

  extension (s: Version)
    def vless: Version = s.stripPrefix("v").replace("^v", "^").replace("~v", "v")
    def vwith: Version = if (s.startsWith("v")) s else "v" + s

  val name: String

  val groupId: MavenCentral.GroupId

  def artifactId(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, MavenCentral.ArtifactId]

  def releaseVersion(maybeVersion: Option[Version], packageInfo: PackageInfo): MavenCentral.Version = MavenCentral.Version(maybeVersion.getOrElse(packageInfo.version).vless)

  def excludes(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[String]]

  val metadataFile: Option[String]

  def maybeBaseDirGlob(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Option[Version]]

  def pathPrefix(artifactId: MavenCentral.ArtifactId, releaseVersion: MavenCentral.Version, packageInfo: PackageInfo): String = s"$artifactId/$releaseVersion/"

  def info(nameOrUrlish: NameOrUrlish, version: Version, maybeSourceUri: Option[URL] = None): ZIO[Scope, Throwable, PackageInfo]

  def mavenDependencies(dependencies: Map[String, String]): ZIO[Scope, Throwable, Set[(MavenCentral.GroupArtifact, String)]]

  def archive(nameOrUrlish: NameOrUrlish, version: Version): ZIO[Scope, Throwable, InputStream]

  def file(nameOrUrlish: NameOrUrlish, version: Version, filename: String): ZIO[Scope, Throwable, String]

  def versions(nameOrUrlish: NameOrUrlish): ZIO[Scope, Throwable, Set[Version]]

  def licenses(nameOrUrlish: NameOrUrlish, version: Version, packageInfo: PackageInfo): ZIO[Scope, Throwable, Set[License]] =

    def tryToGetLicenseFromVariousFiles(files: Set[String]): ZIO[Scope, Throwable, License] =
      files.headOption.fold[ZIO[Scope, Throwable, License]](ZIO.fail(NoValidLicenses())) { licenseFile =>
        file(nameOrUrlish, version, licenseFile).flatMap(licenseDetector.licenseDetect).catchAll { _ =>
          tryToGetLicenseFromVariousFiles(files.tail)
        }
      }

    val normalizedLicenses = packageInfo.metadataLicenses.map {
      case LicenseMetadata.SpdxLicense(spdxLicense) =>
        val replacedDotSlash = if spdxLicense.startsWith("./") then
          spdxLicense.replace("./", "file://")
        else
          spdxLicense

        val replacedSeeLicenseIn = replacedDotSlash.replace("SEE LICENSE IN ", "file://")

        licenseReference(nameOrUrlish, version, replacedSeeLicenseIn)
      case LicenseMetadata.ProvidedLicense(licenseMetadata) =>
        ZIO.succeed(Set(licenseMetadata))
      case LicenseMetadata.UnresolvedLicense =>
        ZIO.succeed(Set.empty)
    }

    val resolvedLicenses = ZIO.foldLeft(normalizedLicenses)(Set.empty[License]) { (acc, zioSet) =>
      zioSet.map(acc ++ _)
    }

    resolvedLicenses
      .filterOrFail(_.nonEmpty)(new NoSuchElementException("No licenses resolved"))
      .catchAll { _ =>
        tryToGetLicenseFromVariousFiles(licenseDetector.typicalLicenseFiles).map(Set(_))
      }
      .filterOrFail(_.nonEmpty)(new NoSuchElementException("No licenses found"))
      .catchAll { _ =>
        ZIO.fail(LicenseNotFoundException(s"${this.name} - $nameOrUrlish $version"))
      }

  def archiveFile(nameOrUrlish: NameOrUrlish, version: Version, filename: String): ZIO[Scope, Throwable, String] =
    defer:
      val resource = archive(nameOrUrlish, version).run
      val maybeContent = WebJarCreator.unarchiveStream(ZStream.fromInputStream(resource))
        .filter(_._1 == filename)
        .take(1)
        .mapZIO { case (_, _, content) =>
          content.via(ZPipeline.utf8Decode).runCollect.map(_.mkString)
        }
        .runHead
        .run
      ZIO.fromOption(maybeContent)
        .orElseFail(new FileNotFoundException(s"Could not find $filename in archive for $nameOrUrlish $version"))
        .run

  def licenseReference(nameOrUrlish: NameOrUrlish, version: Version, license: String): ZIO[Scope, Throwable, Set[License]] =
    if license.startsWith("http://") || license.startsWith("https://") then
      licenseDetector.licenseDetect(URL.unsafeParse(license)).map(Set(_))
    else if license.startsWith("file://") then
      file(nameOrUrlish, version, license.stripPrefix("file://")).flatMap { fileContent =>
        licenseDetector.licenseDetect(fileContent)
      }.catchAll { _ =>
        ZIO.succeed(LicenseWithUrl(URL.unsafeParse(license)))
      }.map(Set(_))
    else if license.startsWith("(") && license.endsWith(")") && !license.contains("AND") then
      ZIO.succeed {
        license.stripPrefix("(").stripSuffix(")").replace(" or ", " OR ").split(" OR ").toSet.map(LicenseWithName(_))
      }
    else
      ZIO.succeed(Set(LicenseWithName(license)))

  def parseDep(nameAndVersionish: (NameOrUrlish, Version)): (NameOrUrlish, Version) =
    val (name, versionish) = nameAndVersionish

    if versionish.contains("/") then
      val urlish = versionish.takeWhile(_ != '#')
      val version = versionish.stripPrefix(urlish).stripPrefix("#").vless
      urlish -> version
    else if versionish.startsWith("npm:") then
      val name = versionish.stripPrefix("npm:").takeWhile(_ != '@')
      val version = versionish.stripPrefix(name).dropWhile(_ != '@').stripPrefix("@")
      name -> version
    else
      name -> versionish.vless

  def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String]): ZIO[Scope, Throwable, Map[String, String]]

object Deployable:
  type NameOrUrlish = String
  type Version = String

trait AllDeployables:
  def fromGroupId(groupId: MavenCentral.GroupId): Option[Deployable]
  def groupIds(): Set[MavenCentral.GroupId]
  def fromName(name: String): Option[Deployable]

case class AllDeployablesLive(classic: Classic, npm: NPM) extends AllDeployables:

  def fromGroupId(groupId: MavenCentral.GroupId): Option[Deployable] =
    if groupId == classic.groupId then Some(classic)
    else if groupId == npm.groupId then Some(npm)
    else None

  def groupIds(): Set[MavenCentral.GroupId] = Set(classic.groupId, npm.groupId)

  def fromName(name: String): Option[Deployable] =
    if name.equalsIgnoreCase(classic.name) then Some(classic)
    else if name.equalsIgnoreCase(npm.name) then Some(npm)
    else None

object AllDeployables:
  val live: ZLayer[Classic & NPM, Nothing, AllDeployables] = ZLayer.derive[AllDeployablesLive]
