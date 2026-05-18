package webjars.utils

import zio.http.URL

enum LicenseMetadata:
  case SpdxLicense(id: String)
  case ProvidedLicense(license: License)
  case UnresolvedLicense

sealed trait License:
  val maybeName: Option[String] = this match
    case LicenseWithName(name) => Some(name)
    case _: LicenseWithUrl => None
    case LicenseWithNameAndUrl(name, _) => Some(name)

  val maybeUrl: Option[URL] = this match
    case _: LicenseWithName => None
    case LicenseWithUrl(url) => Some(url)
    case LicenseWithNameAndUrl(_, url) => Some(url)

  override def toString: String =
    maybeName
      .orElse(maybeUrl.map(_.encode))
      .getOrElse(throw new Exception("License did not have a name or url"))

case class LicenseWithName(name: String) extends License
case class LicenseWithUrl(url: URL) extends License
case class LicenseWithNameAndUrl(name: String, url: URL) extends License

case class LicenseNotFoundException(message: String, cause: Exception = null) extends Exception(message, cause)
case class NoValidLicenses() extends Exception("no valid licenses found")
