package webjars.routes

import zio.http.Request

final case class RequestInputError(message: String)

opaque type NonBlankQueryParam = String

object NonBlankQueryParam:
  def parse(request: Request, name: String): Either[RequestInputError, NonBlankQueryParam] =
    request.url.queryParams.getAll(name).headOption
      .map(_.trim)
      .filter(_.nonEmpty)
      .toRight(RequestInputError(s"Query parameter '$name' is required and must not be blank"))

  extension (param: NonBlankQueryParam)
    def value: String = param

final case class PackageQuery(webJarType: NonBlankQueryParam, name: NonBlankQueryParam)

object PackageQuery:
  def parse(request: Request): Either[RequestInputError, PackageQuery] =
    for
      webJarType <- NonBlankQueryParam.parse(request, "webJarType")
      name       <- NonBlankQueryParam.parse(request, "name")
    yield PackageQuery(webJarType, name)

final case class DeploymentQuery(
  webJarType: NonBlankQueryParam,
  nameOrUrlish: NonBlankQueryParam,
  version: NonBlankQueryParam,
)

object DeploymentQuery:
  def parse(request: Request): Either[RequestInputError, DeploymentQuery] =
    for
      webJarType   <- NonBlankQueryParam.parse(request, "webJarType")
      nameOrUrlish <- NonBlankQueryParam.parse(request, "nameOrUrlish")
      version      <- NonBlankQueryParam.parse(request, "version")
    yield DeploymentQuery(webJarType, nameOrUrlish, version)

final case class VersionedNameQuery(nameOrUrlish: NonBlankQueryParam, version: NonBlankQueryParam)

object VersionedNameQuery:
  def parse(request: Request): Either[RequestInputError, VersionedNameQuery] =
    for
      nameOrUrlish <- NonBlankQueryParam.parse(request, "nameOrUrlish")
      version      <- NonBlankQueryParam.parse(request, "version")
    yield VersionedNameQuery(nameOrUrlish, version)
