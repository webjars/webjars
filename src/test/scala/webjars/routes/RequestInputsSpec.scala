package webjars.routes

import zio.http.{Request, URL}
import zio.test.*

object RequestInputsSpec extends ZIOSpecDefault:

  private def request(path: String): Request =
    Request.get(URL.decode(path).toOption.get)

  def spec = suite("RequestInputs")(
    test("deployment query parses and trims all required values") {
      val parsed = DeploymentQuery.parse(request("/deploy?webJarType=npm&nameOrUrlish=%20flag-icons%20&version=%207.5.0%20"))
      assertTrue(parsed.exists { query =>
        query.webJarType.value == "npm" &&
        query.nameOrUrlish.value == "flag-icons" &&
        query.version.value == "7.5.0"
      })
    },
    test("deployment query rejects a missing version") {
      val parsed = DeploymentQuery.parse(request("/deploy?webJarType=npm&nameOrUrlish=flag-icons"))
      assertTrue(parsed == Left(RequestInputError("Query parameter 'version' is required and must not be blank")))
    },
    test("deployment query rejects a blank package") {
      val parsed = DeploymentQuery.parse(request("/deploy?webJarType=npm&nameOrUrlish=%20%20&version=7.5.0"))
      assertTrue(parsed == Left(RequestInputError("Query parameter 'nameOrUrlish' is required and must not be blank")))
    },
    test("package query rejects an empty name") {
      val parsed = PackageQuery.parse(request("/exists?webJarType=npm&name="))
      assertTrue(parsed == Left(RequestInputError("Query parameter 'name' is required and must not be blank")))
    },
    test("classic create query requires both name and version") {
      val parsed = VersionedNameQuery.parse(request("/create/classic?nameOrUrlish=jquery-ui&version=%20"))
      assertTrue(parsed == Left(RequestInputError("Query parameter 'version' is required and must not be blank")))
    },
  )
