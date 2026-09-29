package webjars.routes

import zio.*
import zio.http.*
import zio.test.*

object DeployGetGuardSpec extends ZIOSpecDefault:

  private val deployUrl = URL.decode("/deploy?webJarType=npm&nameOrUrlish=flag-icons&version=7.5.0").toOption.get

  def spec = suite("GET /deploy guard")(
    test("ordinary GET returns 405 without evaluating deployment") {
      for
        runs <- Ref.make(0)
        request = Request.get(deployUrl).addHeader(Header.Accept(MediaType.text.plain))
        response <- AppRoutes.guardDeployGet(request)(runs.update(_ + 1).as(Response.ok))
        runCount <- runs.get
      yield assertTrue(response.status == Status.MethodNotAllowed, runCount == 0)
    },
    test("EventSource GET evaluates deployment exactly once") {
      for
        runs <- Ref.make(0)
        request = Request.get(deployUrl).addHeader(Header.Accept(MediaType.text.`event-stream`))
        response <- AppRoutes.guardDeployGet(request)(runs.update(_ + 1).as(Response.ok))
        runCount <- runs.get
      yield assertTrue(response.status == Status.Ok, runCount == 1)
    },
  )
