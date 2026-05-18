package webjars.routes

import zio.http.*

object StaticAssets:

  private def resource(path: String): Handler[Any, Nothing, Request, Response] =
    Handler.fromResource(path).catchAll(_ => Handler.notFound)

  // /assets/* serves public/* off the classpath; favicon/robots are aliased
  // explicitly. /webjars/* serving lives in test scope (the WebJar JARs are
  // only on the test classpath; see WebJarsTestApp + TestStaticAssets).
  val routes: Routes[Any, Nothing] = Routes(
    Method.GET / "favicon.ico"             -> resource("public/favicon.ico"),
    Method.GET / "robots.txt"              -> resource("public/robots.txt"),
    Method.GET / "files" / "robots.txt"    -> resource("public/robots.txt"),
  ) ++ Routes.serveResources(Path.empty / "assets", "public")
