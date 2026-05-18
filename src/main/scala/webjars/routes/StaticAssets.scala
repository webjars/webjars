package webjars.routes

import zio.ZIO
import zio.http.*
import zio.http.codec.PathCodec.trailing

object StaticAssets:

  private def resource(path: String): Handler[Any, Nothing, Request, Response] =
    Handler.fromResource(path).catchAll(_ => Handler.notFound)

  // /assets/* serves public/* off the classpath; favicon/robots are aliased
  // explicitly. /webjars/* serving lives in test scope (the WebJar JARs are
  // only on the test classpath; see WebJarsTestApp + TestStaticAssets).
  // The hand-rolled /assets/* route (rather than Routes.serveResources) lets
  // us return 404 for missing resources; serveResources surfaces them as 500.
  val routes: Routes[Any, Nothing] = Routes(
    Method.GET / "favicon.ico"             -> resource("public/favicon.ico"),
    Method.GET / "robots.txt"              -> resource("public/robots.txt"),
    Method.GET / "files" / "robots.txt"    -> resource("public/robots.txt"),
    Method.GET / "assets" / trailing       -> handler { (subPath: Path, request: Request) =>
      val h: Handler[Any, Nothing, Request, Response] =
        Handler.fromResource(s"public/${subPath.dropLeadingSlash.encode}").catchAll(_ => Handler.notFound)
      ZIO.scoped(h(request))
    },
  )
