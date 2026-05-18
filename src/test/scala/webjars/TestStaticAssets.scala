package webjars

import zio.http.*

// Serves /webjars/<artifact>/<version>/<file> from classpath
// META-INF/resources/webjars/* — used by WebJarsTestApp so the dev/test
// bundle can resolve the WebJar JARs that are only on the test classpath
// (see build.sbt: `% Set(WebJar, Test, ...)`). Not part of the production
// build; prod serves WebJar assets via the configured CDN.
object TestStaticAssets:
  val routes: Routes[Any, Nothing] =
    Routes.serveResources(Path.empty / "webjars", "META-INF/resources/webjars")
