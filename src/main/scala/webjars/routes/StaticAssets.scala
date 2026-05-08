package webjars.routes

import zio.*
import zio.http.*

object StaticAssets:

  private val mimeTypes = Map(
    "css" -> MediaType.text.css,
    "js" -> MediaType.application.javascript,
    "png" -> MediaType.image.png,
    "ico" -> MediaType.image.`x-icon`,
    "svg" -> MediaType.image.`svg+xml`,
    "html" -> MediaType.text.html,
    "txt" -> MediaType.text.plain,
    "json" -> MediaType.application.json,
    "woff" -> MediaType.application.`font-woff`,
    "woff2" -> MediaType.application.`font-woff`,
    "ttf" -> MediaType.application.`octet-stream`,
    "eot" -> MediaType.application.`octet-stream`,
    "map" -> MediaType.application.json,
    "gif" -> MediaType.image.gif,
    "jpg" -> MediaType.image.jpeg,
    "jpeg" -> MediaType.image.jpeg,
  )

  private def mediaTypeFor(path: String): MediaType =
    val ext = path.split('.').lastOption.getOrElse("").toLowerCase
    mimeTypes.getOrElse(ext, MediaType.application.`octet-stream`)

  val routes: Routes[Any, Nothing] = Routes(
    // Static assets from public/
    Method.GET / "assets" / trailing -> handler { (path: Path, _: Request) =>
      val resourcePath = s"public/${path.dropLeadingSlash}"
      val maybeResource = Option(getClass.getClassLoader.getResourceAsStream(resourcePath))
      maybeResource.fold(Response.notFound) { inputStream =>
        val bytes = inputStream.readAllBytes()
        inputStream.close()
        Response(
          Status.Ok,
          Headers(Header.ContentType(mediaTypeFor(resourcePath)).untyped),
          Body.fromChunk(Chunk.fromArray(bytes))
        )
      }
    },

    // favicon.ico
    Method.GET / "favicon.ico" -> handler { (_: Request) =>
      val maybeResource = Option(getClass.getClassLoader.getResourceAsStream("public/favicon.ico"))
      maybeResource.fold(Response.notFound) { inputStream =>
        val bytes = inputStream.readAllBytes()
        inputStream.close()
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.image.`x-icon`).untyped),
          Body.fromChunk(Chunk.fromArray(bytes))
        )
      }
    },

    // robots.txt
    Method.GET / "robots.txt" -> handler { (_: Request) =>
      val maybeResource = Option(getClass.getClassLoader.getResourceAsStream("public/robots.txt"))
      maybeResource.fold(Response.notFound) { inputStream =>
        val bytes = inputStream.readAllBytes()
        inputStream.close()
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.text.plain).untyped),
          Body.fromChunk(Chunk.fromArray(bytes))
        )
      }
    },

    Method.GET / "files" / "robots.txt" -> handler { (_: Request) =>
      val maybeResource = Option(getClass.getClassLoader.getResourceAsStream("public/robots.txt"))
      maybeResource.fold(Response.notFound) { inputStream =>
        val bytes = inputStream.readAllBytes()
        inputStream.close()
        Response(
          Status.Ok,
          Headers(Header.ContentType(MediaType.text.plain).untyped),
          Body.fromChunk(Chunk.fromArray(bytes))
        )
      }
    },

    // WebJars from classpath META-INF/resources/webjars/
    Method.GET / "webjars" / trailing -> handler { (path: Path, _: Request) =>
      val resourcePath = s"META-INF/resources/webjars/${path.dropLeadingSlash}"
      val maybeResource = Option(getClass.getClassLoader.getResourceAsStream(resourcePath))
      maybeResource.fold(Response.notFound) { inputStream =>
        val bytes = inputStream.readAllBytes()
        inputStream.close()
        Response(
          Status.Ok,
          Headers(Header.ContentType(mediaTypeFor(resourcePath)).untyped),
          Body.fromChunk(Chunk.fromArray(bytes))
        )
      }
    },
  )
