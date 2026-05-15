package sbtsass

import de.larsgrefer.sass.embedded.importer.ClasspathImporter
import org.webjars.{NotFoundException, WebJarAssetLocator}

import java.net.URL

/**
 * Resolves SCSS `@import` against WebJar JARs on the given classloader.
 *
 * Two cases:
 *
 *  1. **Artifact-prefixed imports** like `@import "bootstrap/scss/functions"`
 *     from your own SCSS — first path segment is the WebJar artifact name.
 *     Resolved via [[WebJarAssetLocator.getFullPath]].
 *
 *  2. **Relative imports** between SCSS files within a WebJar — once
 *     case 1 returns a `jar:file:` URL, dart-sass resolves subsequent
 *     `@import "variables-dark"` against the containing jar URL, producing
 *     `jar:file:.../scss/variables-dark`. We probe SCSS suffix variants
 *     (`_name.scss`, `name.scss`, `name/_index.scss`, `name/index.scss`)
 *     on that URL.
 *
 * The bundled `WebjarsImporter` only handles exact filenames (no suffix
 * resolution), which is why we replace it.
 */
class WebJarsScssImporter(loader: ClassLoader) extends ClasspathImporter(loader) {
  private val locator = new WebJarAssetLocator(loader)

  override def canonicalizeUrl(url: String): URL = {
    if (url.startsWith("jar:") || url.startsWith("file:")) {
      probeUrl(url).orNull
    } else {
      val cleaned    = url.stripPrefix("classpath:").stripPrefix("/")
      val firstSlash = cleaned.indexOf('/')
      if (firstSlash > 0) {
        val artifact = cleaned.substring(0, firstSlash)
        val rest     = cleaned.substring(firstSlash + 1)
        val resolved = scssCandidates(rest).iterator.flatMap { variant =>
          try Option(locator.getFullPath(artifact, variant))
          catch { case _: NotFoundException => None }
        }.find(loader.getResource(_) != null)
        resolved.map(loader.getResource).orNull
      } else null
    }
  }

  /** Try SCSS suffix variants on an absolute URL (jar:/file:). */
  private def probeUrl(base: String): Option[URL] = {
    val (dir, name) = {
      val i = base.lastIndexOf('/')
      if (i < 0) ("", base) else (base.substring(0, i + 1), base.substring(i + 1))
    }
    val candidates = Seq(
      s"${dir}_${name}.scss",
      s"$dir$name.scss",
      s"$dir$name/_index.scss",
      s"$dir$name/index.scss",
    )
    candidates.iterator.flatMap { c =>
      try {
        val u    = new java.net.URI(c).toURL
        val conn = u.openConnection()
        conn.setUseCaches(false)
        val is = conn.getInputStream
        is.close()
        Some(u)
      } catch {
        case _: Throwable => None
      }
    }.toStream.headOption
  }

  /** Sass file-resolution order applied to a relative path. */
  private def scssCandidates(rel: String): Seq[String] = {
    val i = rel.lastIndexOf('/')
    val (dir, name) =
      if (i < 0) ("", rel)
      else (rel.substring(0, i + 1), rel.substring(i + 1))
    Seq(
      s"${dir}_${name}.scss",
      s"$dir$name.scss",
      s"$dir$name/_index.scss",
      s"$dir$name/index.scss",
    )
  }
}
