package webjars.utils

import zio.http.URL

import java.util.Locale
import scala.util.matching.Regex

/** Detects short license statements such as `License: MIT`,
 *  `Licensed under MIT.`, `* License: [MIT](https://…)` or a `## License`
 *  heading followed by `MIT © Someone`.
 *
 *  Deliberately strict: only a small set of SPDX ids is accepted, the id must
 *  appear in a statement-shaped line (not anywhere in prose), and a file that
 *  states more than one distinct license yields nothing. */
object ReadmeLicense:

  private val spdxIds: Map[String, String] =
    Seq("MIT", "ISC", "Apache-2.0", "BSD-2-Clause", "BSD-3-Clause")
      .map(id => id.toLowerCase(Locale.ROOT) -> id)
      .toMap

  // A trailing sentence period must not become part of the id ("MIT." -> "MIT").
  private val id = """([A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)*)"""
  private val linkedId = s"""(?:\\[$id\\]\\((https?://[^)\\s]+)\\)|$id)"""
  private val licenseWord = """(?:license|licence)"""
  private val tail = s"""(?:\\s+$licenseWord)?\\s*\\.?\\s*"""

  private val labelled: Regex = s"""(?i)$licenseWord\\s*[:\\-–]\\s*$linkedId$tail""".r
  private val licensedUnder: Regex = s"""(?i)licen[cs]ed\\s+under\\s+(?:the\\s+)?$linkedId$tail""".r
  private val heading: Regex = s"""(?i)#{1,6}\\s*$licenseWord\\s*#*""".r
  private val afterHeading: Regex = s"""(?i)$linkedId(?:\\s+$licenseWord)?\\s*(?:\\.|(?:©|\\(c\\)|copyright).*)?""".r

  /** Strips list markers and emphasis so `* **License:** MIT` reads as `License: MIT`. */
  private def normalize(line: String): String =
    line.trim.stripPrefix("* ").stripPrefix("- ").stripPrefix("+ ").replace("**", "").replace("__", "").trim

  private def toLicense(name: String, maybeUrl: Option[String]): Option[License] =
    spdxIds.get(name.toLowerCase(Locale.ROOT)).map { spdx =>
      maybeUrl.flatMap(URL.parseOption).fold[License](LicenseWithName(spdx))(LicenseWithNameAndUrl(spdx, _))
    }

  private def fromMatch(linkedName: String | Null, url: String | Null, plainName: String | Null): Option[License] =
    Option(linkedName).orElse(Option(plainName)).flatMap(toLicense(_, Option(url)))

  private def statement(line: String): Option[License] = line match
    case labelled(linkedName, url, plainName)      => fromMatch(linkedName, url, plainName)
    case licensedUnder(linkedName, url, plainName) => fromMatch(linkedName, url, plainName)
    case _                                         => None

  private def headingStatements(lines: List[String]): List[License] =
    lines.zipWithIndex.collect {
      case (line, index) if heading.matches(line) =>
        lines.drop(index + 1).find(_.nonEmpty).flatMap {
          case afterHeading(linkedName, url, plainName) => fromMatch(linkedName, url, plainName)
          case other                                    => statement(other)
        }
    }.flatten

  def detect(text: String): Option[License] =
    val lines = text.linesIterator.map(normalize).toList
    val found = (lines.flatMap(statement) ++ headingStatements(lines)).distinctBy(_.maybeName)
    found match
      case List(license) => Some(license)
      case _             => None
