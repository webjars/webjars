package webjars.views

import zio.http.template2.*

object FileListPage:

  private def pathPrefix(file: String): String =
    file.split("/").drop(5).mkString("/")

  def apply(groupId: String, artifactId: String, version: String, fileList: List[String]): Dom =
    Dom.Fragment(fileList.toVector.flatMap: file =>
      Vector(
        a(
          href   := s"//cdn.jsdelivr.net/webjars/$groupId/$artifactId/$version/${pathPrefix(file)}",
          target := "_blank",
          // nofollow: crawlers that harvest these CDN links fan out into a
          // request per file against jsDelivr, which falls through to
          // webjars-file-service and has overwhelmed it.
          rel    := "nofollow noopener",
          file,
        ),
        br,
      )
    )
