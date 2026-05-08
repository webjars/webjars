package webjars.views

object FileListPage:

  private def pathPrefix(file: String): String =
    file.split("/").drop(5).mkString("/")

  def apply(groupId: String, artifactId: String, version: String, fileList: List[String]): String =
    fileList.map { file =>
      s"""<a href="//cdn.jsdelivr.net/webjars/$groupId/$artifactId/$version/${pathPrefix(file)}" target="_blank">$file</a><br/>"""
    }.mkString("\n")
