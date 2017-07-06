package controllers

import javax.inject.Inject

import org.webjars.play.routes.WebJarAssets
import org.webjars.play.WebJarsUtil
import play.api.Configuration

class ContentWebJarsUtil @Inject() (configuration: Configuration, webJarsUtil: WebJarsUtil) {

  lazy val maybeContentUrl: Option[String] = configuration.getOptional[String]("contentUrl")

  def getUrl(path: String): String = {
    val resolvedPath = webJarsUtil.locate(path)
    val atUrl = WebJarAssets.at(resolvedPath).url
    maybeContentUrl.fold(atUrl)(_ + atUrl)
  }

}

