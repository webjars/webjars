package utils

import java.io.InputStream
import java.net.{URI, URL, URLEncoder}
import javax.inject.Inject

import play.api.http.Status
import play.api.libs.concurrent.Futures
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.libs.ws._
import utils.PackageInfo._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower @Inject() (ws: WSClient, git: Git, gitHub: GitHub, maven: Maven, npm: NPM) (implicit ec: ExecutionContext, futures: Futures) extends Deployable {

  import Bower._

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  override val name: String = "Bower"

  override val groupIdQuery: String = "org.webjars.bower"

  override def includesGroupId(groupId: String): Boolean = groupId.equalsIgnoreCase(groupIdQuery)

  override def groupId(nameOrUrlish: String, version: String): Future[String] = Future.successful(groupIdQuery)

  override def artifactId(nameOrUrlish: String, version: String): Future[String] = git.artifactId(nameOrUrlish)

  override def excludes(nameOrUrlish: String, version: String): Future[Set[String]] = {
    // todo: apply bower ignore in case of git repo
    Future.successful(Set(".bower.json"))
  }

  override val metadataFile: String = "bower.json"

  override val contentsInSubdir: Boolean = false

  override def pathPrefix(nameOrUrlish: String, releaseVersion: String, packageInfo: PackageInfo): Future[String] = {
    artifactId(nameOrUrlish, releaseVersion).map { artifactId =>
      s"$artifactId/$releaseVersion/"
    }
  }

  def bowerToMaven(keyValue: (String, String)): Future[(String, String, String)] = {
    val (name, version) = parseDep(keyValue)

    for {
      latestVersionInRange <- latestDep(name, version)
      groupId <- groupId(name, latestVersionInRange)
      artifactId <- artifactId(name, latestVersionInRange)
      version <- Future.fromTry(SemVer.convertSemVerToMaven(version))
    } yield (groupId, artifactId, version)
  }

  override def versions(packageNameOrGitRepo: String): Future[Set[String]] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap(git.versions)
    }
    else {
      val maybeName = Try {
        URLEncoder.encode(packageNameOrGitRepo, "UTF-8")
      }

      maybeName.toOption.fold[Future[Set[String]]] {
        Future.failed(new Exception("Could not encode the URL for the specified package"))
      } { name =>
        ws.url(s"$BASE_URL/info/$name").get().flatMap { response =>
          response.status match {
            case Status.OK =>
              val versions = (response.json \ "versions").as[Seq[String]]
              val cleanVersions = versions.filterNot(_.contains("sha"))
              Future.successful(cleanVersions.toSet)
            case _ =>
              Future.failed(new Exception(response.body))
          }
        }
      }
    } recoverWith {
      case _ => npm.versions(packageNameOrGitRepo)
    }
  }

  def rawInfo(packageNameOrGitRepo: String, version: String): Future[PackageInfo] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap { gitUrl =>
        git.versions(gitUrl).flatMap { versions =>

          def parseBowerJson(bowerJson: String): Future[PackageInfo] = {
            Future.fromTry {
              Try {

                // add the gitUrl into the json since it is not in the file, just the json served by the Bower index
                val json = Json.parse(bowerJson).as[JsObject] + ("_source" -> JsString(gitUrl))

                val jsonWithCorrectVersion = (json \ "version").asOpt[String].fold {
                  // the version was not in the json so add the specified version
                  json + ("version" -> JsString(version.vless))
                } { _ =>
                  // todo: resolve conflicts?
                  // for now just use the version from the json
                  json
                }

                jsonWithCorrectVersion.as[PackageInfo].copy(version = version.vless) // ignore the version in the bower.json
              }
            }
          }

          val tagCommitOrBranch = if (versions.contains(version.vless)) {
            version.vless
          }
          else if (versions.contains(version.vwith)) {
            version.vwith
          }
          else {
            // assume this is a commit or branch
            version
          }

          GitHub.gitHubUrl(gitUrl).fold({ _ =>
            git.file(gitUrl, Some(tagCommitOrBranch), "bower.json").fallbackTo(git.file(gitUrl, Some(tagCommitOrBranch), "package.json")).flatMap(parseBowerJson)
          }, { gitHubUrl =>
            gitHub.currentUrls(gitHubUrl).flatMap { case (homepage, sourceConnectionUri, issuesUrl) =>
              gitHub.raw(homepage, tagCommitOrBranch, "bower.json").fallbackTo(gitHub.raw(homepage, tagCommitOrBranch, "package.json")).flatMap(parseBowerJson).map { packageInfo =>
                packageInfo.copy(
                  maybeHomepageUrl = Some(homepage),
                  sourceConnectionUri = sourceConnectionUri,
                  maybeIssuesUrl = Some(issuesUrl)
                )
              }
            }
          })
        }
      }
    }
    else {
      ws.url(s"$BASE_URL/info/$packageNameOrGitRepo").get().flatMap { versionlessResponse =>
        versionlessResponse.status match {
          case Status.OK =>
            // todo: we do this because this is not correct: https://bower-as-a-service.herokuapp.com/info/jQuery/3.2.1
            // but it should probably be fixed in the bower-as-a-service
            // since the name returned on `bower info jQuery#3.2.1` is "jQuery"
            val name = (versionlessResponse.json \ "name").as[String]

            ws.url(s"$BASE_URL/info/$packageNameOrGitRepo/$version").get().flatMap { versionResponse =>
              versionResponse.status match {
                case Status.OK =>
                  Future.successful(versionResponse.json.as[PackageInfo].copy(name = name, version = version.vless))
                case _ =>
                  Future.failed(new Exception(versionResponse.body))
              }
            }
          case _ =>
            Future.failed(new Exception(versionlessResponse.body))
        }
      }
    } recoverWith {
      case _ =>
        npm.info(packageNameOrGitRepo, Some(version))
    }
  }

  override def info(packageNameOrGitRepo: String, maybeVersion: Option[String] = None, maybeSourceUri: Option[URI] = None): Future[PackageInfo] = {

    // if no version was specified use the latest
    val versionFuture: Future[String] = maybeVersion.fold {
      versions(packageNameOrGitRepo).flatMap { versions =>
        versions.headOption.fold(Future.failed[String](new Exception("The latest version could not be determined.")))(Future.successful)
      }
    }(Future.successful)

    versionFuture.flatMap { version =>
      rawInfo(packageNameOrGitRepo, version).flatMap { initialInfo =>
        initialInfo.maybeGitHubUrl.fold(Future.successful(initialInfo)) { gitHubUrl =>
          gitHub.currentUrls(gitHubUrl).map {
            case (homepage, sourceConnectionUri, issuesUrl) =>
              initialInfo.copy(
                maybeHomepageUrl = Some(homepage),
                sourceConnectionUri = sourceConnectionUri,
                maybeIssuesUrl = Some(issuesUrl)
              )
          }
        }
      }
    }
  }

  override def archive(packageNameOrGitRepo: String, version: String): Future[InputStream] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.gitUrl(packageNameOrGitRepo).flatMap { gitUrl =>
        git.tar(gitUrl, Some(version), Set("bower_modules"))
      }
    }
    else {
      Future.fromTry {
        Try {
          val url = new URL(s"$BASE_URL/download/$packageNameOrGitRepo/$version")
          url.openConnection().getInputStream
        }
      }
    }
  }

  override def mavenDependencies(dependencies: Map[String, String]): Future[Set[(String, String, String)]] = {
    maven.convertNpmBowerDependenciesToMaven(dependencies).map { mavenDependencies =>
      mavenDependencies.map {
        case (artifactId, version) =>
          ("org.webjars.bower", artifactId, version)
      }.toSet
    }
  }

  def lookup(packageNameOrGitRepo: String, version: String): Future[URL] = {
    val urlTry = Try {
      val maybeUrl = if (packageNameOrGitRepo.contains("/") && !packageNameOrGitRepo.contains(":")) {
        s"https://github.com/$packageNameOrGitRepo"
      }
      else {
        packageNameOrGitRepo
      }
      new URL(maybeUrl)
    }

    Future.fromTry(urlTry.flatMap(GitHub.gitHubUrl)).recoverWith {
      case _ =>
        ws.url(s"$BASE_URL/lookup/$packageNameOrGitRepo").get().flatMap { response =>
          response.status match {
            case Status.OK =>
              Future.fromTry(Try((response.json \ "url").as[URL]).flatMap(GitHub.gitHubUrl))
            case _ =>
              Future.failed(new Exception(s"Could not find package: $packageNameOrGitRepo"))
          }
        }
    } recoverWith {
      case _ =>
        npm.info(packageNameOrGitRepo, Some(version)).map(_.sourceConnectionUri.toURL)
    }
  }

}

object Bower {
  val sourceReads: Reads[URI] = (__ \ "_source").read[URI]

  val sourceToGitHubReads: Reads[URL] = {
    val error = JsonValidationError("Could not convert source to GitHub URL")
    sourceReads.collect(error) {
      // todo: nasty
      case uri: URI if GitHub.gitHubUrl(uri).isSuccess => GitHub.gitHubUrl(uri).get
    }
  }

  val sourceToGitHubIssuesReads: Reads[URL] = {
    val error = JsonValidationError("Could not convert source to GitHub Issues URL")
    sourceReads.collect(error) {
      // todo: nasty
      case uri: URI if GitHub.gitHubIssuesUrl(uri).isSuccess => GitHub.gitHubIssuesUrl(uri).get
    }
  }

  implicit def jsonReads: Reads[PackageInfo] = (
    (__ \ "name").read[String] ~
    (__ \ "version").read[String] ~
    (__ \ "homepage").read[URL].orElse(sourceToGitHubReads).map(Some(_)) ~
    sourceReads ~
    sourceToGitHubIssuesReads.map(Some(_)) ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse((__ \ "licenses").read[Seq[String]]).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
    Reads.pure(Map.empty[String, String])
  )(PackageInfo.apply _)

}

