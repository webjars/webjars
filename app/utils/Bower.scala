package utils

import play.api.http.Status
import play.api.i18n.{Langs, MessagesApi}
import play.api.libs.concurrent.Futures
import play.api.libs.concurrent.Futures._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws._
import utils.Deployable._
import utils.PackageInfo._

import java.io.InputStream
import java.net.{URI, URL}
import javax.inject.Inject
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class Bower @Inject() (val ws: WSClient, val licenseDetector: LicenseDetector, val messages: MessagesApi, val langs: Langs, git: Git, gitHub: GitHub, maven: Maven) (implicit ec: ExecutionContext) extends Deployable {

  import Bower._

  val BASE_URL = "https://bower-as-a-service.herokuapp.com"

  override val name: String = "Bower"

  override val groupIdQuery: String = "org.webjars.bower"

  override def includesGroupId(groupId: String): Boolean = groupId.equalsIgnoreCase(groupIdQuery)

  override def groupId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = Future.successful(groupIdQuery)

  override def artifactId(nameOrUrlish: NameOrUrlish, version: Version): Future[String] = git.artifactId(nameOrUrlish)

  override def excludes(nameOrUrlish: NameOrUrlish, version: Version): Future[Set[String]] = {
    // todo: apply bower ignore in case of git repo
    Future.successful(Set(".bower.json"))
  }

  override val metadataFile: String = "bower.json"

  override val contentsInSubdir: Boolean = false

  override def pathPrefix(nameOrUrlish: NameOrUrlish, releaseVersion: Version, packageInfo: PackageInfo): Future[String] = {
    artifactId(nameOrUrlish, releaseVersion).map { artifactId =>
      s"$artifactId/$releaseVersion/"
    }
  }

  def bowerToMaven(keyValue: (String, String)): Future[(String, String, String)] = {
    val (name, version) = parseDep(keyValue)

    for {
      info <- info(name, version)
      latestVersionInRange = info.version
      groupId <- groupId(name, latestVersionInRange)
      artifactId <- artifactId(name, latestVersionInRange)
      version <- Future.fromTry(SemVer.convertSemVerToMaven(version))
    } yield (groupId, artifactId, version)
  }

  override def versions(packageNameOrGitRepo: NameOrUrlish): Future[Set[Version]] = {
    ws.url(s"$BASE_URL/info").withQueryStringParameters("package" -> packageNameOrGitRepo).get().flatMap { response =>
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

  def rawJson(packageNameOrGitRepo: NameOrUrlish, version: Version): Future[JsValue] = {
    println((s"$BASE_URL/info", packageNameOrGitRepo, version))
    ws.url(s"$BASE_URL/info").withQueryStringParameters("package" -> packageNameOrGitRepo, "version" -> version).get().flatMap { versionResponse =>
      println(versionResponse.status)
      versionResponse.status match {
        case Status.OK =>
          Future.successful(versionResponse.json)
        case _ =>
          Future.failed(new Exception(versionResponse.body))
      }
    }
  }

  def rawInfo(packageNameOrGitRepo: NameOrUrlish, version: Version): Future[PackageInfo] = {
    rawJson(packageNameOrGitRepo, version).map(_.as[PackageInfo])
  }

  override def info(packageNameOrGitRepo: NameOrUrlish, version: Version, maybeSourceUri: Option[URI] = None): Future[PackageInfo] = {
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

  override def archive(packageNameOrGitRepo: NameOrUrlish, version: Version): Future[InputStream] = {
    Future.fromTry {
      Try {
        val url = new URL(s"$BASE_URL/download?package=$packageNameOrGitRepo&version=$version")
        url.openConnection().getInputStream
      }
    }
  }

  override def file(packageNameOrGitRepo: String, version: Version, filename: String): Future[String] = {
    if (git.isGit(packageNameOrGitRepo)) {
      git.file(packageNameOrGitRepo, version, filename)
    }
    else {
      archiveFile(packageNameOrGitRepo, version, filename)
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

  def lookup(packageNameOrGitRepo: NameOrUrlish): Future[URL] = {
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
    }
  }

  override def depGraph(packageInfo: PackageInfo, deps: Map[String, String] = Map.empty[String, String])(implicit ec: ExecutionContext, futures: Futures): Future[Map[String, String]] = {
    def depResolver(unresolvedDeps: Map[String, String], resolvedDeps: Map[String, String]): Future[(Map[String, String], Map[String, String])] = {
      val packagesToResolve = unresolvedDeps.view.filterKeys(!resolvedDeps.contains(_)).toMap
      packagesToResolve.headOption.fold {
        Future.successful(packagesToResolve -> resolvedDeps)
      } { dep =>
        val (nameOrUrlish, versionish) = parseDep(dep)
        info(nameOrUrlish, versionish).flatMap { newPackageInfo =>
          val newResolvedDeps = resolvedDeps + (nameOrUrlish -> newPackageInfo.version)
          val newUnresolvedDeps = packagesToResolve.tail ++ newPackageInfo.dependencies.map(parseDep)
          depResolver(newUnresolvedDeps, newResolvedDeps)
        } recoverWith {
          // just skip deps that can't be resolved
          case _ => depResolver(packagesToResolve.tail, resolvedDeps)
        }
      }
    }

    depResolver(packageInfo.dependencies.map(parseDep), Map.empty[String, String]).map(_._2).withTimeout(10.minutes)
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
    (__ \ "version").read[String].orElse((__ \ "_release").read[String]) ~
    (__ \ "homepage").read[URL].orElse(sourceToGitHubReads).map(Some(_)) ~
    sourceReads ~
    sourceToGitHubIssuesReads.map(Some(_)) ~
    (__ \ "license").read[Seq[String]].orElse((__ \ "license").read[String].map(Seq(_))).orElse((__ \ "licenses").read[Seq[String]]).orElse(Reads.pure(Seq.empty[String])) ~
    (__ \ "dependencies").read[Map[String, String]].orElse(Reads.pure(Map.empty[String, String])) ~
    Reads.pure(Map.empty[String, String]) ~
    (__ \ "_resolution" \ "tag").readNullable[String]
  )(PackageInfo.apply _)

}