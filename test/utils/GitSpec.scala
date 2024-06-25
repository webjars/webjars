package utils


import io.lemonlabs.uri.AbsoluteUrl
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.pekko.util.Timeout
import org.eclipse.jgit.api.{Git => GitApi}
import play.api.test._

import java.io.BufferedInputStream
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

class GitSpec extends PlaySpecification with GlobalApplication {

  override implicit def defaultAwaitTimeout: Timeout = 120.seconds

  lazy val git: Git = application.injector.instanceOf[Git]

  "git versions" should {
    "not work with an invalid git url" in {
      await(git.versions("foo/bar")) must throwA[Exception]
    }
    "work with git://host/path.git" in {
      val versions = await(git.versions("git://github.com/mochajs/mocha.git"))
      versions must not be empty
    }
    "work with git+http://host/path.git" in {
      val versions = await(git.versions("git+http://github.com/mochajs/mocha.git"))
      versions must not be empty
    }
    "work with git+https://host/path.git" in {
      val versions = await(git.versions("git+https://github.com/mochajs/mocha.git"))
      versions must not be empty
    }
    "work with http://host/path.git" in {
      val versions = await(git.versions("http://github.com/mochajs/mocha.git"))
      versions must not be empty
    }
    "work with https://host/path.git" in {
      val versions = await(git.versions("https://github.com/mochajs/mocha.git"))
      versions must not be empty
    }
    "work with githuborg/repo" in {
      val versions = await(git.versions("mochajs/mocha"))
      versions must not be empty
    }
    "work with a redirect" in {
      val versions = await(git.versions("visionmedia/mocha"))
      versions must not be empty
    }
  }

  "git file" should {
    "fetch a file with a version" in {
      val file = await(git.file("mochajs/mocha", "2.2.5", "package.json"))
      file.length must beEqualTo (1683)
      file.contains(""""version": "2.2.5"""") must beTrue
    }
    "fetch a file with a different version" in {
      val file = await(git.file("mochajs/mocha", "2.2.4", "package.json"))
      file.length must beEqualTo (1663)
      file.contains(""""version": "2.2.4"""") must beTrue
    }
    "fetch a file with a git url syntax" in {
      val file = await(git.file(AbsoluteUrl.parse("https://github.com/yiminghe/async-validator.git"), "v3.4.0", "LICENSE.md"))
      file.length must beEqualTo (1083)
      file must contain ("The MIT License (MIT)")
    }
    "fetch a file with a git url and a commit" in {
      val file = await(git.file(AbsoluteUrl.parse("git://github.com/mdedetrich/requirejs-plugins"), "d9c103e7a0", "LICENSE.txt"))
      file.length must beEqualTo(1082)
      file must contain("The MIT License (MIT)")
    }
  }

  "git tar" should {
    "fetch a tar" in {
      val tar = await(git.tar("mochajs/mocha", "2.2.5", Set("node_modules")))

      val bufferedInputStream = new BufferedInputStream(tar)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream[TarArchiveInputStream](bufferedInputStream)

      bufferedInputStream.available() must beEqualTo (640512)

      val files = LazyList.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName)
      files.size must beEqualTo (178)
      files.exists(_.contains("node_modules")) must beFalse
      files.exists(_.contains(".git")) must beFalse
    }
  }

  "artifactId" should {
    "convert a name to a name" in {
      await(git.artifactId("foo")) must beEqualTo ("foo")
    }
    "convert a github url to a name" in {
      await(git.artifactId("mochajs/mocha")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a git:// url to a name" in {
      await(git.artifactId("git://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a https:// url to a name" in {
      await(git.artifactId("https://github.com/mochajs/mocha.git")) must beEqualTo ("github-com-mochajs-mocha")
    }
    "convert a scoped name" in {
      await(git.artifactId("@reactivex/rxjs")) must beEqualTo ("reactivex__rxjs")
    }
    "not go case insensitive for github repos" in {
      await(git.artifactId("MochaJS/Mocha")) must beEqualTo ("github-com-MochaJS-Mocha")
    }
    "not go case insensitive for non-github repos" in {
      await(git.artifactId("Foo")) must beEqualTo ("Foo")
    }
  }

  "versionsOnBranch" should {
    "get the commits on a branch" in {
      await(git.versionsOnBranch("git://github.com/mochajs/mocha.git", "main")) must contain("8a100df959")
    }
    "fetch the versions" in {
      await(git.versionsOnBranch("git://github.com/mdedetrich/requirejs-plugins", "jsonSecurityVulnerability")) must contain ("d9c103e7a0")
    }
  }

  "cloneOrCheckout" should {
    "update the cache when run again" in {
      val dir = await(git.cloneOrCheckout("https://github.com/vaadin/vaadin-board.git", "v2.2.0"))

      val deletedTags = GitApi.open(dir).tagDelete().setTags("refs/tags/v2.0.0-beta2").call()

      deletedTags.asScala must contain ("refs/tags/v2.0.0-beta2")

      await(git.cloneOrCheckout("https://github.com/vaadin/vaadin-board.git", "v2.2.0"))

      val tags = GitApi.open(dir).tagList().call()

      tags.asScala.map(_.getName) must contain ("refs/tags/v2.0.0-beta2")
    }
    "be able to checkout a version, then a version" in {
      await(git.cloneOrCheckout("mochajs/mocha", "2.2.0", false)).exists() must beTrue
      await(git.cloneOrCheckout("mochajs/mocha", "2.2.4", false)).exists() must beTrue
    }
  }

}
