package utils


import java.io.BufferedInputStream

import akka.util.Timeout
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import play.api.test._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class GitSpec extends PlaySpecification {

  override implicit def defaultAwaitTimeout: Timeout = 30.seconds

  val ws = StandaloneWS.apply()
  val git = Git(ExecutionContext.global, ws.client)

  "git versions" should {
    "not work with an invalid git url" in {
      await(git.versions("foo/bar")) must throwA[Exception]
    }
    "work with git://host/path.git" in {
      val versions = await(git.versions("git://github.com/mochajs/mocha.git"))
      versions.length must beGreaterThan (0)
    }
    "work with git+http://host/path.git" in {
      val versions = await(git.versions("git+http://github.com/mochajs/mocha.git"))
      versions.length must beGreaterThan (0)
    }
    "work with git+https://host/path.git" in {
      val versions = await(git.versions("git+https://github.com/mochajs/mocha.git"))
      versions.length must beGreaterThan (0)
    }
    "work with http://host/path.git" in {
      val versions = await(git.versions("http://github.com/mochajs/mocha.git"))
      versions.length must beGreaterThan (0)
    }
    "work with https://host/path.git" in {
      val versions = await(git.versions("https://github.com/mochajs/mocha.git"))
      versions.length must beGreaterThan (0)
    }
    "work with githuborg/repo" in {
      val versions = await(git.versions("mochajs/mocha"))
      versions.length must beGreaterThan (0)
    }
    "work with a redirect" in {
      val versions = await(git.versions("visionmedia/mocha"))
      versions.length must beGreaterThan (0)
    }
  }

  "git file" should {
    "fetch a file with a version" in {
      val file = await(git.file("mochajs/mocha", Some("2.2.5"), "package.json"))
      file.length must beEqualTo (1683)
      file.contains(""""version": "2.2.5"""")
    }
    "fetch a file" in {
      val file = await(git.file("mochajs/mocha", None, "package.json"))
      file.length must beGreaterThan (0)
    }
    "fetch a file with a different version" in {
      val file = await(git.file("mochajs/mocha", Some("2.2.4"), "package.json"))
      file.length must beEqualTo (1663)
      file.contains(""""version": "2.2.4"""")
    }
  }

  "git tar" should {
    "fetch a tar" in {
      val tar = await(git.tar("mochajs/mocha", Some("2.2.5"), Set("node_modules")))

      val bufferedInputStream = new BufferedInputStream(tar)
      val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(bufferedInputStream)

      bufferedInputStream.available() must beEqualTo (645120)

      val files = Stream.continually(archiveStream.getNextEntry).takeWhile(_ != null).map(_.getName).toSeq
      files.size must beEqualTo (178)
      files.exists(_.contains("node_modules")) must beFalse
      files.exists(_.contains(".git")) must beFalse
    }
  }


  step(ws.close())

}