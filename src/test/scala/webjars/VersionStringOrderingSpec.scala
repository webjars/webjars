package webjars

import webjars.models.WebJarVersion
import webjars.models.WebJarVersion.given
import webjars.utils.VersionStringOrdering
import zio.test.*

import scala.util.Random

object VersionStringOrderingSpec extends ZIOSpecDefault:

  def spec = suite("VersionStringOrdering")(
    test("deal with malformed version") {
      assertTrue(
        VersionStringOrdering.unmalform("0rc1") == "0.rc.1",
        VersionStringOrdering.unmalform("0beta1") == "0.beta.1",
      )
    },
    test("sort in the correct order") {
      val correct = Seq(
        WebJarVersion("10.0.10"),
        WebJarVersion("10.0.9"),
        WebJarVersion("10.0.0"),
        WebJarVersion("2.0.0"),
        WebJarVersion("1.10.0"),
        WebJarVersion("1.2.0"),
        WebJarVersion("1.1.2"),
        WebJarVersion("1.1.1"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with dashes") {
      val correct = Seq(
        WebJarVersion("1.2"),
        WebJarVersion("1.1-2"),
        WebJarVersion("1.1-1"),
        WebJarVersion("1.1"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with different version schemes") {
      val correct = Seq(
        WebJarVersion("1.2.10"),
        WebJarVersion("1.2.1"),
        WebJarVersion("1.2.0"),
        WebJarVersion("1.2.0-rc.3"),
        WebJarVersion("1.2.0-rc.2"),
        WebJarVersion("1.2.0rc1"),
        WebJarVersion("1.2.0-beta2"),
        WebJarVersion("1.2.0beta1"),
        WebJarVersion("1.1.4"),
        WebJarVersion("1.1.1"),
        WebJarVersion("1.0.8"),
        WebJarVersion("1.0.6"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with dash numbers") {
      val correct = Seq(
        WebJarVersion("1.1-10"),
        WebJarVersion("1.1-1"),
        WebJarVersion("1.1-0"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with beta modifiers") {
      val correct = Seq(
        WebJarVersion("1.3.0"),
        WebJarVersion("1.3.0-beta.15"),
        WebJarVersion("1.3.0-beta.7-1"),
        WebJarVersion("1.3.0-beta.7"),
        WebJarVersion("1.3.0-beta.2"),
        WebJarVersion("1.2.20"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with dates") {
      val correct = Seq(
        WebJarVersion("01.08.2014"),
        WebJarVersion("07.31.2013"),
        WebJarVersion("04.10.2013"),
        WebJarVersion("04.09.2013"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with SHA versions") {
      val correct = Seq(
        WebJarVersion("04.10.2013"),
        WebJarVersion("10.0.6"),
        WebJarVersion("2.3.0-12"),
        WebJarVersion("1.2.0rc1"),
        WebJarVersion("b1d6033"),
        WebJarVersion("3768da3142"),
        WebJarVersion("0bafe48a77"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with SHA versions - smart-table case") {
      val correct = Seq(
        WebJarVersion("2.0.3-1"),
        WebJarVersion("2.0.3"),
        WebJarVersion("2.0.1"),
        WebJarVersion("1.4.11"),
        WebJarVersion("1.4.10"),
        WebJarVersion("1.4.9"),
        WebJarVersion("1.4.8"),
        WebJarVersion("1.4.7"),
        WebJarVersion("1.4.6"),
        WebJarVersion("1.4.5"),
        WebJarVersion("1.4.4"),
        WebJarVersion("1.4.3"),
        WebJarVersion("1.4.2"),
        WebJarVersion("1.4.0"),
        WebJarVersion("1.3.0"),
        WebJarVersion("1.2.6"),
        WebJarVersion("b1d6033"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with + signs") {
      val correct = Seq(
        WebJarVersion("3.3.5"),
        WebJarVersion("3.3.4+1"),
        WebJarVersion("3.3.2"),
        WebJarVersion("3.3.1+2"),
        WebJarVersion("3.3.1+1"),
        WebJarVersion("3.3.1"),
        WebJarVersion("3.2.0+4"),
        WebJarVersion("3.2.0"),
      )
      assertTrue(Random.shuffle(correct).sorted == correct)
    },
    test("deal with dates in the tag") {
      val correct = Seq(
        WebJarVersion("1.8.0-dev.20151215"),
        WebJarVersion("1.8.0-dev.20151214"),
        WebJarVersion("1.7.5"),
        WebJarVersion("1.7.5-dev.20150925"),
        WebJarVersion("1.7.0-dev.20150925"),
      )
      assertTrue(correct.sorted == correct)
    },
    test("deal with jquery stuff") {
      val correct = Seq(
        WebJarVersion("3.0.0"),
        WebJarVersion("3.0.0-beta1"),
        WebJarVersion("3.0.0-alpha2"),
        WebJarVersion("3.0.0-alpha1"),
        WebJarVersion("2.2.0"),
      )
      assertTrue(correct.sorted == correct)
    },
  )
