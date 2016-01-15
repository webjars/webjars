import models.WebJarVersion
import models.WebJarVersion.WebJarVersionOrdering
import org.specs2.mutable._
import utils.VersionOrdering

import scala.util.Random

class WebJarVersionOrderingSpec extends Specification {

  "WebJarVersionOrdering" should {

    "deal with malformed version" in {
      VersionOrdering.unmalform("0rc1") must beEqualTo("0.rc.1")
      VersionOrdering.unmalform("0beta1") must beEqualTo("0.beta.1")
    }

    "sort in the correct order" in {

      val correct = Seq(
        WebJarVersion("1.1.1"),
        WebJarVersion("1.1.2"),
        WebJarVersion("1.2.0"),
        WebJarVersion("1.10.0"),
        WebJarVersion("2.0.0"),
        WebJarVersion("10.0.0"),
        WebJarVersion("10.0.9"),
        WebJarVersion("10.0.10")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with dashes" in {

      val correct = Seq(
        WebJarVersion("1.1"),
        WebJarVersion("1.1-1"),
        WebJarVersion("1.1-2"),
        WebJarVersion("1.2")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with different version schemes" in {

      val correct = Seq(
        WebJarVersion("1.0.6"),
        WebJarVersion("1.0.8"),
        WebJarVersion("1.1.1"),
        WebJarVersion("1.1.4"),
        WebJarVersion("1.2.0beta1"),
        WebJarVersion("1.2.0-beta2"),
        WebJarVersion("1.2.0rc1"),
        WebJarVersion("1.2.0-rc.2"),
        WebJarVersion("1.2.0-rc.3"),
        WebJarVersion("1.2.0"),
        WebJarVersion("1.2.1"),
        WebJarVersion("1.2.10")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with dashes" in {

      val correct = Seq(
        WebJarVersion("1.1-0"),
        WebJarVersion("1.1-1"),
        WebJarVersion("1.1-10")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with beta modifiers" in {

      val correct = Seq(
        WebJarVersion("1.2.20"),
        WebJarVersion("1.3.0-beta.2"),
        WebJarVersion("1.3.0-beta.7"),
        WebJarVersion("1.3.0-beta.7-1"),
        WebJarVersion("1.3.0-beta.15"),
        WebJarVersion("1.3.0")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with dates" in {
      val correct = Seq(
        WebJarVersion("04.09.2013"),
        WebJarVersion("04.10.2013"),
        WebJarVersion("07.31.2013"),
        WebJarVersion("01.08.2014")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

    "deal with SHA versions" in {
      val correct = Seq(
        WebJarVersion("0bafe48a77"),
        WebJarVersion("3768da3142"),
        WebJarVersion("b1d6033"),
        WebJarVersion("1.2.0rc1"),
        WebJarVersion("2.3.0-12"),
        WebJarVersion("10.0.6"),
        WebJarVersion("04.10.2013")
      )

      correct.reverse.sorted must beEqualTo(correct)

      // Specific case from "smart-table"
      val correct2 = Seq(
        WebJarVersion("b1d6033"),
        WebJarVersion("1.2.6"),
        WebJarVersion("1.3.0"),
        WebJarVersion("1.4.0"),
        WebJarVersion("1.4.2"),
        WebJarVersion("1.4.3"),
        WebJarVersion("1.4.4"),
        WebJarVersion("1.4.5"),
        WebJarVersion("1.4.6"),
        WebJarVersion("1.4.7"),
        WebJarVersion("1.4.8"),
        WebJarVersion("1.4.9"),
        WebJarVersion("1.4.10"),
        WebJarVersion("1.4.11"),
        WebJarVersion("2.0.1"),
        WebJarVersion("2.0.3"),
        WebJarVersion("2.0.3-1")
      )

      correct2.reverse.sorted must beEqualTo(correct2)

    }

    "deal with + signs" in {
      val correct = Seq(
        WebJarVersion("3.2.0"),
        WebJarVersion("3.2.0+4"),
        WebJarVersion("3.3.1"),
        WebJarVersion("3.3.1+1"),
        WebJarVersion("3.3.1+2"),
        WebJarVersion("3.3.2"),
        WebJarVersion("3.3.4+1"),
        WebJarVersion("3.3.5")
      )

      Random.shuffle(correct).sorted must beEqualTo (correct)
    }

    "deal with dates in the tag" in {
      val correct = Seq(
        WebJarVersion("1.7.0-dev.20150925"),
        WebJarVersion("1.7.5-dev.20150925"),
        WebJarVersion("1.7.5"),
        WebJarVersion("1.8.0-dev.20151214"),
        WebJarVersion("1.8.0-dev.20151215")
      )

      correct.reverse.sorted must beEqualTo (correct)
    }

    "deal with jquery stuff" in {
      val correct = Seq(
        WebJarVersion("2.2.0"),
        WebJarVersion("3.0.0-alpha1"),
        WebJarVersion("3.0.0-alpha2"),
        WebJarVersion("3.0.0-beta1"),
        WebJarVersion("3.0.0")
      )

      correct.reverse.sorted must beEqualTo (correct)
    }

  }
}
