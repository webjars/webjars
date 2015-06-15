import models.WebJarVersion
import models.WebJarVersion.WebJarVersionOrdering
import org.specs2.mutable._
import utils.VersionOrdering

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
        WebJarVersion("1.2.0rc1"),
        WebJarVersion("2.3.0-12"),
        WebJarVersion("10.0.6"),
        WebJarVersion("04.10.2013"),
        WebJarVersion("0bafe48a77"),
        WebJarVersion("3768da3142"),
        WebJarVersion("b1d6033")
      )

      correct.reverse.sorted must beEqualTo(correct)
    }

  }
}
