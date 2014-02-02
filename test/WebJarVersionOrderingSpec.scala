package test

import org.specs2.mutable._
import models.WebJarVersion
import scala.util.Random

class WebJarVersionOrderingSpec extends Specification {
  
  "WebJarVersionOrdering" should {
    
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

  }
}