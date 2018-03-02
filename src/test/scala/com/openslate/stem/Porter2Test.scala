package com.openslate.stem

import scala.io.Source
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.matchers._

class Porter2Test extends FlatSpec {
  val prefile = "/voc.txt"
  val postfile = "/output.txt"

  "A Stemmer" should "stem words" in {
    val inIter = Source.fromInputStream(getClass.getResourceAsStream(prefile)).getLines
    val outIter = Source.fromInputStream(getClass.getResourceAsStream(postfile)).getLines
    val gen = inIter.zip(outIter)
    for ((pre, post) <- gen) {
      assertResult(post) { Porter2.stem(pre) }
    }
  }
}
