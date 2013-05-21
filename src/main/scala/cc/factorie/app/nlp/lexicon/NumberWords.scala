package cc.factorie.app.nlp.lexicon
import scala.collection.immutable.{Set,HashSet}

/** A simple lexicon of individual words represented as a scala.collection.Set[String]. */
class WordLexicon(contents:String*) extends Set[String] {
  protected def inner: Set[String] = HashSet(contents:_*)
  def contains(key:String): Boolean = inner.contains(key)
  def iterator: Iterator[String] = inner.iterator
  def +(elem: String): Set[String] = inner + elem
  def -(elem: String): Set[String] = inner + elem
}  

object NumberWords extends WordLexicon(
      "zero",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "tens",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen",
      "twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety",
      "hundred",
      "hundreds",
      "thousand",
      "thousands",
      "million",
      "millions",
      "billion",
      "billions",
      "trillion",
      "trillions",
      "quadrillion",
      "quintillion",
      "sextillion",
      "septillion",
      "zillion",
      "umpteen",
      "multimillion",
      "multibillion"
      )



