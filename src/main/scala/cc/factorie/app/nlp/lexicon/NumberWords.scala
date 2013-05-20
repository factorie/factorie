package cc.factorie.app.nlp.lexicon
import scala.collection.immutable.{Set,HashSet}

class WordsLexicon(contents:String*) extends Set[String] {
  protected def inner: Set[String] = HashSet(contents:_*)
  def contains(key:String): Boolean = inner.contains(key)
  def iterator: Iterator[String] = inner.iterator
  def +(elem: String): Set[String] = inner + elem
  def -(elem: String): Set[String] = inner + elem
}  

object NumberWords extends WordsLexicon(
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
      "thousand",
      "million",
      "billion",
      "trillion",
      "quadrillion",
      "quintillion",
      "sextillion",
      "septillion",
      "zillion",
      "umpteen",
      "multimillion",
      "multibillion"
      )



