package cc.factorie.app.nlp.morph

import collection.mutable

/** A simple morphological analyzer, simply indicating if a noun is singular or plural.
    Obviously this supports very limited functionality. More will be added as needed.
    @author David Belanger */
class MorphologicalAnalyzer1(dictPath: String) {
  val pluralWords = mutable.HashSet[String]()
  val singularWords = mutable.HashSet[String]()
  scala.io.Source.fromFile(dictPath + "/noun.exc").getLines().foreach(x => {
    val words = x.split(" ")
    pluralWords += words(0)
    singularWords += words(1)
  })
  singularWords ++= scala.io.Source.fromFile(dictPath + "/noun.txt").getLines().toSeq

  //note you can imagine that these following methods would simply be negations of each other.
  //for example, you could have them be precision biased and you could classify things as being in the gray area
  //note that the default for totally unseen words that don't have a regular ending is that it's singular
  def isSingular(s: String) = !isPlural(s)
  def isPlural(s: String): Boolean = (!singularWords.contains(s)) && (matchesCommonPluralEndings(s) || pluralWords.contains(s))

  val nounSufxs = List("s", "ses",  "ches", "shes", "men", "ies", "xes", "zes")
  def matchesCommonPluralEndings(s: String): Boolean = {
    nounSufxs.exists(suff =>  s.endsWith(suff))
  }
}
