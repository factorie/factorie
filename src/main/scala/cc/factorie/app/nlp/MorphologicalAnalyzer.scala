package cc.factorie.app.nlp.morph

import collection.mutable

/**
Obviously this supports very limited functionality. More will be added as needed
 */
class MorphologicalAnalyzer(dictPath: String) {
  val pluralWordsList = scala.io.Source.fromFile(dictPath + "/noun.exc").getLines()
  val pluralWords = mutable.HashSet[String]()
  val singularWords = mutable.HashSet[String]()
  pluralWordsList.foreach(x => {
    val words = x.split(" ")
    pluralWords += words(0)
    singularWords += words(1)
  })

  singularWords ++= scala.io.Source.fromFile(dictPath + "/noun.txt").getLines().toSeq

  //note you can imagine that these following methods would simply be negations of eachother.
  //for example, you could have them be precision biased and you could classify things as being in the gray area
  //note that the default for totally unseen words that don't have a regular ending is that it's singular
  def isSingular(s: String) = !isPlural(s)
  def isPlural(s: String): Boolean = {
    if(singularWords.contains(s))
      return false
    else{
      if(matchesCommonPluralEndings(s) || pluralWords.contains(s))
        return true
      else return false
    }
  }

  val nounSufxs = List("s", "ses",  "ches", "shes", "men", "ies", "xes", "zes")

  def matchesCommonPluralEndings(s: String): Boolean = {
    nounSufxs.exists(suff =>  s.endsWith(suff))
  }
}
