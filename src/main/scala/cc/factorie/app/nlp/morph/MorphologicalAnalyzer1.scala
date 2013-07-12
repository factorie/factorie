package cc.factorie.app.nlp.morph

import collection.mutable
import cc.factorie.app.nlp.wordnet.WordNet
import cc.factorie.app.nlp.lexicon.Lexicon
import cc.factorie.util.ClasspathURL

/** A simple morphological analyzer, simply indicating if a noun is singular or plural.
    Obviously this supports very limited functionality. More will be added as needed.
    @author David Belanger */
class MorphologicalAnalyzer1(fmap: String => io.Source) {
  def this(name: String) = this(s => scala.io.Source.fromFile(name + s))
  val pluralWords = mutable.HashSet[String]()
  val singularWords = mutable.HashSet[String]()
  fmap("morph/en/noun.exc").getLines().foreach(x => {
    val words = x.split(" ")
    pluralWords += words(0)
    singularWords += words(1)
  })
  singularWords ++= fmap("morph/en/noun.txt").getLines().toSeq

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

object MorphologicalAnalyzer1 extends MorphologicalAnalyzer1((s:String) => {
  io.Source.fromURL(ClasspathURL.fromDirectory[Lexicon](s))
})
