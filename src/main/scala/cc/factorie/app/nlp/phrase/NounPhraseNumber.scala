package cc.factorie.app.nlp.phrase

import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.morph.BasicMorphologicalAnalyzer
import org.jblas.Singular
import cc.factorie.variable.{EnumDomain, CategoricalVariable}
import scala.reflect.ClassTag
import cc.factorie.app.nlp.coref.mention.{MentionList, Mention}

object NumberDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  SINGULAR,        // known to be non-person
  PLURAL = Value   // person, but uncertain about gender
}

class NumberLabel[P <: Phrase](val phrase: P, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def this(m:P, initialIntValue:Int) = this(m, NumberDomain(initialIntValue).category.asInstanceOf[String])
  def domain = NumberDomain
}

/** Cheap number predictor based on rules and lexicons.  Really this should use a real morphological analyzer. */
class NumberLabeler[P <: Phrase, PL <: TokenSpanList[P]](implicit ctList:ClassTag[PL], ctPhrase:ClassTag[P])  extends DocumentAnnotator {
  val singularPronoun = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluralPronoun = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singularDeterminer = Set("a", "an", "this")
  val pluralDeterminer = Set("those", "these", "some")
  def isProper(pos:String): Boolean = pos.startsWith("NNP")
  def isNoun(pos:String): Boolean = pos(0) == 'N'
  def isPossessive(pos:String): Boolean = pos == "POS"
  def process(document:Document): Document = {
    import NumberDomain._
    for (phrase <- document.attr()(ctList)) {
      val number = new NumberLabel(phrase, UNKNOWN)
      phrase.attr += number
      if (phrase.length > 0) {
        val firstWord = phrase(0).string.toLowerCase
        val headPos = phrase.headToken.attr[PennPosTag].categoryValue
        if (singularPronoun.contains(firstWord) || singularDeterminer.contains(firstWord)) number := SINGULAR
        else if (pluralPronoun.contains(firstWord) || pluralDeterminer.contains(firstWord)) number := PLURAL
        else if (isProper(headPos) && phrase.exists(token => token.string.toLowerCase == "and")) number := PLURAL
        else if (isNoun(headPos) || isPossessive(headPos)) {
          val headWord = phrase.headToken.string.toLowerCase
          if (BasicMorphologicalAnalyzer.isPlural(headWord)) number := PLURAL
          else if (headPos.startsWith("N")) { if (headPos.endsWith("S")) number := PLURAL else number := SINGULAR }
          else number := SINGULAR
        }
      }
    }
    document
  }
  override def tokenAnnotationString(token:Token): String = { val phrases = token.document.attr()(ctList).filter(_.contains(token)); phrases.map(_.attr[NumberLabel[P]].categoryValue).mkString(",") }
  override def phraseAnnotationString(phrase:Phrase): String = { val t = phrase.attr[NumberLabel[P]]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[PennPosTag], classOf[NounPhrase])
  def postAttrs: Iterable[Class[_]] = List(classOf[NumberLabel[P]])
}

object NounPhraseNumberLabeler extends NumberLabeler[NounPhrase,NounPhraseList]
object MentionNumberLabeler extends NumberLabeler[Mention,MentionList]
