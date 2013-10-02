package cc.factorie.app.nlp.mention
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos._
import cc.factorie.app.nlp.morph.MorphologicalAnalyzer1
import org.jblas.Singular
import cc.factorie.variable.{EnumDomain, CategoricalVariable}

object MentionNumberDomain extends EnumDomain {
  val UNKNOWN,     // uncertain 
  SINGULAR,        // known to be non-person
  PLURAL = Value   // person, but uncertain about gender
}
class MentionNumberLabel(val mention:Mention, initialCategory:String) extends CategoricalVariable(initialCategory) {
  def this(m:Mention, initialIntValue:Int) = this(m, MentionNumberDomain(initialIntValue).category.asInstanceOf[String])
  def domain = MentionNumberDomain
}

/** Cheap number predictor based on rules and lexicons.  Really this should use a real morphological analyzer. */
class MentionNumberLabeler extends DocumentAnnotator {
  val singularPronoun = Set("i", "me", "my", "mine", "myself", "he", "she", "it", "him", "her", "his", "hers", "its", "one", "ones", "oneself", "this", "that")
  val pluralPronoun = Set("we", "us", "our", "ours", "ourselves", "ourself", "they", "them", "their", "theirs", "themselves", "themself", "these", "those")
  val singularDeterminer = Set("a", "an", "this")
  val pluralDeterminer = Set("those", "these", "some")
  def isProper(pos:String): Boolean = pos.startsWith("NNP")
  def isNoun(pos:String): Boolean = pos(0) == 'N'
  def isPossessive(pos:String): Boolean = pos == "POS"
  def process(document:Document): Document = {
    import MentionNumberDomain._
    for (mention <- document.attr[MentionList]) {
      val number = new MentionNumberLabel(mention, UNKNOWN)
      mention.attr += number
      if (mention.length > 0) {
        val firstWord = mention(0).string.toLowerCase
        val headPos = mention.headToken.attr[PennPosLabel].categoryValue
        if (singularPronoun.contains(firstWord) || singularDeterminer.contains(firstWord)) number := SINGULAR
        else if (pluralPronoun.contains(firstWord) || pluralDeterminer.contains(firstWord)) number := PLURAL
        else if (isProper(headPos) && mention.exists(token => token.string.toLowerCase == "and")) number := PLURAL
        else if (isNoun(headPos) || isPossessive(headPos)) {
          val headWord = mention.headToken.string.toLowerCase
          if (MorphologicalAnalyzer1.isPlural(headWord)) number := PLURAL
          else if (headPos.startsWith("N")) { if (headPos.endsWith("S")) number := PLURAL else number := SINGULAR }
          else number := SINGULAR
        }
      }
    }
    document
  }
  override def tokenAnnotationString(token:Token): String = { val mentions = token.document.attr[MentionList].filter(_.contains(token)); mentions.map(_.attr[MentionNumberLabel].categoryValue).mkString(",") }
  override def mentionAnnotationString(mention:Mention): String = { val t = mention.attr[MentionNumberLabel]; if (t ne null) t.categoryValue else "_" }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[PennPosLabel], classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[MentionNumberLabel])
}

object MentionNumberLabeler extends MentionNumberLabeler

