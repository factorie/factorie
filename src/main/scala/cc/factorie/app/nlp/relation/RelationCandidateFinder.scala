package cc.factorie.app.nlp.relation

import cc.factorie.app.nlp.{Document, TokenSpan, RemoteTokenSpan}

import scala.collection.mutable

object EntityTypePatterns {
  val SLOTFILLING = "(ORG|PER|ORGANIZATION|PERSON)"
  val COLDSTART = "(ORG|PER|GPE|ORGANIZATION|PERSON|GPE:COUNTRY|GPE:STATE_PROVINCE|GPE:CITY)"
}

object SlotfillingRelationMentionGenerator {
  def getRelationMentions(doc:Document):Iterator[RelationPattern[String]] = new RelationCandidateFinder(EntityTypePatterns.SLOTFILLING).getCandidates(doc).flatMap(SurfaceStringPatternGenerator.getPattern)
}

case class RelationCandidate(arg1:TokenSpan, arg2:TokenSpan)

trait CandidateGenerator {
  def getCandidates(doc:Document):Iterator[RelationCandidate]
}

class RelationCandidateFinder(entityTypePatternString: String) extends CandidateGenerator {
  private val perOrgPattern = entityTypePatternString.r
  def getCandidates(document: Document): Iterator[RelationCandidate] = {
    val coref = document.getCoref
    val relationCandidates = mutable.ArrayBuffer[RelationCandidate]()

    val mentions = coref.mentions.sortBy(_.phrase.asInstanceOf[TokenSpan]).toList

    /** this produces a sliding window of 4 mentions that we then compare to generate contexts. Each mention should be compared
      * to the three mentions before and after it in the following loop. The last element is a singleton list which we drop.
      * The last mention in the document has already been compared to the three mentions that precede it.
      */
    val mentionGrouping = (0 until mentions.size).map(idx => mentions.slice(idx, math.min(idx + 4, mentions.size))).dropRight(1)

    for(m1 :: ms <- mentionGrouping;
        m2 <- ms;
        e1 = m1.phrase;
        e2 = m2.phrase;
        e1Type = e1.headToken.nerTag.baseCategoryValue;
        e2Type = e2.headToken.nerTag.baseCategoryValue
        if e1.sentence == e2.sentence) {

      // Entity1 is person or organization: add arg1 arg2 pattern
      if (perOrgPattern.findFirstIn(e1Type).isDefined) {
        relationCandidates += RelationCandidate(e1, e2)
      }
      // Entity2 is person or organization: add arg2 arg1 pattern
      if (perOrgPattern.findFirstIn(e2Type).isDefined) {
        relationCandidates += RelationCandidate(e2, e1)
      }
    }
    relationCandidates.toIterator
  }
}

case class RelationPattern[PatternType](arg1:TokenSpan, arg2:TokenSpan, arg1Type:String, arg2Type:String, pattern:PatternType) {
  /** a version that is not attached to documents */
  def remote = RemoteRelationPattern(arg1.remote, arg2.remote, arg1Type, arg2Type, pattern.toString)
}
case class RemoteRelationPattern(arg1:RemoteTokenSpan, arg2:RemoteTokenSpan, arg1Type:String, arg2Type:String, pattern:String)

trait PatternGenerator[PatternType] {
  def getPattern(candidate:RelationCandidate):Option[RelationPattern[PatternType]]
}

object SurfaceStringPatternGenerator extends PatternGenerator[String] {
  // note this method assumes that both arguments of the candidate reside in a single sentence
  def getPattern(candidate:RelationCandidate) = candidate match {
    case RelationCandidate(arg1, arg2) => Option(RelationPattern(arg1, arg2, Option(arg1.head.nerTag).map(_.baseCategoryValue).getOrElse(""), Option(arg2.head.nerTag).map(_.baseCategoryValue).getOrElse(""), arg1.sentence.drop(arg1.last.positionInSentence).take(arg2.head.positionInSentence).mkString(" ")))
  }
}