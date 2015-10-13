package cc.factorie.app.nlp.relation

import java.io.InputStream

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.{ParseForwardCoref, WithinDocCoref}

import scala.io.Source

/**
 * @author John Sullivan, Benjamin Roth
 */
class PatternBasedRelationFinder(predictors:Seq[PatternRelationPredictor]) extends DocumentAnnotator{
  def tokenAnnotationString(token: Token) = null

  def postAttrs = Seq(classOf[RelationMentionSeq])

  def prereqAttrs = (Seq(classOf[WithinDocCoref]) ++ ParseForwardCoref.prereqAttrs).distinct

  def process(doc: Document) = {
    val coref = doc.coref

    val mentions = coref.mentions.sortBy(_.phrase.asInstanceOf[TokenSpan]).toList

    /** this produces a sliding window of 4 mentions that we then compare to generate contexts. Each mention should be compared
      * to the three mentions before and after it in the following loop. The last element is a singleton list which we drop.
      * The last mention in the document has already been compared to the three mentions that precede it.
      */
    val mentionGrouping = (0 until mentions.size).map(idx => mentions.slice(idx, math.min(idx + 4, mentions.size))).dropRight(1).toList

    val relationMentions = (for(m1 :: ms <- mentionGrouping;
                               m2 <- ms;
                               if ((m1.phrase.sentence eq m2.phrase.sentence) && (m1.phrase.sentence.length < 100)))
    yield {Seq(new RelationMention(m1, m2, true), new RelationMention(m2, m1, false))}).flatten

    for (rm <- relationMentions;
         predictor <- predictors;
         matchLevel = predictor.relationMatch(rm);
         if matchLevel > 0.0) {
      rm._relations.+=(TACRelation(predictor.relation, matchLevel, rm.arg1.phrase.sentence.string))
    }

    val relSet = new RelationMentionSeq()
    relSet.++=(relationMentions.filter(_._relations.nonEmpty))
    doc.attr += relSet
    doc
  }
}

object OntoNotesPatternBasedRelationFinder extends PatternBasedRelationFinder(PatternRelationPredictor.predictorsFromStreams(getClass.getResourceAsStream("/cc/factorie/app/nlp/relation/patterns.tuned"), getClass.getResourceAsStream("/cc/factorie/app/nlp/relation/argtypes_ontonotes")))
object ConllPatternBasedRelationFinder extends PatternBasedRelationFinder(PatternRelationPredictor.predictorsFromStreams(getClass.getResourceAsStream("/cc/factorie/app/nlp/relation/patterns.tuned"), getClass.getResourceAsStream("/cc/factorie/app/nlp/relation/argtypes_conll")))


case class PatternRelationPredictor(relation : String, patternConfidences : Map[String, Double], qTypes : Set[String],
                                           sTypes : Set[String]) {

  val ARG1 = "$ARG1"
  val ARG2 = "$ARG2"


  /** The first boolean indicates if the relation holds in the forward direction (arg1 first) the second if it holds in the reverse */
  def relationMatch(rm : RelationMention) : Double = {
    val arg1End = rm.arg1.phrase.last.positionInSentence
    val arg2Start = rm.arg2.phrase.head.positionInSentence


    val forwardPattern = ARG1 + " " + rm.arg1.phrase.sentence.slice(arg1End + 1, arg2Start).map(_.string).mkString(" ") + " " + ARG2
    val backwardPattern = ARG2 + " " + rm.arg1.phrase.sentence.slice(arg1End + 1, arg2Start).map(_.string).mkString(" ") + " " + ARG1

    val pattern = if(rm.isArg1First) forwardPattern else backwardPattern

    val arg1Type = rm.arg1.phrase.head.nerTag.baseCategoryValue
    val arg2Type = rm.arg2.phrase.head.nerTag.baseCategoryValue
    val hasMatch = qTypes.contains(arg1Type) && sTypes.contains(arg2Type) && patternConfidences.contains(pattern)
    if(hasMatch) patternConfidences(pattern) else 0.0
  }
}

object PatternRelationPredictor {
  def predictorsFromStreams(patternStream:InputStream, typeFileStream:InputStream):Seq[PatternRelationPredictor] = {

    val relToPats = Source.fromInputStream(patternStream, "UTF8").getLines.map(_.stripLineEnd.split(" ", 3)).
      map(fields => fields(1) -> (fields(2), fields(0).toDouble)).toList.groupBy(_._1).map { case (k,v) => (k,v.map(_._2).toMap)}

    // reads types from a white-space & comma-separted file of the form:
    // relation arg1type,arg1type... arg2type,arg2type
    // Types of ontonotes domain described here: http://catalog.ldc.upenn.edu/docs/LDC2008T04/OntoNotes-Release-2.0.pdf
    val relToTypes = Source.fromInputStream(typeFileStream, "UTF8").getLines.map(_.stripLineEnd.split(" ", 3)).
      map(fields => fields(0) -> (fields(1).split(',').toSet, fields(2).split(',').toSet)).toList
    for ((rel, (arg1types, arg2types)) <- relToTypes) yield
      new PatternRelationPredictor(rel, relToPats.getOrElse(rel, Map.empty[String, Double]), arg1types, arg2types)
  }
}
