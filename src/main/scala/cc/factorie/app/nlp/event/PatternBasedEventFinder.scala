package cc.factorie.app.nlp.event

import cc.factorie._
import cc.factorie.app.nlp.{Sentence, Token, Document, DocumentAnnotator}
import cc.factorie.app.nlp.ner._
import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern
import scala.reflect.ClassTag
import scala.collection.mutable

/**
 * @author John Sullivan
 */
class PatternBasedEventFinder[NERSpan <: NerSpan, NERBuffer <: NerSpanBuffer[NERSpan]](typeEventMap:Map[String, Iterable[EventRole]], eventPatterns:Map[EventRole, Seq[EventPattern]])(implicit buff:ClassTag[NERBuffer]) extends DocumentAnnotator {
  def tokenAnnotationString(token: Token) = null //todo we can actually set this up if we need it

  val postAttrs = Seq(classOf[MatchedEventPatterns])

  val prereqAttrs = Seq(classOf[Token], classOf[Sentence], buff.getClass)

  def process(document: Document) = {
    val matchedPatterns = new MatchedEventPatterns
    document.attr[NERBuffer].map{span =>
      val sentenceSlug = " " + span.sentence.string.replace(span.string, "\\$ARG") + " "
      matchedPatterns ++= typeEventMap(span.label.categoryValue).flatMap{ role =>
        eventPatterns.get(role).flatMap(_.collectFirst{case ep if ep.matches(sentenceSlug) => ep})
      }.map(MatchedEventPattern(span, _))
      //todo have a option to only return most confident pattern
    }
    document.attr += matchedPatterns

    document
  }
}

object BBNEventPatternBasedEventFinder extends PatternBasedEventFinder[BBNEventNerSpan, BBNEventNerSpanBuffer](
  typeEventMap = new BufferedReader(new InputStreamReader(getClass.getResourceAsStream("/event/bbn_tags")))
    .toIterator.flatMap{ line =>
      val Array(event, role, typeString) = line.split("\\s+")
      val types = typeString.split(",")
      types.map(_ -> EventRole(event, role, types.toSet))
    }.toIterable.groupBy(_._1).mapValues(_.map(_._2)),
  eventPatterns = new BufferedReader(new InputStreamReader(getClass.getResourceAsStream("/event/event_patterns_10k")))
    .toIterator.map{line =>
    val Array(confString, event, role, pattern, _) = line.split("\t")
    val confidence = confString.toDouble
    val er = EventRole(event, role)
    er -> RegexEventPattern(confidence, er, pattern)
  }.toSeq.groupBy(_._1).mapValues(_.map(_._2)))

case class EventRole(event:String, role:String, typeRestrictions:Set[String]=Set.empty[String])
trait EventPattern {
  def confidence:Double
  def eventRole:EventRole

  def matches(sentenceSlug:String):Boolean
}
case class RegexEventPattern(confidence:Double, eventRole:EventRole, rawPattern:String) extends EventPattern {
  // This is used to replace a star by a regex that matches 1-4 words.
  private val STAR_REGEX = "[^ ]+( [^ ]+){0,3}";
  // a question mark corresponds to 1 word.
  private val QUESTION_MARK_REGEX = "[^ ]+";
  // This matches escaped and unescaped stars or question marks.
  private val STAR_OR_QM_MATCH_PATTERN = "(^| )(\\\\?\\*|\\?)( |$)".r
  private val patternRegex = {
    val pattern = ( " " + rawPattern + " ").replaceAll("ARG", "\\$ARG")
    var start = 0
    val sb = new StringBuffer();
    // Deal with stars in patterns.
    STAR_OR_QM_MATCH_PATTERN.findAllMatchIn(pattern).foreach(m => {
      val starOrQuestionMark = m.group(2)
      if ("*".equals(starOrQuestionMark)) {
        // Unescaped star: expand to token sequence regex.
        val quoted = Pattern.quote(pattern.substring(start, m.start(2)));
        sb.append(quoted)
        sb.append(STAR_REGEX)
      } else if ("?".equals(starOrQuestionMark)) {
        // Unescaped question mark.
        val quoted = Pattern.quote(pattern.substring(start, m.start(2)));
        sb.append(quoted)
        sb.append(QUESTION_MARK_REGEX)
      } else if ("\\*".equals(starOrQuestionMark)) {
        // Escaped star: include as star-token.
        val quoted = Pattern.quote(pattern.substring(start, m.start(2)) + "*")
        sb.append(quoted);
      } else if ("\\?".equals(starOrQuestionMark)) {
        // Escapes question mark.
        val quoted = Pattern.quote(pattern.substring(start, m.start(2)) + "?")
        sb.append(quoted);
      } else {
        throw new IllegalStateException("Impossible regex match.");
      }
      start = m.end(2);
    })

    sb.append(Pattern.quote(pattern.substring(start)));

    val regexStr = sb.toString //.replace("$ARG", "\\E" + "(the )?" + "\\Q$ARG");
    regexStr.r

  }

  def matches(sentenceSlug:String) = patternRegex.findFirstMatchIn(sentenceSlug).isDefined
}

case class MatchedEventPattern(span:NerSpan, pattern:EventPattern)
class MatchedEventPatterns extends mutable.ArrayBuffer[MatchedEventPattern]

