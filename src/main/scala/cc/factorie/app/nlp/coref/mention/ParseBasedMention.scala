package cc.factorie.app.nlp.coref.mention

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.pos.PennPosTag
import scala.Some


class ParseBasedMentionList(spans:Iterable[Mention]) extends MentionList(spans)
//class NerSpanList extends TokenSpanList[NerSpan]

object ParseBasedMentionFinding extends ParseBasedMentionFinding(false)
object ParseAndNerBasedMentionFinding extends ParseBasedMentionFinding(true)

class ParseBasedMentionFinding(val useNER: Boolean) extends DocumentAnnotator {

  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")
  private final val COMMON_NOUNS      = Seq("NN" , "NNS")
  private final val PROPER_NOUNS      = Seq("NNP", "NNPS")
  private final val ALL_NOUNS         = Seq("NN","NNS","NNP","NNPS","PRP","PRP$")

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isCommonNoun     (t: Token) = COMMON_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isProperNoun     (t: Token) = PROPER_NOUNS.contains(t.posTag.categoryValue.toUpperCase)
  private def isNoun           (t: Token) = ALL_NOUNS.contains(t.posTag.categoryValue.toUpperCase)

  def predictMentionType(t: Token): Option[String] =
    if(isPersonalPronoun(t)) Some("PRO")
    else if(isCommonNoun(t)) Some("NOM")
    else if(isProperNoun(t)) Some("NAM")
    else None

  var FILTER_APPOS = true /* This flag is so that appositive filtering can be turned off.
                            If the mentions that we are extracting do not include the appositives as part of a mention
                            we want to make sure that we are extracting the appositives separately
                            default behavior is that we do filter the appositives.   */


  private def nerSpans(doc: Document): Seq[Mention] = {
    (for (span <- doc.attr[ConllNerSpanBuffer]) yield
      new Mention(span.section, span.start, span.length, span.length - 1) //this sets the head token idx to be the last token in the span
      ).toSeq
  }

  private def NNPSpans(doc : Document) : Seq[Mention] = {
    val spans = ArrayBuffer[ArrayBuffer[Token]]()
    spans += ArrayBuffer[Token]()
    for(section <- doc.sections; sentence <- section.sentences; token <- sentence.tokens) {
      if(spans.last.nonEmpty && spans.last.last.next != token) spans += ArrayBuffer[Token]()
      if(isProperNoun(token)) spans.last += token
    }
    if(spans.nonEmpty && spans.last.isEmpty) spans.remove(spans.length-1)
    (for(span <- spans) yield
      new Mention(span.head.section, span.head.positionInSection, span.last.positionInSection-span.head.positionInSection+1, span.last.positionInSection-span.head.positionInSection)).toSeq
  }

  // [Assumes personal pronouns are single tokens.]
  private def personalPronounSpans(doc: Document): Seq[Mention] = {
    (for (section <- doc.sections; s <- section.sentences;
           (t,i) <- s.tokens.zipWithIndex if isPersonalPronoun(t)) yield
        new Mention(section, s.start + i, 1,0)
      ).toSeq
  }

  private def getHeadTokenIdx(m: Mention): Int = {
   val tokenIdxInSection =  getHead(
      m.head.sentence.parse,
      m.start until (m.start + m.length) //these are section-level offsets
    )
    val tokenIdxInSpan = tokenIdxInSection - m.start
    assert(tokenIdxInSpan >= 0 && tokenIdxInSpan <= m.length)
    tokenIdxInSpan
  }
  //this expects as input indices in the **document section** not the sentence
  //note that this never returns the root as the head, it always returns a pointer to an actual token in the sentence
  //it will either return the root of a parse tree span, or a token that is a child of the root
  def getHead(parse: ParseTree, subtree: Seq[Int]): Int = {
    val sentenceLevelIndices = subtree.map(i => i - parse.sentence.start)
    var curr = sentenceLevelIndices.head
    val leftBoundary = sentenceLevelIndices.head
    val rightBoundary = sentenceLevelIndices.last
    while(parse.parentIndex(curr) > 0 && containedInInterval(leftBoundary,rightBoundary,parse.parentIndex(curr))){
      curr = parse.parentIndex(curr)
    }
    curr + parse.sentence.start  //this shifts it back to have section-level indices
  }

  private def containedInInterval(left: Int, right: Int, testIndex: Int): Boolean = {
    testIndex >= left && testIndex <= right
  }

  final val copularVerbs = collection.immutable.HashSet[String]() ++ Seq("is","are","was","'m")

  final val allowedChildLabels = Set("amod", "det", "nn", "num", "hmod", "hyph", "possessive", "poss", "predet", "nmod", "dep")
  final val disallowedChildLabels = Set("conj", "punct", "prep", "cc", "appos", "npadvmod", "advmod", "quantmod", "partmod", "rcmod", "dobj", "nsubj", "infmod", "ccomp", "advcl", "aux", "intj", "neg", "preconj", "prt", "meta", "parataxis", "complm", "mark")

  private def nounPhraseSpans(doc: Document, nounFilter: Token => Boolean): Seq[Mention] =  {
    val mentions = ArrayBuffer[Mention]()
    for (section <- doc.sections; s <- section.sentences; (t, si) <- s.tokens.zipWithIndex if nounFilter(t);
         label = s.parse.label(t.positionInSentence).categoryValue
         if label != "nn" && label != "hmod")  {
      val children = s.parse.children(t.positionInSentence)
      children.foreach(c => {
        val cat = s.parse.label(c.positionInSentence).categoryValue
        if (!(allowedChildLabels.contains(cat) || disallowedChildLabels.contains(cat))) {
          println("BAD LABEL: " + cat)
          // println(doc.owplString(DepParser1))
        }
      })
      val goodChildren = children.filter(c => allowedChildLabels.contains(s.parse.label(c.positionInSentence).categoryValue))
      val tokens = Seq(t) ++ goodChildren.map(c => s.parse.subtree(c.positionInSentence)).flatten
      val sTokens = tokens.sortBy(_.positionInSection)
      val start = sTokens.head.positionInSection
      val end = sTokens.last.positionInSection
      mentions += new Mention(section, start, end-start+1, sTokens.zipWithIndex.filter(i => i._1 eq t).head._2)
    }
    mentions
  }


  private def dedup(mentions: Seq[Mention]): Seq[Mention] = {
      def dedupOverlappingMentions(mentions: Seq[Mention]): Mention = {
        if(mentions.length == 1){
          return mentions.head
        }else{
          mentions.find(_.attr[MentionType].categoryValue == "NAM").getOrElse(mentions.head)
        }
      }


      mentions
      .groupBy(m => (m.section,m.start,m.length))
      .values.map(mentionSet => dedupOverlappingMentions(mentionSet)).toSeq
      .sortBy(m => (m.tokens.head.stringStart, m.length))

  }


  def process(doc: Document): Document = {

    var docMentions = new ArrayBuffer[Mention]

    //if NER has already been done, then convert the NER tags to NER spans
    //Note that this doesn't change the postAttrs for the annotator, since it may not necessarily add spans
    if(useNER) docMentions ++=  NerAndPronounMentionFinder.getNerMentions(doc)

    // NAM = proper noun, NOM = common noun, PRO = pronoun
    docMentions ++= personalPronounSpans(doc)           map(  m => {m.attr += new MentionType(m,"PRO");m})
    docMentions ++= nounPhraseSpans(doc, isCommonNoun)  map(  m => {m.attr += new MentionType(m,"NOM");m})
    docMentions ++= nounPhraseSpans(doc, isProperNoun)  map(  m => {m.attr += new MentionType(m,"NAM");m})
    docMentions ++= NNPSpans(doc)                       map(  m => {m.attr += new MentionType(m,"NAM");m})
    // Filter Mentions that have no MentionType and that are longer than 5 words -akm
    //doc.attr += (new MentionList() ++= removeSmallerIfHeadWordEqual(doc, dedup(docMentions)).filter(mention => (mention.attr[MentionType] ne null) && mention.span.length < 6).toSeq)
    doc.attr += (new MentionList(dedup(docMentions).filter(mention => mention.attr[MentionType] ne null).toSeq))
    doc
  }

  def prereqAttrs: Iterable[Class[_]] = if (!useNER) Seq(classOf[parse.ParseTree]) else Seq(classOf[parse.ParseTree], classOf[ner.IobConllNerTag])
  def postAttrs: Iterable[Class[_]] = Seq(classOf[MentionList])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+m.indexOf(token)).mkString(","); case _ => "_" }


}


