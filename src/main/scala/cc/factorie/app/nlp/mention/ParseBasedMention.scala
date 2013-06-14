package cc.factorie.app.nlp.mention

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.ner.NerSpan
import cc.factorie.app.nlp.pos.PTBPosLabel
import scala.Some

object ParseBasedMentionFinding extends DocumentAnnotator {

  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")
  private final val COMMON_NOUNS      = Seq("NN" , "NNS")
  private final val PROPER_NOUNS      = Seq("NNP", "NNPS")
  private final val ALL_NOUNS         = Seq("NN","NNS","NNP","NNPS","PRP","PRP$")

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS.contains(t.posLabel.categoryValue.toUpperCase)
  private def isCommonNoun     (t: Token) = COMMON_NOUNS.contains(t.posLabel.categoryValue.toUpperCase)
  private def isProperNoun     (t: Token) = PROPER_NOUNS.contains(t.posLabel.categoryValue.toUpperCase)
  private def isNoun           (t: Token) = ALL_NOUNS.contains(t.posLabel.categoryValue.toUpperCase)

  private def nerSpans(doc: Document): Seq[Mention] = {
    (for (section <- doc.sections; span <- section.spansOfClass[NerSpan]) yield
      Mention(section, span.start, span.length, span.length - 1) //this sets the head token idx to be the last token in the span
      ).toSeq
  }

  // [Assumes personal pronouns are single tokens.]
  private def personalPronounSpans(doc: Document): Seq[Mention] = {
    (for (section <- doc.sections; s <- section.sentences;
           (t,i) <- s.tokens.zipWithIndex if isPersonalPronoun(t)) yield
        Mention(section, s.start + i, 1,0)
      ).toSeq
  }

  private def getHeadTokenIdx(m: Mention): Int = {
   val tokenIdxInSection =  getHead(
      m.span.head.sentence.parse,
      m.start until (m.start + m.length) //these are section-level offsets
    )
    val tokenIdxInSpan = tokenIdxInSection - m.span.start
    assert(tokenIdxInSpan >= 0 && tokenIdxInSpan <= m.span.length)
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

  private def nounPhraseSpans(doc: Document, nounFilter: Token => Boolean): Seq[Mention] = (

    for (section <- doc.sections; s <- section.sentences;
         usedTokens = new HashSet[Token]();
         (t, si) <- s.tokens.zipWithIndex if nounFilter(t)) yield {

      //These two filtering rules are from 'Mention Detection: Heuristics for the OntoNotes annotations'
      val prevTokenIsCopular = si > 0 && copularVerbs.contains(s.tokens(si-1).string.toLowerCase)
      val copularPhrase = s.parse.parentIndex(si)!= -1 && prevTokenIsCopular && s.parse.parent(si).posLabel.value == "VB"

      val parentIsNoun = (s.parse.parent(t) ne null) && isNoun(s.parse.parent(t))
      val prevWordIsComma = t.hasPrev && t.prev.string == ","
      val prevPhraseIsNP = if(si > 1) usedTokens.contains(s.tokens(si - 2)) else false
      val apposition =  parentIsNoun && prevWordIsComma && prevPhraseIsNP

      // skip tokens that are:
      //   1. part of a copular phrase,
      //   2. appositive clauses,
      //   3. other nouns in a noun phrase
      if (copularPhrase || apposition || parentIsNoun) None
      else {
        val subtree = (Seq(t) ++ s.parse.subtree(si)).sortBy(_.position)
        usedTokens ++= subtree
        val (start, length) = subtree.size match {
          // leaf of the parse
          case 1 => t.position -> 1
          // non-leaf
          case _ => subtree.head.position -> (subtree.last.position - subtree.head.position + 1)
        }
        val headTokenIndexInSpan = t.position - start
        val res = Some(Mention(section, start, length,headTokenIndexInSpan))

        res
      }

    }).flatten.toSeq



  private def removeSmallerIfHeadWordEqual(doc: Document, mentions: Seq[Mention]): Seq[Mention] =
    mentions
      .groupBy( m => m.headTokenIndex + m.span.start)
      .map { case (_, mentionSeq) => mentionSeq.maxBy(_.length) }
      .toSeq

  private def dedup(mentions: Seq[Mention]): Seq[Mention] = {
    // Note: equality is only in the first set of arguments for case classes
    case class MentionStartLength(start: Int, length: Int)(val mention: Mention) {
      def this(mention: Mention) = this(mention.start, mention.length)(mention)
    }

    (for (m <- mentions) yield new MentionStartLength(m))
      .toSet
      .map { m: MentionStartLength => m.mention }
      .toSeq
  }


  def process1(doc: Document): Document = {

    var docMentions = new ArrayBuffer[Mention]
    // NAM = proper noun, NOM = common noun, PRO = pronoun
    docMentions ++= nerSpans(doc)                       map(  m => {m.attr += new MentionType(m,"NAM");m})
    docMentions ++= personalPronounSpans(doc)           map(  m => {m.attr += new MentionType(m,"PRO");m})
    docMentions ++= nounPhraseSpans(doc, isCommonNoun)  map(  m => {m.attr += new MentionType(m,"NOM");m})
    docMentions ++= nounPhraseSpans(doc, isProperNoun)  map(  m => {m.attr += new MentionType(m,"NAM");m})
    // Filter Mentions that have no MentionType and that are longer than 5 words -akm
    //doc.attr += (new MentionList() ++= removeSmallerIfHeadWordEqual(doc, dedup(docMentions)).filter(mention => (mention.attr[MentionType] ne null) && mention.span.length < 6).toSeq)
    doc.attr += (new MentionList() ++= removeSmallerIfHeadWordEqual(doc, dedup(docMentions)).filter(mention => mention.attr[MentionType] ne null).toSeq)

    doc
  }

  def prereqAttrs: Iterable[Class[_]] = Seq(classOf[parse.ParseTree])
  def postAttrs: Iterable[Class[_]] = Seq(classOf[MentionList])
  override def tokenAnnotationString(token:Token): String = token.document.attr[MentionList].filter(mention => mention.span.contains(token)) match { case ms:Seq[Mention] if ms.length > 0 => ms.map(m => m.attr[MentionType].categoryValue+":"+m.span.indexOf(token)).mkString(","); case _ => "_" }

}


