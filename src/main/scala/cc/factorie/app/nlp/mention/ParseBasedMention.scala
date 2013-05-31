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

  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS exists(_ == t.posLabel.categoryValue.toUpperCase())
  private def isCommonNoun     (t: Token) = COMMON_NOUNS      exists(_ == t.posLabel.categoryValue.toUpperCase())
  private def isProperNoun     (t: Token) = PROPER_NOUNS      exists(_ == t.posLabel.categoryValue.toUpperCase())
  private def isNoun           (t: Token) = ALL_NOUNS         exists(_ == t.posLabel.categoryValue.toUpperCase())

  private def nerSpans(doc: Document): Seq[Mention] = {
    (for (section <- doc.sections; span <- section.spansOfClass[NerSpan]) yield
      Mention(section, span.start, span.length, span.start - span.sentence.start  + span.length - 1) //this sets the head token idx to be the last token in the span  //todo: s.start is offset in document, right?
      ).toSeq
  }

  // [Assumes personal pronouns are single tokens.]
  private def personalPronounSpans(doc: Document): Seq[Mention] = {
    (for (section <- doc.sections; s <- section.sentences;
           (t,i) <- s.tokens.zipWithIndex if isPersonalPronoun(t)) yield
        Mention(section, s.start + i, 1,s.start+i)
      ).toSeq
  }

  private def getHeadTokenIdx(m: Mention): Int = {
    getHead(
      m.span.head.sentence.parse, // much more efficient than the commented line below
      //m.document.sentenceContaining(m.document(m.start)).parse,
      m.start until (m.start + m.length) // TODO: is the until correct here?
    )
  }
  //this expects as input indices in the **document** not the sentence
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
    curr + parse.sentence.start  //this shifts it back to have document-level indices
  }

  private def containedInInterval(left: Int, right: Int, testIndex: Int): Boolean = {
    testIndex >= left && testIndex <= right
  }

  final val copularVerbs = collection.immutable.HashSet[String]() ++ Seq("is","are","was","'m")

  private def nounPhraseSpans(doc: Document, nounFilter: Token => Boolean): Seq[Mention] = (

    for (section <- doc.sections; s <- section.sentences;
         usedTokens = new HashSet[Token]();
         (t, si) <- s.tokens.zipWithIndex if nounFilter(t)) yield {

      val di = s.start + si
      //These two filtering rules are from 'Mention Detection: Heuristics for the OntoNotes annotations'
      val prevTokenIsCopular = if(si > 0) copularVerbs.contains(s.tokens(si-1).string.toLowerCase) else false
      val copularPhrase = if(s.parse.parentIndex(si)!= -1) prevTokenIsCopular && (s.parse.parent(si).attr[PTBPosLabel].value == "VB"  )  else false

      val parentIsNoun = if(s.parse.parentIndex(si) == -1) false else isNoun(s.parse.parent(si))
      val prevWordIsComma = if(si > 0) s.tokens(si -1).string == "," else false
      val prevPhraseIsNP = if(si > 1) usedTokens.contains(s.tokens(si - 2)) else false
      val apposition =  parentIsNoun && prevWordIsComma && prevPhraseIsNP

      // skip tokens that are already added as part of a larger subtree or aren't nouns
      if (copularPhrase || apposition) None
      else {
        val subtree = (Seq(t) ++ s.parse.children(si)).sortBy(_.position)
        usedTokens ++= subtree
        val (start, length) = subtree.size match {
          // leaf of the parse
          case 1 => t.position -> 1
          // non-leaf
          case _ => subtree.head.position -> (subtree.last.position - subtree.head.position + 1)
        }

        val res = Some(Mention(section, start, length,si))

        res
      }

    }).flatten.toSeq



  private def removeSmallerIfHeadWordEqual(doc: Document, mentions: Seq[Mention]): Seq[Mention] =
    mentions
      .groupBy(_.headTokenIndex)
      .map { case (_, mentions) => mentions.maxBy(_.length) }
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
    docMentions ++= nerSpans(doc)                      map(  m => {m.attr += new MentionType(m,"NAM");m})
    docMentions ++= personalPronounSpans(doc)           map(  m => {m.attr += new MentionType(m,"PRO");m})
    docMentions ++= nounPhraseSpans(doc, isCommonNoun)  map(  m => {m.attr += new MentionType(m,"NOM");m})
    docMentions ++= nounPhraseSpans(doc, isProperNoun)  map(  m => {m.attr += new MentionType(m,"NAM");m})

    docMentions = docMentions map { m => m.copy(headTokenIndex = getHeadTokenIdx(m)) }

    doc.attr += (new MentionList() ++= removeSmallerIfHeadWordEqual(doc, dedup(docMentions)).toSeq)

    doc
  }

  // TODO: to implement
  def prereqAttrs: Iterable[Class[_]] = Seq()
  def postAttrs: Iterable[Class[_]] = Seq() // TODO: what is this for?

}


