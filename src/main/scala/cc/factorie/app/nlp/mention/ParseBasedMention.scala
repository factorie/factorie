package cc.factorie.app.nlp.mention

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.{Document, DocumentAnnotator}
import cc.factorie.app.nlp.ner.NerSpan

object ParseBasedMentionFinding extends DocumentAnnotator {
  
  private case class DocMention(
      doc: Document,
      start: Int,
      length: Int,
      mentionType: String = "", // NAM = proper noun, NOM = common noun, PRO = pronoun
      headTokenIdx: Int = -1
  ){ def addAsSpan(): Unit = new NounMentionSpan(doc, start, length) }
  
  private final val PERSONAL_PRONOUNS = Seq("PRP", "PRP$")
  private final val COMMON_NOUNS      = Seq("NN" , "NNS")
  private final val PROPER_NOUNS      = Seq("NNP", "NNPS")
  
  private def isPersonalPronoun(t: Token) = PERSONAL_PRONOUNS exists(_ == t.posLabel.categoryValue.toUpperCase())
  private def isCommonNoun     (t: Token) = COMMON_NOUNS      exists(_ == t.posLabel.categoryValue.toUpperCase())
  private def isProperNoun     (t: Token) = PROPER_NOUNS      exists(_ == t.posLabel.categoryValue.toUpperCase())
  
  private def nerSpans(doc: Document): Seq[DocMention] = {
    (for (s <- doc.spansOfClass[NerSpan]) yield
       DocMention(doc, s.start, s.length)
    ).toSeq
  }
  
  // [Assumes personal pronouns are single tokens.]
  private def personalPronounSpans(doc: Document): Seq[DocMention] = (
    for (s <- doc.sentences;
         (t,i) <- s.tokens.zipWithIndex if isPersonalPronoun(t)) yield
      DocMention(doc, s.start + i, 1)
    ).toSeq
    
  private def getHeadTokenIdx(m: DocMention): Int = getHead(
    m.doc.sentenceContaining(m.doc(m.start)).parse,
    m.start until (m.start + m.length) // TODO: is the until correct here?
  )
  
  private def getHead(parse: ParseTree, subtree: Seq[Int]): Int = {
    var curr = subtree.head - parse.sentence.start
    while (parse.parentIndex(curr) > 0 && subtree.contains(parse.parentIndex(curr)))
      curr = parse.parentIndex(curr)
    curr + parse.sentence.start
  }
    
  private def nounPhraseSpans(doc: Document, nounFilter: Token => Boolean): Seq[DocMention] = (
    for (s <- doc.sentences;
         included = new HashSet[Token]();
         (t, si) <- s.tokens.zipWithIndex if nounFilter(t)) yield {
      
      val di = s.start + si
      
      // this catches the strange case where t.sentencePosition is -1
      //assert(t.sentencePosition == si, "%d %d, %d %s %s \n %s" format 
      //  (si, t.sentencePosition, t.position, t, t.sentence, t.sentence.tokens.map(t => t.string -> t.sentencePosition -> t.position).mkString("\n")))
 
      // skip tokens that are already added as part of a larger subtree or aren't nouns
      if (included.contains(t)) None
      else {
        val subtree = (Seq(t) ++ s.parse.children(si)).sortBy(_.position)
        val (start, length) = subtree.size match {
          // leaf of the parse
          case 0 => t.position -> 1
          // non-leaf
          case _ => subtree.head.position -> (subtree.last.position - subtree.head.position + 1)
        }
        
        val res = Some(DocMention(doc, start, length))
        
        res
      }
      
    }).flatten.toSeq
    
  private def removeSmallerIfHeadWordEqual(doc: Document, mentions: Seq[DocMention]): Seq[DocMention] =
    mentions
      .groupBy(_.headTokenIdx)
      .map { case (_, mentions) => mentions.maxBy(_.length) }
      .toSeq
        
  private def dedup(mentions: Seq[DocMention]): Seq[DocMention] = {
    // Note: equality is only in the first set of arguments for case classes
    case class MentionStartLength(start: Int, length: Int)(val mention: DocMention) { 
      def this(mention: DocMention) = this(mention.start, mention.length)(mention)
    }
    
    (for (m <- mentions) yield new MentionStartLength(m))
      .toSet
      .map { m: MentionStartLength => m.mention }
      .toSeq
  }
    
  def process1(doc: Document): Document = {
    
    var docMentions = new ArrayBuffer[DocMention]
    // NAM = proper noun, NOM = common noun, PRO = pronoun
    docMentions ++= nerSpans(doc)                       map { m => m.copy(mentionType = "NAM")}
    docMentions ++= personalPronounSpans(doc)           map { m => m.copy(mentionType = "PRO")}
    docMentions ++= nounPhraseSpans(doc, isCommonNoun)  map { m => m.copy(mentionType = "NOM")}
    docMentions ++= nounPhraseSpans(doc, isProperNoun)  map { m => m.copy(mentionType = "NAM")}
    
    docMentions = docMentions map { m => m.copy(headTokenIdx = getHeadTokenIdx(m)) }
    
    removeSmallerIfHeadWordEqual(doc, dedup(docMentions)).map(_.addAsSpan())
    
    doc
  }
  
  // TODO: to implement
  def prereqAttrs: Iterable[Class[_]] = Seq()
  def postAttrs: Iterable[Class[_]] = Seq() // TODO: what is this for?
  
}
