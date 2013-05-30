package cc.factorie.app.nlp.mention
import cc.factorie.app.nlp._
import collection.mutable.ArrayBuffer

class NounMentionSpan(sec:Section, initialStart:Int, initialLength:Int) extends TokenSpan(sec, initialStart, initialLength)(null) {
  def this (doc:Document, initialStart:Int, initialLength:Int) = this(doc.wholeDocumentSection, initialStart, initialLength)
}

/** Find noun mentions merely by contiguous nouns (possibly prefixed by adjectives) and pronouns. */
object NounMention1 extends DocumentAnnotator {
  def process1(document:Document): Document = {
    val spans = ArrayBuffer[TokenSpan]()
    var tempSpan: TokenSpan = null
    for (token <- document.tokens) {
      // Put a span around contiguous sequences of NN or PR part-of-speech prefixes
      val posPrefix = token.attr[pos.PTBPosLabel].categoryValue.take(2)
      if (posPrefix == "NN" || posPrefix == "PR" || (posPrefix == "JJ" && token.hasNext && token.next.attr[pos.PTBPosLabel].categoryValue.take(2) == "NN")) {
        if (tempSpan eq null) tempSpan = new  TokenSpan(document,token.position,1)
        else tempSpan.append(1)(null)
      } else if (tempSpan ne null) {
        if (token.string == "-" && token.hasNext && token.next.attr[pos.PTBPosLabel].categoryValue.take(2) == "NN") tempSpan.append(1)(null) // Handle dashed nouns
        else { tempSpan = null; spans += tempSpan}
      }
    }
    document.attr += (new MentionList ++= spans.map(s => new Mention(s,s.start + s.length -1)))    //this uses the last token in the span as the head token
    document
  }
  override def tokenAnnotationString(token:Token): String = {
    val spans = token.spans
    if (spans.isEmpty) return null
    val span = spans.find(_.getClass.isAssignableFrom(classOf[Mention])).get
    if (span eq null) return null
    else if (span.head == token) "B-MENTION"
    else "I-MENTION"
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[pos.PTBPosLabel])
  def postAttrs: Iterable[Class[_]] = List(classOf[Mention])
}
