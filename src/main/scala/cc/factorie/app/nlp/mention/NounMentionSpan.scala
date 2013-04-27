package cc.factorie.app.nlp.mention
import cc.factorie.app.nlp._

class NounMentionSpan(doc:Document, initialStart:Int, initialLength:Int) extends TokenSpan(doc, initialStart, initialLength)(null)

/** Find noun mentions merely by contiguous nouns (possibly prefixed by adjectives) and pronouns. */
object NounMention1 extends DocumentAnnotator {
  def process1(document:Document): Document = {
    var span: NounMentionSpan = null
    for (token <- document.tokens) {
      // Put a span around contiguous sequences of NN or PR part-of-speech prefixes
      val posPrefix = token.attr[pos.PTBPosLabel].categoryValue.take(2)
      if (posPrefix == "NN" || posPrefix == "PR" || (posPrefix == "JJ" && token.hasNext && token.next.attr[pos.PTBPosLabel].categoryValue.take(2) == "NN")) {
        if (span eq null) span = new NounMentionSpan(document, token.position, 1)
        else span.append(1)(null)
      } else if (span ne null) span = null
    }
    document
  }
  override def tokenAnnotationString(token:Token): String = {
    val spans = token.spans
    if (spans.isEmpty) return null
    val span = spans.find(_.getClass.isAssignableFrom(classOf[NounMentionSpan])).get
    if (span eq null) return null
    else if (span.head == token) "B-MENTION"
    else "I-MENTION"
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[pos.PTBPosLabel])
  def postAttrs: Iterable[Class[_]] = List(classOf[NounMentionSpan])
}
