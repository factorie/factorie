package cc.factorie.app.nlp.mention
import cc.factorie.app.nlp._
import collection.mutable.ArrayBuffer

// TODO Once Mention is a TokenSpan, make this inherit from Mention. -akm
class NounChunk(sec:Section, initialStart:Int, initialLength:Int) extends TokenSpan(sec, initialStart, initialLength) {
  def this (doc:Document, initialStart:Int, initialLength:Int) = this(doc.asSection, initialStart, initialLength)
}

class NounChunkList extends TokenSpanList[NounChunk]

/** Find noun mentions merely by contiguous nouns (possibly prefixed by adjectives) and pronouns. */
object NounChunker1 extends DocumentAnnotator {
  def process(document:Document): Document = {
    val chunks = new NounChunkList
    var tempSpan: NounChunk = null
    for (section <- document.sections; token <- section.tokens) {
      // Put a span around contiguous sequences of NN or PR part-of-speech prefixes
      val posPrefix = token.attr[pos.PennPosLabel].categoryValue.take(2)
      if (posPrefix == "NN" || posPrefix == "PR" || (posPrefix == "JJ" && token.hasNext && token.next.attr[pos.PennPosLabel].categoryValue.take(2) == "NN")) {
        if (tempSpan eq null) tempSpan = new  NounChunk(section, token.position, 1)
        else tempSpan.append(1)(null)
      } else if (tempSpan ne null) {
        if (token.string == "-" && token.hasNext && token.next.attr[pos.PennPosLabel].categoryValue.take(2) == "NN") tempSpan.append(1)(null) // Handle dashed nouns
        else { chunks += tempSpan; tempSpan = null}
      }
    }
    document.attr += chunks
    document
  }
  override def tokenAnnotationString(token:Token): String = {
    val chunks = token.document.attr[NounChunkList].spansContaining(token)
    if (chunks.isEmpty) return null
    chunks.map(c => if (c.head == token) "B-NOUNCHUNK" else "I-NOUNCHUNK").mkString(",")
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[pos.PennPosLabel])
  def postAttrs: Iterable[Class[_]] = List(classOf[Mention])
}
