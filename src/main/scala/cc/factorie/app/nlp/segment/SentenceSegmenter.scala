package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{Sentence, Document}

/**
 * Author: martin
 * Date: 11/23/11
 */

// Provides sentence spans to the input document.  The document should already be tokenized by nlp.segment.Tokenizer.
// This segmenter will need generalization if documents are to be tokenized by any other method.
class SentenceSegmenter {
  def process(documents: Seq[Document]): Unit = documents.map(d => process(d))
  def process(document: Document): Unit = {
    val endingIdxs = document.tokens.filter {
      t =>
        val s = t.string
        s == "." ||
        s == ".\"" ||
        s == "?" ||
        s == "?\""
      } map(_.position)   // indices for sentence-ending tokens
    var prevIdx = 0
    for (idx <- endingIdxs) {
      new Sentence(document, prevIdx, idx - prevIdx + 1)
      prevIdx = idx + 1
    }
  }
}

object SentenceSegmenter extends SentenceSegmenter {  // why do I need to extend? Isn't this a companion object? --brian
  def main(args: Array[String]): Unit = {
    val doc = new Document("name", strValue = io.Source.fromFile(args(0)).getLines.mkString("\n"))
    Tokenizer.process(doc)
    SentenceSegmenter.process(doc)
    for (sent <- doc.sentences)
      print("\n\n" + sent.tokens.map(_.string).mkString(" | "))
  }
}