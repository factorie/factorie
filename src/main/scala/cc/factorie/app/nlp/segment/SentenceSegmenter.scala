package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{Sentence, Document}

/** Author: martin */

// Provides sentence spans to the input document.  The document should already be tokenized by nlp.segment.Tokenizer.
// This segmenter will need generalization if documents are to be tokenized by any other method.
class SentenceSegmenter {
  val lastTokenRegex = "^[.?!][\\p{Pe}\\p{Pf}]?$|^[\\p{Pe}\\p{Pf}]?[.?!]$".r
  def process(documents: Seq[Document]): Unit = documents.map(d => process(d))
  def process(document: Document): Unit = {
    val endingIdxs = document.tokens.filter(token => lastTokenRegex.findFirstIn(token.string) != None).map(_.position)
    var prevIdx = 0
    for (idx <- endingIdxs) {
      new Sentence(document, prevIdx, idx - prevIdx + 1)
      prevIdx = idx + 1
    }
  }
}

object SentenceSegmenter extends SentenceSegmenter {
  def main(args: Array[String]): Unit = {
    for (filename <- args) {
      val doc = new Document(filename, strValue = io.Source.fromFile(filename).mkString)
      Tokenizer.process(doc)
      this.process(doc)
      println(filename)
      for (sentence <- doc.sentences)
        print("\n\n" + sentence.tokens.map(_.string).mkString(" | "))
      print("\n\n\n")
    }
  }
}
