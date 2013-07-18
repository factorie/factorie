package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp._

/** Author: martin */

// Provides sentence spans to the input document.  The document should already be tokenized by nlp.segment.Tokenizer.
// This segmenter will need generalization if documents are to be tokenized by any other method.
class SentenceSegmenter extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = null

  val lastTokenRegex = "^[.?!][\\p{Pe}\\p{Pf}]?$|^[\\p{Pe}\\p{Pf}]?[.?!]$".r
  def process(documents: Seq[Document])(implicit m: DocumentAnnotatorMap): Unit = documents.map(d => process(d))
  def process1(document: Document): Document = {
    for (section <- document.sections) {
      val endingIdxs = section.tokens.filter(token => lastTokenRegex.findFirstIn(token.string) != None).map(_.position)
      var prevIdx = 0
      for (idx <- endingIdxs) {
        new Sentence(section, prevIdx, idx - prevIdx + 1)
        prevIdx = idx + 1
      }
    }
    document
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[Sentence])
}

object SentenceSegmenter extends SentenceSegmenter {
  def main(args: Array[String]): Unit = {
    for (filename <- args) {
      val doc = new Document(io.Source.fromFile(filename).mkString).setName(filename)
      RegexTokenizer.process1(doc)
      this.process1(doc)
      println(filename)
      for (sentence <- doc.sentences)
        print("\n\n" + sentence.tokens.map(_.string).mkString(" | "))
      print("\n\n\n")
    }
  }
}
