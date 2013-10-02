package cc.factorie.app.nlp.load

import cc.factorie._
import cc.factorie.variable.LabeledCategoricalVariable
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.UnknownDocumentAnnotator

/**
 * Author: martin
 * Date: 2/25/12
 *
 * On each line, there should be a space-separated label and word.
 * Sentences are separated by blank lines.
 * Returns all data in a single document.
 */

// TODO I want to change this so that the expected format is:
// word label1 label2 label3...

object LoadOWPL {
  def fromFilename(file: String, labelMaker: (Token, String) => LabeledCategoricalVariable[String], limitSentenceCount: Int = -1): Seq[Document] = {
    val doc = new Document
    doc.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    doc.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    var sentence = new Sentence(doc)
    var numSentences = 1
    for (line <- io.Source.fromFile(file).getLines()) {
      if (line.trim == "") {
        sentence = new Sentence(doc)
        numSentences += 1
        if (limitSentenceCount > -1 && numSentences > limitSentenceCount)
          return Seq(doc)
      }
      else {
        val fields = line.split(" ")
        val label = fields(0)
        val word = fields(1).substring(5)
        val token = new Token(sentence, word)
        token.attr += labelMaker(token, label)
        doc.appendString(" ")
      }
    }
    doc.asSection.chainFreeze
    //println("LoadOWPL doc.tokens.length="+doc.tokens.length+" last token: "+doc.tokens.last.string+" "+doc.tokens.last.attr)
    Seq(doc)
  }
}