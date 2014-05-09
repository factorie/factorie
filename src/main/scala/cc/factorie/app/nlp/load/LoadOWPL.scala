package cc.factorie.app.nlp.load

import cc.factorie._
import cc.factorie.variable.LabeledMutableCategoricalVar
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.UnknownDocumentAnnotator

/**
 * Author: martin, strubell
 * Date: 2/25/12, 4/28/14
 *
 * On each line, there should be a whitespace-delimited list of the form:
 * word label1 label2 label3 ...
 * 
 * Sentences are separated by blank lines.
 * Returns all data in a single document.
 */

object LoadOWPL {
  def fromFilename(file: String, labelMaker: (Token, Seq[String]) => LabeledMutableCategoricalVar[String], limitSentenceCount: Int = -1): Seq[Document] = {
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
        val fields = line.split("\\s+")
        val word = fields(0)
        val token = new Token(sentence, word)
        token.attr += labelMaker(token, fields)
        doc.appendString(" ")
      }
    }
    doc.asSection.chainFreeze
    //println("LoadOWPL doc.tokens.length="+doc.tokens.length+" last token: "+doc.tokens.last.string+" "+doc.tokens.last.attr)
    Seq(doc)
  }
}