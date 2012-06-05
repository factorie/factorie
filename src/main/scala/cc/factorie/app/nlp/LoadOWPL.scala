package cc.factorie.app.nlp

import cc.factorie._

/**
 * Author: martin
 * Date: 2/25/12
 *
 * On each line, there should be a space-separated label and word.
 * Sentences are separated by blank lines.
 */
object LoadOWPL {
  def fromFilename(file: String, labelMaker: (Token, String) => LabelVariable[String], takeOnly: Int = -1): Seq[Document] = {
    val doc = new Document("", "")
    var sentence = new Sentence(doc)
    var numSentences = 1
    for (line <- io.Source.fromFile(file).getLines) {
      if (line.trim == "") {
        sentence = new Sentence(doc)
        numSentences += 1
        if (takeOnly > -1 && numSentences > takeOnly)
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
    Seq(doc)
  }
}