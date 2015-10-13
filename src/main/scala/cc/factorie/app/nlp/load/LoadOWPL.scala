/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.load

import cc.factorie.app.nlp.{Document, Sentence, Token, UnknownDocumentAnnotator}
import cc.factorie.variable.MutableCategoricalVar

/**
 * Author: martin, strubell
 * Date: 2/25/12, 4/28/14
 *
 * On each line, there should be a whitespace-delimited list of the form:
 * word label1 label2 label3 ...
 *
 * Sentences are separated by blank lines.
 * Returns all data in a single document.
 *
 * labelMaker is a function that takes all fields but the first in a line of the file,
 * and returns a sequence of MutableCategoricalVars corresponding to the tags that
 * any subset of those fields represent
 */

object LoadOWPL {
  def fromFilename(file: String, labelMaker: (Token, Seq[String]) => Seq[MutableCategoricalVar[String]], separator: String = "\\s+", limitSentenceCount: Int = -1): Seq[Document] = {
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
        val fields = line.split(separator)
        val word = fields(0)
        val token = new Token(sentence, word)
        labelMaker(token, fields.drop(1)).foreach(token.attr += _)
        doc.appendString(" ")
      }
    }
    doc.asSection.chainFreeze
    //println("LoadOWPL doc.tokens.length="+doc.tokens.length+" last token: "+doc.tokens.last.string+" "+doc.tokens.last.attr)
    Seq(doc)
  }
}