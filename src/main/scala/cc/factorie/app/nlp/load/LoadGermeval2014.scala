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

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import cc.factorie.util.FastLogging

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/* Loader for Germeval 2014 data
   @author Peter Schueller
  1   token ID
  2   word form
  3   gold named entity tag level 1
  4   gold named entity tag level 2 (nested named entity)
 */

class LoadGermeval2014 extends Load with FastLogging {
  // competition format = BIO
  def fromSource(source:io.Source): Seq[Document] = fromSource(source,"BIO")
  // alternate format = BILOU
  def fromSource(source:io.Source, encoding:String): Seq[Document] = {
    def newDocument(name:String): Document = {
      var document = new Document("").setName(name)
      document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass
      document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass
      encoding match {
        case "BIO" => {
          document.annotators(classOf[Lvl1BioGermevalNerTag]) = UnknownDocumentAnnotator.getClass
          document.annotators(classOf[Lvl2BioGermevalNerTag]) = UnknownDocumentAnnotator.getClass }
        case "BILOU" => {
          document.annotators(classOf[Lvl1BilouGermevalNerTag]) = UnknownDocumentAnnotator.getClass
          document.annotators(classOf[Lvl2BilouGermevalNerTag]) = UnknownDocumentAnnotator.getClass }
        case _ => throw new Error("Germeval2014Load supports only BIO and BILOU encodings")
      }
      document
    }

    val documents = new ArrayBuffer[Document]
    var document = newDocument("Germeval2014-"+documents.length)
    documents += document
    var sentence = new Sentence(document)
    val rComment = """#.*""".r
    val rEmpty = """\S*""".r
    for (line <- source.getLines()) {
      line match {
        case rComment() => { } // ignore comments
        case rEmpty() => {   // empty line starts new sentence
          // be robust to double empty lines
          if (sentence.tokens.size > 0) {
            document.appendString("\n")
            document.asSection.chainFreeze
            document = newDocument("Germeval2014-"+documents.length)
            documents += document
            sentence = new Sentence(document)
          } }
        case _ => addToken(document, sentence, line, encoding)
      }
    }
    logger.info("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.tokens.size).sum+" tokens total")
    documents
  }

  def addToken(document:Document, sentence:Sentence, line:String, encoding:String): Token = {
    val fields = line.split("\t")
    val word : String = fields(1)
    val ner1gold : String = fields(2)
    val ner2gold : String = fields(3)
    if (sentence.length > 0) document.appendString(" ")
    val token = new Token(sentence, word)
    encoding match {
      case "BIO" => {
        token.attr += new LabeledLvl1BioGermevalNerTag(token, ner1gold)
        token.attr += new LabeledLvl2BioGermevalNerTag(token, ner2gold) }
      case "BILOU" => {
        token.attr += new LabeledLvl1BilouGermevalNerTag(token, ner1gold)
        token.attr += new LabeledLvl2BilouGermevalNerTag(token, ner2gold) }
    }
    token
  }
}

