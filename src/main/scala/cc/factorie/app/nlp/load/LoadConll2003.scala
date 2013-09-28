/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
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

import cc.factorie._
import cc.factorie.app.nlp.ner._
import collection.mutable.ArrayBuffer
import cc.factorie.util.FastLogging
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.Token
import cc.factorie.app.nlp.UnknownDocumentAnnotator
import cc.factorie.app.nlp.pos.PennPosLabel

// Usage:
// Either LoadConll2003.fromFilename("foo")
// or LoadConll2003(BILOU = true).fromFilename("foo")

object LoadConll2003 extends LoadConll2003(false)

case class LoadConll2003(BILOU:Boolean = false) extends Load with FastLogging {
  val conllToPennMap = Map("\"" -> "''", "(" -> "-LRB-", ")" -> "-RRB-", "NN|SYM" -> "NN")

  def fromSource(source:io.Source): Seq[Document] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer
    def newDocument(name:String): Document = {
      val document = new Document("").setName(name)
      document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
      document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
      document.annotators(classOf[pos.PennPosLabel]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
      document
    }

    val documents = new ArrayBuffer[Document]
    var document = newDocument("CoNLL2003-"+documents.length)
    documents += document
    var sentence = new Sentence(document)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        //sentence.stringLength = document.stringLength - sentence.stringStart
        //document += sentence
        document.appendString("\n")
        sentence = new Sentence(document)
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
        document.asSection.chainFreeze
        document = new Document().setName("CoNLL2003-"+documents.length)
        document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
        document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
        documents += document
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = conllToPennMap.getOrElse(fields(1), fields(1))
        val ner = fields(3).stripLineEnd
        if (sentence.length > 0) document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new BioConllNerLabel(token, ner)
        token.attr += new cc.factorie.app.nlp.pos.PennPosLabel(token, partOfSpeech)
      }
    }
    if (BILOU) convertToBILOU(documents)
    //sentence.stringLength = document.stringLength - sentence.stringStart
    logger.info("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.tokens.size).sum+" tokens total")
    documents
  }
  def convertToBILOU(documents : ArrayBuffer[Document]) {
    for(doc <- documents) {
      for(sentence <- doc.sentences) {
        for(token <- sentence.tokens) {
          //println("=======")
          val ner = token.nerLabel
          var prev : Token = null
          var next : Token = null
          //println(token + " -> " + ner.categoryValue);
          if(token.sentenceHasPrev) prev = token.sentencePrev
          if(token.sentenceHasNext) next = token.sentenceNext
          token.sentenceNext
          /*
          if(prev != null)
            println(prev + " -> " + prev.nerLabel.categoryValue);
          if(next != null)
            println(next + " -> " + next.nerLabel.categoryValue); */
          val newLabel : String = IOBtoBILOU(prev, token, next)
          /*if(token.string == "Peter")
            println(newLabel)
          if(token.prev != null && token.prev.string == "Peter") {
            println("Peter Prev")
            println(token.string)
            println(newLabel)
          }*/
          // token.attr.remove[BioConllNerLabel]
          token.attr += new BilouConllNerLabel(token, newLabel)
        }
      }
    }
  }

  def IOBtoBILOU(prev : Token, token : Token,  next : Token) : String = {
    if(token.nerLabel.categoryValue == "O") return "O"
    // The major case that needs to be converted is I, which is dealt with here
    val ts = token.nerLabel.categoryValue.split("-")
    var ps : Array[String] = null
    var ns : Array[String] = null
    if(prev != null)
      ps = splitLabel(prev)
    if(next != null)
      ns = splitLabel(next)

    if(token.nerLabel.categoryValue.contains("B-")) {
    if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
    else
        return token.nerLabel.categoryValue
    }

    if(prev == null || ps(1) != ts(1)) {
      if(next == null || ns(1) != ts(1) || ns(0) == "B")
        return "U-" + ts(1)
      return "B-" + ts(1)     
    }
    if(next == null || ns(1) != ts(1) || ns(0) == "B")
      return "L-" + ts(1)
    "I-" + ts(1)
  }
  
  private def splitLabel(token : Token) : Array[String] = {
    if(token.nerLabel.categoryValue.contains("-"))
      token.nerLabel.categoryValue.split("-")
    else
      Array("", "O")
  }
}


