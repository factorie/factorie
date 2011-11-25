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

package cc.factorie.app.nlp

object LoadConll2003 {
  def fromFilename(filename:String): Seq[Document] = {
    import scala.io.Source
    import scala.collection.mutable.ArrayBuffer

    val documents = new ArrayBuffer[Document]
    var document = new Document("CoNLL2003-"+documents.length, "")
    documents += document
    val source = Source.fromFile(new java.io.File(filename))
    var sentence = new Sentence(document)(null)
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        //sentence.stringLength = document.stringLength - sentence.stringStart
        //document += sentence
        document.appendString("\n")
        sentence = new Sentence(document)(null)
      } else if (line.startsWith("-DOCSTART-")) {
        // Skip document boundaries
        document = new Document("CoNLL2003-"+documents.length, "")
        documents += document
      } else {
        val fields = line.split(' ')
        assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = fields(1)
        val ner = fields(3).stripLineEnd
        document.appendString(" ")
        val token = new Token(sentence, word)
        if (false && document.stringLength < 100) {
          println("word=%s documentlen=>%s<".format(word, document.string))
          println("token start,end %d,%d".format(token.stringStart, token.stringLength))
          println("token=%s".format(token.string))
          println()
        }
        token.attr += new cc.factorie.app.nlp.ner.ChainNerLabel(token, ner)
        token.attr += new cc.factorie.app.nlp.pos.PosLabel(token, partOfSpeech)
      }
    }
    //sentence.stringLength = document.stringLength - sentence.stringStart
    println("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.length).sum+" tokens total from file "+filename)
    documents
  }

}
