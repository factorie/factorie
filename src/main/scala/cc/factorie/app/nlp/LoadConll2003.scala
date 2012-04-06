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
import cc.factorie._
import cc.factorie.app.nlp.ner._
import collection.mutable.ArrayBuffer

object Conll2003NerDomain extends CategoricalDomain[String]
class Conll2003ChainNerLabel(token:Token, initialValue:String) extends ChainNerLabel(token, initialValue) {
  def domain = Conll2003NerDomain
}

object LoadConll2003 {
  def fromFilename(filename:String, BILOU:Boolean = false): Seq[Document] = {
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
        // fields = word part-of-speech shallow-parse(IOB) ner-label(IOB)
        println(fields.mkString(","))
	assert(fields.length == 4)
        val word = fields(0)
        val partOfSpeech = fields(1)
        val ner = fields(3).stripLineEnd
        if (sentence.length > 0) document.appendString(" ")
        val token = new Token(sentence, word)

        if (false && document.stringLength < 100) {
          println("word=%s documentlen=>%s<".format(word, document.string))
          //println("token start,end %d,%d".format(token.stringStart, token.stringLength))
          println("token=%s".format(token.string))
          println()
        }
        token.attr += new Conll2003ChainNerLabel(token, ner)
		//println("token: " + token + " Ner: " + ner)
		token.attr += new cc.factorie.app.nlp.pos.PosLabel(token, partOfSpeech)
      }
    }
    if(BILOU) convertToBILOU(documents)
	/*for(doc <- documents) {
		for(token <- doc.tokens) {
			if(token.attr[ChainNerLabel].categoryValue.trim().length == 0) {
				println("Something Bad Happened")
				println(token + " - " + token.attr[ChainNerLabel].categoryValue.trim())
			}
		}
	}*/
    //sentence.stringLength = document.stringLength - sentence.stringStart
    println("Loaded "+documents.length+" documents with "+documents.map(_.sentences.size).sum+" sentences with "+documents.map(_.length).sum+" tokens total from file "+filename)
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
          token.attr.remove[ChainNerLabel]
          token.attr += new Conll2003ChainNerLabel(token, newLabel)
        }
      }
    }
    //printthings(documents);
  }

  def printthings(documents : ArrayBuffer[Document]) {
    for(doc <- documents) {
      for(sentence <- doc.sentences) {
        for(token <- sentence.tokens) {
          val ner = token.nerLabel
          print(token + " -> " + ner.categoryValue + "\t");
        }
      }
      println(" ")
    }
  }
  def IOBtoBILOU(prev : Token, token : Token,  next : Token) : String = {
    if(token.nerLabel.categoryValue == "O") return "O";
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
  
  def splitLabel(token : Token) : Array[String] = {
    if(token.nerLabel.categoryValue.contains("-"))
      token.nerLabel.categoryValue.split("-")
    else
      Array("", "O")
  }
}
