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

import scala.io.Source
import cc.factorie.app.nlp.pos.PosLabel
import cc.factorie.app.nlp.parse.ParseTree

/*
 * Loader for the CoNLL 2008 closed-track shared task data.
 * Details on the format are available at http://barcelona.research.yahoo.net/dokuwiki/doku.php?id=conll2008:format
 *
 * @author Brian Martin
 */

object LoadConll2008 {
  private def addDepInfo(s: Sentence, depInfoSeq: Seq[(Int,Int,String)]): Unit = {
    val tree = new ParseTree(s)
    for ((childIdx, parentIdx, depLabel) <- depInfoSeq) {
      tree.setParent(childIdx, parentIdx)
      tree.label(childIdx).setCategory(depLabel)(null)
    }
    s.attr += tree
  }

  def fromFilename(filename:String): Seq[Document] = {
    var document: Document = new Document("Conll2008", "")
    val source = Source.fromFile(filename)
    var sentence: Sentence = new Sentence(document)(null)
    var depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addDepInfo(sentence, depInfoSeq)
        depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        sentence = null
      } else {
        if (sentence eq null)
          sentence = new Sentence(document)(null) // avoids empty sentence at the end of doc
        val fields = line.split('\t')
        assert(fields.length >= 10)
        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)
        val partOfSpeech = fields(3)
        val parentIdx = fields(8).toInt - 1
        val depLabel = fields(9)
        document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new PosLabel(token, partOfSpeech)
        depInfoSeq.append((currTokenIdx, parentIdx, depLabel))
      }
    }
    if (sentence ne null)
      addDepInfo(sentence, depInfoSeq)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.length+" tokens total from file "+filename)
    Seq(document)
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)

}
