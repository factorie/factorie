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
import cc.factorie.app.nlp.parse.{ParseLabel, ParseEdge}

object LoadConll2006 {
  private def addChildParentSeq(ts: Seq[Token], xs: Seq[(Token,Int)]): Unit =
    for ((child, parentIdx) <- xs)
      child.attr[ParseEdge].set(ts(parentIdx))(null)

  def fromFilename(filename:String): Seq[Document] = {
    var document: Document = new Document("Conll2006", "")
    val source = Source.fromFile(new java.io.File(filename))
    var sentence: Sentence = new Sentence(document)(null)
    var childParentSeq = Seq.empty[(Token,Int)]
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addChildParentSeq(sentence.tokens, childParentSeq)
        childParentSeq = Seq.empty[(Token,Int)]
        sentence = new Sentence(document)(null)
      } else {
        val fields = line.split('\t')
        assert(fields.length == 10)
        val word = fields(1)
        val partOfSpeech = fields(3)
        val parentIdx = fields(6).toInt - 1
        val depLabel = fields(7)
        document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new PosLabel(token, partOfSpeech)
        new ParseEdge(token, null, depLabel) // all parents are the first word
        // if the token is not <root>, record the actual parents to add once the whole sentence has been read
        if (parentIdx != -1)
          childParentSeq ++= Seq((token, parentIdx))
      }
    }
    addChildParentSeq(sentence.tokens, childParentSeq)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.length+" tokens total from file "+filename)
    Seq(document)
  }

  def printDocument(doc: Document): Unit = {
    def isParentRoot(token: Token): Boolean = token.attr[ParseEdge].parent == null
    def getRoot(sentence: Sentence): Token = sentence.tokens.filter(t => isParentRoot(t)).head

    def treeString(root: Token, sentence: Sentence): String = {
      var sb = new StringBuilder
      for (child <- root.attr[ParseEdge].children)
        sb.append(child.attr[ParseEdge].label.value + "(" + root.string + "-" + sentence.tokens.indexOf(root) + ", " + child.string + "-" + sentence.tokens.indexOf(child) + ")" + "\n" + treeString(child, sentence))
      sb.toString()
    }

    def printTree(sentence:Sentence) = println(treeString(getRoot(sentence), sentence))

    for (sent <- doc.sentences)
      printTree(sent)

  }

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)

}
