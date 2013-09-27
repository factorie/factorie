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


import scala.io.Source
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.lemma.TokenLemma

import java.io.PrintWriter

/*
 * Loader for the CoNLL 2008 closed-track shared task data.
 * wordIndex word lemma POS parentIndex depLabel
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

  var loadLemma = true

  def fromFilename(filename:String): Seq[Document] = {
    var document: Document = new Document
    document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    document.annotators(classOf[pos.PennPosLabel]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
    if (loadLemma) document.annotators(classOf[TokenLemma]) = UnknownDocumentAnnotator.getClass // register that we have lemma
    val source = Source.fromFile(filename)
    var sentence: Sentence = new Sentence(document)
    var depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
    for (line <- source.getLines()) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addDepInfo(sentence, depInfoSeq)
        depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        sentence = null
      } else {
        if (sentence eq null)
          sentence = new Sentence(document) // avoids empty sentence at the end of doc
        val fields = line.split('\t')
        assert(fields.length >= 10)
        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)
        val lemma = fields(2)
        val partOfSpeech = fields(3)
        val parentIdx = fields(8).toInt - 1
        val depLabel = fields(9)
        document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new PennPosLabel(token, partOfSpeech) // TODO Change this to PennPosLabel
        if (loadLemma)
          token.attr += new TokenLemma(token, lemma) // TODO Change this to some more specific TokenLemma subclass
        depInfoSeq.append((currTokenIdx, parentIdx, depLabel))
      }
    }
    if (sentence ne null)
      addDepInfo(sentence, depInfoSeq)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.asSection.length+" tokens total from file "+filename)
    Seq(document)
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)

}


object WriteConll2008 {

  // if the source file is given, then include the fields that we don't know anything about
  // otherwise just give underscores for info we don't know.
  def toFile(outputFile: String, document: Document, sourceFile: String = null): Unit = {
    val source = { if (sourceFile eq null) None else Some(Source.fromFile(sourceFile).getLines())}
    val sentences = document.sentences.iterator
    val writer = new PrintWriter(outputFile)
    var sentence: Sentence = sentences.next()
    var currTokenIdx = 0
    var tree: ParseTree = sentence.parse
    while (true) {
      if (currTokenIdx == sentence.length) {
        writer.println()
        if (sentences.hasNext) {
          source match {
            case Some(source) => source.next()
            case _ => ()
          }
          sentence = sentences.next()
          tree = sentence.parse
          currTokenIdx = 0
        }
        else {
          writer.close()
          return
        }
      }
      else {
        val field8 = "" + (tree.parentIndex(currTokenIdx) + 1)
        val field9 = "" + { val category = tree.label(currTokenIdx).categoryValue; if (category == "") "_" else category }
        val fields = source match {
          case None => {
            val x = Array.fill[String](10)("_")
            x(0) = "" + (currTokenIdx + 1)
            x(1) = sentence.tokens(currTokenIdx).string
            x(3) = sentence.tokens(currTokenIdx).posLabel.categoryValue
            x(8) = field8
            x(9) = field9
            x
          }
          case Some(source) => {
            val x = source.next().split("\t")
            x(8) = field8
            x(9) = field9
            x
          }
        }
        currTokenIdx += 1
        writer.println(fields.mkString("\t"))
      }
    }
  }

}
