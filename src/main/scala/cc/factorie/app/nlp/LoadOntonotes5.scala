package cc.factorie.app.nlp

import scala.io.Source
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PosLabel
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.lemma.TokenLemma

import java.io.PrintWriter

/*
 * Loader for the OntoNotes 5 data
 * @author Brian Martin
 */

object LoadOntonotes5 {
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
    var document: Document = new Document("Ontonotes499" + filename, "")
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
        val lemma = fields(3)
        val partOfSpeech = fields(5)
        val parentIdx = fields(7).toInt - 1
        val depLabel = fields(9)
        document.appendString(" ")
        val token = new Token(sentence, word)
        token.attr += new PosLabel(token, partOfSpeech) // TODO Replace with PTBPosLabel
        if (loadLemma)
          token.attr += new TokenLemma(token, lemma) // TODO Change this to some more specific TokenLemma subclass
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