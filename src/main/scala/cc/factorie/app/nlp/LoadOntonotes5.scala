package cc.factorie.app.nlp

import scala.io.Source
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PTBPosLabel
import cc.factorie.app.nlp.ner.BioOntonotesNerLabel
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.lemma.TokenLemma

import java.io.PrintWriter

/* Loader for the OntoNotes 5 data
   @author Brian Martin, Andrew McCallum
  1   token ID
  2   word form
  3   gold lemma
  4   auto lemma
  5   gold POS tag
  6   auto POS tag
  7   gold feats
  8   gold head ID
  9   auto head ID
  10  gold dependency label
  11  auto dependency label
  12  gold secondary dependencies
  13  gold semantic arguments
  14  gold named entity tags
  15  gold coreference
  
 */

object LoadOntonotes5 {
  private def addDepInfo(s: Sentence, depInfoSeq: Seq[(Int,Int,String)]): Unit = {
    //assert(depInfoSeq.map(_._1) == Seq.tabulate(depInfoSeq.length)(i => i), "Token indices: "+depInfoSeq.map(_._1).mkString(" ")) // Assert that we have an entry for each token index, in order
    val tree = new ParseTree(s, depInfoSeq.map(_._2), depInfoSeq.map(_._3))
    s.attr += tree
  }

  var loadLemma = true
  var loadPos = true
  var loadNer = true

  def fromFilename(filename:String): Seq[Document] = {
    val document: Document = new Document().setName("Ontonotes499" + filename)
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
        val lemma = fields(2) // was 3
        val partOfSpeech = fields(4)
        val parentIdx = fields(8).toInt - 1
        val depLabel = fields(10)
        var ner = fields(13); if (ner == "_") ner = "O"
        // Alex: we can't ignore XX tokens or the parser trees end up malformed.
        // if (partOfSpeech != "XX") { // Skip words marked with part-of-speech "XX"
          document.appendString(" ")
          val token = new Token(sentence, word)
          if (loadPos) token.attr += new PTBPosLabel(token, if (partOfSpeech == "XX") "PUNC" else partOfSpeech)
          if (loadNer) token.attr += new BioOntonotesNerLabel(token, ner)
          if (loadLemma) token.attr += new TokenLemma(token, lemma) // TODO Change this to some more specific TokenLemma subclass
          depInfoSeq.append((currTokenIdx, parentIdx, depLabel))
        // }
      }
    }
    if (sentence ne null)
      addDepInfo(sentence, depInfoSeq)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.wholeDocumentSection.length+" tokens total from file "+filename)
    Seq(document)
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)

}