package cc.factorie.app.nlp.load
import cc.factorie.app.nlp._


import scala.io.Source
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.PennPosLabel
import cc.factorie.app.nlp.ner.{BioOntonotesNerLabel,BilouOntonotesNerLabel}
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

  def fromFilename(filename:String, loadLemma:Boolean = true, loadPos:Boolean = true, loadNer:Boolean = true, nerBilou:Boolean = false): Seq[Document] = {
    val document: Document = new Document().setName("Ontonotes499/" + filename)
    document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    if (loadPos) document.annotators(classOf[pos.PennPosLabel]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
    if (loadNer) if (nerBilou) document.annotators(classOf[ner.BilouOntonotesNerLabel]) = UnknownDocumentAnnotator.getClass else document.annotators(classOf[ner.BioOntonotesNerLabel]) = UnknownDocumentAnnotator.getClass
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
        assert(fields.length >= 10, "Fewer than 10 fields in file "+filename+"\nOffending line:\n"+line)
        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)
        val lemma = fields(2) // was 3
        val partOfSpeech = fields(4)
        val parentIdx = fields(8).toInt - 1
        val depLabel = fields(10)
        var ner = fields(13); if (ner == "_") ner = "O"  // If we wanted to distinguish "unnamed entities" from background, we wouldn't have this.
        document.appendString(" ")
        val token = new Token(sentence, word)
        if (loadPos) token.attr += new PennPosLabel(token, if (partOfSpeech == "XX") "PUNC" else partOfSpeech)
        if (loadNer) token.attr += (if (nerBilou) new BilouOntonotesNerLabel(token, ner) else new BioOntonotesNerLabel(token, ner))
        if (loadLemma) token.attr += new TokenLemma(token, lemma) // TODO Change this to some more specific TokenLemma subclass
        depInfoSeq.append((currTokenIdx, parentIdx, depLabel))
      }
    }
    if (sentence ne null) addDepInfo(sentence, depInfoSeq)
    if (nerBilou) convertBioBilou(document.asSection)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.asSection.length+" tokens total from file "+filename)
    Seq(document)
  }
  
  // TODO Use this method in method above, to avoid redundancy.
  def fromLines(lines:Iterator[String], filename:String = "?UNKNOWN?", loadLemma:Boolean = true, loadPos:Boolean = true, loadNer:Boolean = true, nerBilou:Boolean = false): Seq[Document] = {
    val document: Document = new Document()
    document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    if (loadPos) document.annotators(classOf[pos.PennPosLabel]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
    if (loadNer) if (nerBilou) document.annotators(classOf[ner.BilouOntonotesNerLabel]) = UnknownDocumentAnnotator.getClass else document.annotators(classOf[ner.BioOntonotesNerLabel]) = UnknownDocumentAnnotator.getClass
    var sentence: Sentence = new Sentence(document)
    var depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
    for (line <- lines) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addDepInfo(sentence, depInfoSeq)
        depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        sentence = null
      } else {
        if (sentence eq null)
          sentence = new Sentence(document) // avoids empty sentence at the end of doc
        val fields = line.split('\t')
        assert(fields.length >= 10, "Fewer than 10 fields in file "+filename+"\nOffending line:\n"+line)
        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)
        val lemma = fields(2) // was 3
        val partOfSpeech = fields(4)
        val parentIdx = fields(8).toInt - 1
        val depLabel = fields(10)
        var ner = fields(13); if (ner == "_") ner = "O"  // If we wanted to distinguish "unnamed entities" from background, we wouldn't have this.
        document.appendString(" ")
        val token = new Token(sentence, word)
        if (loadPos) token.attr += new PennPosLabel(token, if (partOfSpeech == "XX") "PUNC" else partOfSpeech)
        if (loadNer) token.attr += (if (nerBilou) new BilouOntonotesNerLabel(token, ner) else new BioOntonotesNerLabel(token, ner))
        if (loadLemma) token.attr += new TokenLemma(token, lemma) // TODO Change this to some more specific TokenLemma subclass
        depInfoSeq.append((currTokenIdx, parentIdx, depLabel))
      }
    }
    if (sentence ne null) addDepInfo(sentence, depInfoSeq)
    if (nerBilou) convertBioBilou(document.asSection)

    println("Loaded 1 document with "+document.sentences.size+" sentences with "+document.asSection.length+" tokens total from file "+filename)
    Seq(document)
  }
  
  def convertBioBilou(section:Section): Unit = {
    /** Return the string of the NER label, including the two letter (B- or I-) prefix. */
    def cat(token:Token): String = if (token eq null) "null" else token.attr[BilouOntonotesNerLabel].categoryValue
    /** Return true if the strings are equal without their two letter (B- or I-) prefix. */
    def sim(s1:String, s2:String): Boolean = s1.drop(2) == s2.drop(2)
    def isU(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && (!sim(cat2, cat3) || cat3(0) == 'B')
    def isB(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && sim(cat2, cat3) && cat3(0) == 'I'
    def isL(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && sim(cat1, cat2) && (cat3(0) == 'B' || !sim(cat2, cat3))
    def isI(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && cat3(0) == 'I'
    for (token <- section.tokens) if (token.attr[BilouOntonotesNerLabel].intValue != 0) {
      val nerLabel = token.attr[BilouOntonotesNerLabel]
      val cat1 = cat(token.prev); val cat2 = cat(token); val cat3 = cat(token.next)
      if (isU(cat1, cat2, cat3)) nerLabel.target.setCategory("U-"+cat2.drop(2))(null)
      else if (isB(cat1, cat2, cat3)) nerLabel.target.setCategory("B-"+cat2.drop(2))(null)
      else if (isL(cat1, cat2, cat3)) nerLabel.target.setCategory("L-"+cat2.drop(2))(null)
      else if (isI(cat1, cat2, cat3)) nerLabel.target.setCategory("I-"+cat2.drop(2))(null)
      nerLabel.setToTarget(null)
    }
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

  def main(args: Array[String]) =
    for (filename <- args)
      printDocument(fromFilename(filename).head)

}