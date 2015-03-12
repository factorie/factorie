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
package cc.factorie.app.nlp.segment

import java.io.StringReader

import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token}

/** Split a String into a sequence of Tokens.  Aims to adhere to tokenization rules used in Ontonotes and Penn Treebank.
    Note that CoNLL tokenization would use tokenizeAllDashedWords=true.
    Punctuation that ends a sentence should be placed alone in its own Token, hence this segmentation implicitly defines sentence segmentation also.
    (Although our the DeterministicSentenceSegmenter does make a few adjustments beyond this tokenizer.)
    @author Andrew McCallum
  */
class DeterministicLexerTokenizer(caseSensitive:Boolean = false, tokenizeSgml:Boolean = false, tokenizeNewline:Boolean = false, tokenizeAllDashedWords:Boolean = false, abbrevPreceedsLowercase:Boolean = false) extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString

  def process(document: Document): Document = {
    for (section <- document.sections) {
      val lexer = new EnglishLexer(new StringReader(section.string + "\n"))
      var next = lexer.next().asInstanceOf[Array[Int]]
      while (next != null){
        val tok = new Token(section, next(0), next(0) + next(1))
        println(tok.string)
        next = lexer.next().asInstanceOf[Array[Int]]
      }
    }
    if (!document.annotators.contains(classOf[Token]))
      document.annotators(classOf[Token]) = this.getClass
    document
  }

  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = List(classOf[Token])

  /** Convenience function to run the tokenizer on an arbitrary String.  The implementation builds a Document internally, then maps to token strings. */
  def apply(s:String): Seq[String] = process(new Document(s)).tokens.toSeq.map(_.string)
}

object DeterministicLexerTokenizer extends DeterministicLexerTokenizer(false, false, false, false, false) {
  def main(args: Array[String]): Unit = {
//    val fname = "/iesl/canvas/strubell/data/tackbp/source/2013/LDC2013E45_TAC_2013_KBP_Source_Corpus_disc_2/data/English/discussion_forums/bolt-eng-DF-200"
    val fname = "/iesl/canvas/strubell/arabic.txt"
//    val fname = "/Users/strubell/Documents/research/arabic.txt"
    println(s"Loading $fname")
    val string = io.Source.fromFile(fname, "utf-8").mkString
//    val string = "A.  A.A.A.I.  and U.S. in U.S.. etc., but not A... or A..B iPhone 3G in Washington D.C...."
//    val string = "Washington D.C.... A..B!!C??D.!?E.!?.!?F..!!?? U.S.." // want: [A, .., B, !!, C, ??, D, .!?, E, .!?.!?, F, ..!!??]
//    val string = "AT&T but don't grab LAT&Eacute; and be sure not to grab PE&gym AT&T"
//    val string = "2012-04-05"
//    val string = "poop-centric"
//    val string = "he'll go to hell we're"
//    val string = "I paid $50 USD"
//    val string =  "$1 E2 L3 USD1 2KPW ||$1 USD1.." // want: "[$, 1, E2, L3, USD, 1, 2, KPW, |, |, $, 1, USD, 1, ..]"
//    val string = " 1. Buy a new Chevrolet (37%-owned in the U.S..) . 15%"
//    val string = "blah blah Abbrev. has blah"

    println("Tokenizing...")
    val doc = new Document(string)
    val t0 = System.currentTimeMillis()
    DeterministicLexerTokenizer.process(doc)
    val time = System.currentTimeMillis()-t0
    println(s"Processed ${doc.tokenCount} tokens in ${time}ms (${doc.tokenCount.toDouble/time*1000} tokens/second)")
//    println(string)
//    println(doc.tokens.map(_.string).mkString("\n"))
  }
}
