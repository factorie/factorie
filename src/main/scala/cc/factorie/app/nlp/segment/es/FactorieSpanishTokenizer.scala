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
package cc.factorie.app.nlp.segment.es

import java.io.{StringReader, FileReader, BufferedReader}

import cc.factorie.app.nlp.{Document, DocumentAnnotator, Token}

/** Split a String into a sequence of Tokens.  Aims to adhere to tokenization rules used in Ontonotes and Penn Treebank.
    Note that CoNLL tokenization would use tokenizeAllDashedWords=true.
    Punctuation that ends a sentence should be placed alone in its own Token, hence this segmentation implicitly defines sentence segmentation also.
    (Although our the DeterministicSentenceSegmenter does make a few adjustments beyond this tokenizer.)
    @author Andrew McCallum
  */
class FactorieSpanishTokenizer(caseSensitive:Boolean = false, tokenizeSgml:Boolean = false, tokenizeNewline:Boolean = false, tokenizeAllDashedWords:Boolean = false, abbrevPreceedsLowercase:Boolean = false) extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString

  def process(document: Document): Document = {
    for (section <- document.sections) {
      val lexer = new SpanishLexer(new StringReader(section.string))
      var next = lexer.next().asInstanceOf[Array[Int]]
      while (next != null){
        new Token(section, next(0), next(0) + next(1))
        next = lexer.next().asInstanceOf[Array[Int]]


        //        if (abbrevPreceedsLowercase && prevTokenPeriod && java.lang.Character.isLowerCase(string(0)) && section.length > 1 && section.tokens(section.length-2).stringEnd == section.tokens(section.length-1).stringStart) {
//          // If we have a pattern like "Abbrev. has" (where "has" is any lowercase word) with token strings "Abbrev", ".", "is" (currently looking at "is")
//          // then assume that the previous-previous word is actually an abbreviation; patch it up to become "Abbrev.", "has".
//          val lastTwoTokens = section.takeRight(2).toIndexedSeq
//          section.remove(section.length-1); section.remove(section.length-1)
//          new Token(section, lastTwoTokens(0).stringStart, lastTwoTokens(1).stringEnd)
//          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
//        } else if (tokenizeNewline && string == "\n") {
//          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
//        } else if (tokenizeSgml ||
//          !((string(0) == '<' && string(string.length-1) == '>') // We have an SGML tag
//            || (string(0) == '&' && string(string.length-1) == ';') // We have an odd escaped SGML tag &gt;...&lt;
//            || string.toLowerCase == "&nbsp;" // Don't make token from space
//            )
//        ) {
//          new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
//        }
//        if (string == ".") prevTokenPeriod = true else prevTokenPeriod = false
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

object FactorieSpanishTokenizer extends FactorieSpanishTokenizer(false, false, false, false, false) {
  def main(args: Array[String]): Unit = {
    val string = io.Source.fromFile("test.file").mkString
    val doc = new Document(string)
    FactorieSpanishTokenizer.process(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}
