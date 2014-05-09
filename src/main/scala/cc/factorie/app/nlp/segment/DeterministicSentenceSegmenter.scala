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

import cc.factorie.app.nlp._

/** Segments a sequence of tokens into sentences.
    @author Andrew McCallum */
class DeterministicSentenceSegmenter extends DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = null
  
  /** If true, every newline causes a sentence break. */
  var newlineBoundary = false

  /** If true,, every double newline causes a sentence break. */
  var doubleNewlineBoundary = true

  /** Matches the Token.string of punctuation that always indicates the end of a sentence.  It does not include possible additional tokens that may be appended to the sentence such as quotes and closing parentheses. */
  val closingRegex = "\\A([!\\?]+|[\\.;])\\Z".r // We allow repeated "!" and "?" to end a sentence, but not repeated "."
  
  /** Matches the Token.string of tokens that may extend a sentence, such as quotes, closing parentheses, and even additional periods. */
  val closingContinuationRegex = "^''|[\\.\"!\\?\\p{Pf}\\p{Pe}]+$".r
  
  /** Matches the Token.string of tokens that might possibility indicate the end of a sentence, such as an mdash.
      The sentence segmenter will only actually create a sentence end here if possibleSentenceStart is true for the following token. */
  val possibleClosingRegex = "^\\.\\.+|[-\\p{Pd}\u2014]+$".r
  
  /** Whitespace that should not be allowed between a closingRegex and closingContinuationRegex for a sentence continuation.  For example:  He ran.  "You shouldn't run!" */
  val spaceRegex = "[ \n\r\t\u00A0\\p{Z}]+".r
  
  val emoticonRegex = ("\\A("+DeterministicTokenizer.emoticon+")\\Z").r
  
  /** If there are more than this number of characters between the end of the previous token and the beginning of this one, force a sentence start.
      If negative, don't break sentences according to this criteria at all. */
  val charOffsetBoundary = 10
  
  /** Returns true for strings that probably start a sentence after a word that ends with a period. */
  def possibleSentenceStart(s:String): Boolean = java.lang.Character.isUpperCase(s(0)) && (cc.factorie.app.nlp.lexicon.StopWords.containsWord(s) || s == "Mr." || s == "Mrs." || s == "Ms." || s == "\"" || s == "''") // Consider adding more honorifics and others here. -akm
  
  
  def process(document: Document): Document = {
    def safeDocSubstring(start:Int, end:Int): String = if (start < 0 || end > document.stringLength) "" else document.string.substring(start, end)
    def safeDocChar(i:Int): Char = if (i < 0 || i >= document.stringLength) '\u0000' else document.string(i)
    //println("SentenceSegmenter1 possibleClosingRegex "+possibleClosingRegex.findPrefixMatchOf("\u2014"))
    for (section <- document.sections) {
      val tokens = section.tokens
      var i = 0
      var sentenceStart = 0
      // Create a new Sentence, register it with the section, and update sentenceStart.  Here sentenceEnd is non-inclusive
      def newSentence(sentenceEnd:Int): Unit = {
         if(sentenceStart != sentenceEnd) new Sentence(section, sentenceStart, sentenceEnd - sentenceStart); sentenceStart = sentenceEnd
      }
      while (i < tokens.length) {
        val token = tokens(i)
        val string = tokens(i).string
        //println("SentenceSegmenter1 first char: "+Integer.toHexString(string(0))+" possibleClosingRegex "+possibleClosingRegex.findPrefixMatchOf(string))
        // Sentence boundary from a single newline
        if (newlineBoundary && i > sentenceStart && i > 0 && document.string.substring(tokens(i-1).stringEnd, token.stringStart).contains('\n')) {
          newSentence(i)
        }
        // Sentence boundary from a double newline
        else if (doubleNewlineBoundary && i > sentenceStart && i > 0 && document.string.substring(tokens(i-1).stringEnd, token.stringStart).contains("\n\n")) {
          //println("SentenceSegmenter1 i="+i+" doubleNewline")
          newSentence(i)
        }
        // Emoticons are single-token sentences
        else if (emoticonRegex.findFirstMatchIn(string) != None) {
          if (i > 0) newSentence(i)
          newSentence(i+1)
        }
        // Sentence boundary from sentence-terminating punctuation
        else if (closingRegex.findFirstMatchIn(string) != None) {
          //println("SentenceSegmenter1 i="+i+" starting end with "+string)
          while (i+1 < tokens.length && spaceRegex.findFirstMatchIn(document.string.substring(token.stringEnd, tokens(i+1).stringStart)) == None && closingContinuationRegex.findPrefixMatchOf(tokens(i+1).string) != None) i += 1 // look for " or ) after the sentence-ending punctuation
          //println("SentenceSegmenter1 i="+i+" ending with "+tokens(i).string)
          newSentence(i+1)
        // Possible sentence boundary from a word that ends in '.'  For example:  "He left the U.S.  Then he came back again."
        } else if (string(string.length-1) == '.') { // token ends in ., might also be end of sentence
          if (i+1 < tokens.length && possibleSentenceStart(tokens(i+1).string)) { // If the next word is a capitalized stopword, then say it is a sentence
            //println("SentenceSegmenter1 i="+i+" possibleSentenceStart "+tokens(i+1).string)
            val t2 = new Token(token.stringEnd-1, token.stringEnd)
            section.insert(i+1, t2) // Insert a token containing just the last (punctuation) character
            i += 1
            newSentence(i+1)
            t2._sentence = section.sentences.last // need to have this down here in case this was the first sentence
          }
        // Possible sentence boundary from the dash in something like LONDON - Today Prime Minister... 
        } else if (possibleClosingRegex.findPrefixMatchOf(string) != None) {
          //println("SentenceSegmenter1 found dash: "+string)
          if (i+1 < tokens.length && possibleSentenceStart(tokens(i+1).string)) {
            //println("SentenceSegmenter1 i="+i+" possibleClosingRegex "+tokens(i+1).string)
            newSentence(i+1)
          }
        } else if (charOffsetBoundary > 0 && i > sentenceStart && token.hasPrev && token.stringStart - token.prev.stringEnd > charOffsetBoundary && document.string.substring(token.prev.stringEnd, token.prev.stringEnd+3).trim.toLowerCase != "<a") {
            // TODO The main way this can break a sentence incorrectly is when there is a long "<a href...>" tag.
          newSentence(i)          
        }
        i += 1
      }
      if (sentenceStart < tokens.length) newSentence(tokens.length) // Final sentence
      for (sentence <- section.sentences; token <- sentence.tokens) token._sentence = sentence  // Set each Token's internal record of its sentence, to avoid having to look it up later. 
    }
    document
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(classOf[Sentence])
}

object DeterministicSentenceSegmenter extends DeterministicSentenceSegmenter {
  def main(args: Array[String]): Unit = {
    for (filename <- args) yield {
      val doc = new Document(io.Source.fromFile(filename).mkString).setName(filename)
      DeterministicTokenizer.process(doc)
      DeterministicSentenceSegmenter.this.process(doc)
      println(filename)
      for (sentence <- doc.sentences)
        print("\n\n" + sentence.tokens.map(_.string).mkString(" | "))
      print("\n\n\n")
    }
  }
}
