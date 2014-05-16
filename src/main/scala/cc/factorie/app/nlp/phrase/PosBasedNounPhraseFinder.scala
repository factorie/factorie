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
package cc.factorie.app.nlp.phrase
import cc.factorie.app.nlp._
import scala.collection.mutable.ListBuffer

/** Find noun phrases merely by contiguous nouns (possibly prefixed by adjectives) and pronouns.
    This is simple but much less accurate than ChainChunker.
    @author Andrew McCallum */
object PosBasedNounPhraseFinder extends DocumentAnnotator {
  def process(document:Document): Document = {
    document.attr += new NounPhraseList(phrases(document))
    document
  }
  def phrases(document:Document): Seq[Phrase] = {
    val phrases = new ListBuffer[Phrase]()
    var tempSpan: Phrase = null
    for (section <- document.sections; token <- section.tokens) {
      // Put a span around contiguous sequences of NN or PR part-of-speech prefixes
      val posPrefix = token.attr[pos.PennPosTag].categoryValue.take(2)
      if (posPrefix == "NN" || posPrefix == "PR" || (posPrefix == "JJ" && token.hasNext && token.next.attr[pos.PennPosTag].categoryValue.take(2) == "NN")) {
        if (tempSpan eq null) tempSpan = new Phrase(section, token.position, 1,offsetToHeadToken = -1)
        else tempSpan.append(1)(null)
      } else if (tempSpan ne null) {
        if (token.string == "-" && token.hasNext && token.next.attr[pos.PennPosTag].categoryValue.take(2) == "NN") tempSpan.append(1)(null) // Handle dashed nouns
        else { phrases += tempSpan; tempSpan = null}
      }
    }
    phrases
  }
  override def tokenAnnotationString(token:Token): String = {
    val phrases = token.document.attr[NounPhraseList].spansContaining(token)
    if (phrases.isEmpty) return null
    phrases.map(c => if (c.head == token) "B-NP" else "I-NP").mkString(",")
  }
  def prereqAttrs: Iterable[Class[_]] = List(classOf[pos.PennPosTag])
  def postAttrs: Iterable[Class[_]] = List(classOf[NounPhraseList])
}
