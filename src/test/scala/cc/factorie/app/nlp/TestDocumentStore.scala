/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp

import cc.factorie.app.nlp.parse._
import cc.factorie.app.nlp.pos.{PennPosDomain, PennPosTag}
import org.scalatest._

/** Test serialization of Document to BSON.
    @author John Sullivan, Andrew McCallum
 */
class TestDocumentStore extends FlatSpec with Matchers {

  def fix = new {
    val doc1 = new Document("If it's your job to eat a frog, it's best to do it first thing in the morning. And If it's your job to eat two frogs, it's best to eat the biggest one first.")
    DocumentAnnotatorPipeline(segment.DeterministicNormalizingTokenizer, segment.DeterministicSentenceSegmenter).process(doc1)
    for (token <- doc1.tokens) token.attr += new PennPosTag(token, token.positionInSentence % PennPosDomain.size)
    for (sentence <- doc1.sentences) sentence.attr += new ParseTree(sentence, Range(0, sentence.length).toArray, Range(0, sentence.length).map(_ % ParseTreeLabelDomain.length).toArray)
    doc1.annotators(classOf[PennPosTag]) = this.getClass
    doc1.annotators(classOf[ParseTree]) = this.getClass
  }

  "DocumentCubbie" should "serialize and deserialize properly" in {
    val f = fix
    import f._

    val cubbie = new StandardDocumentCubbie() := doc1
    val doc2 = cubbie.document
    
    assert(doc1.tokens.toSeq.map(_.string) == doc2.tokens.toSeq.map(_.string))
    assert(doc1.tokens.toSeq.map(_.posTag.categoryValue) == doc2.tokens.toSeq.map(_.posTag.categoryValue))
  }
/*
  it should "preserve document annotation metadata" in {
    val f = fix
    import f._

    val cubbie = new StandardDocumentCubbie() := doc1
    val doc2 = cubbie.document

    assert(doc1.annotators.keySet == doc2.annotators.keySet)

  }
*/
}
