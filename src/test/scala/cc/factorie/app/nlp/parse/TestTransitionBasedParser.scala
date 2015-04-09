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
package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp.load._

import org.junit.Test
import org.junit.Before
import org.junit.Assert

class TestTransitionBasedParser {

  val testFileName = this.getClass.getResource("/parser-test-input").getPath()
  var parser: TransitionBasedParser = _

  @Before
  def setUp() = {
    parser = new TransitionBasedParser()
  }

  @Test
  def testDepToken() = {

    val numThreads = 1

    /* This file contains just one sentence right now */
    val testDoc = LoadOntonotes5.fromFilename(testFileName).head
    val testSentences = testDoc.sentences

    val parseDecisions = parser.generateDecisions(testSentences, ParserConstants.TRAINING, numThreads)

    /* Check that the relations between tokens are correct */
    parseDecisions.map(_.last).zip(testSentences).foreach(ds => {
      val parseTree = ds._2.attr[ParseTree]
      println(s"Sentence: ${ds._2.tokens.map(_.string).mkString(" ")}")
      val state = ds._1.state
      val sentence = state.sentence
      val tokens = state.sentence._tokens
      val heads = state.headIndices
      val labels = state.arcLabels

      tokens.zip(1 until tokens.length).foreach { case (tok, idx) => {
        val parseTreeIdx = idx - 1
        val thisHead = if (heads(idx) != -1) sentence(heads(idx)) else null
        val trueHead = parseTree.parent(parseTreeIdx)
        if (trueHead == null || thisHead == null) {
          // if one has no head then neither should, and this should be the root
          if (thisHead != null) {
            Assert.assertEquals(s"Head of token ``${tok.string}'' incorrect.", ParserConstants.ROOT_STRING, thisHead.string)
            Assert.assertEquals(s"Label of token ``${tok.string}'' incorrect.", "root", labels(idx))
          } else {
            Assert.assertNotNull(s"Head of token ``${tok.string}'' incorrect.", thisHead)
          }
        } else {
          // should be the same word
          Assert.assertEquals(s"Head of token ``${tok.string}'' incorrect.", trueHead.string, thisHead.string)

          // labels should be the same
          Assert.assertEquals(s"Label of token ``${tok.string}'' incorrect.", parseTree.label(parseTreeIdx).categoryValue, labels(idx))

          // leftmost dependents should be the same
          val thisLeftmostDep = sentence(state.leftmostDependent(idx))
          val trueLeftmostDep = if (!parseTree.leftChildren(parseTreeIdx).isEmpty) parseTree.leftChildren(parseTreeIdx).head else null
          if (thisLeftmostDep == null || trueLeftmostDep == null) {
            // if one is null then they both should be
            if (thisLeftmostDep != null)
              Assert.assertEquals(s"Leftmost dependency of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisLeftmostDep.string)
            else
              Assert.assertNotNull(s"Leftmost dependency of token ``${tok.string}'' incorrect.", thisLeftmostDep)
          } else {
            // should be the same word
            Assert.assertEquals(s"Leftmost dependency of token ``${tok.string}'' incorrect.", trueLeftmostDep.string, thisLeftmostDep.string)

            // 2nd leftmost dependents should be the same
            val thisLeftmostDep2 = sentence(state.leftmostDependent2(idx))
            val trueLeftmostDep2 = if (!trueLeftmostDep.parseLeftChildren.isEmpty) trueLeftmostDep.parseLeftChildren.head else null
            if (thisLeftmostDep2 == null || trueLeftmostDep2 == null) {
              // if one is null then they both should be
              if (thisLeftmostDep != null)
                Assert.assertEquals(s"2nd leftmost dependency of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisLeftmostDep2.string)
              else
                Assert.assertNotNull(s"2nd leftmost dependency of token ``${tok.string}'' incorrect.", thisLeftmostDep2)
            } else {
              // should be same word
              Assert.assertEquals(s"2nd leftmost dependency of token ``${tok.string}'' incorrect.", trueLeftmostDep2.string, thisLeftmostDep2.string)
            }
          }

          // rightmost dependents should be the same
          val thisRightmostDep = sentence(state.rightmostDependent(idx))
          val trueRightmostDep = if (!parseTree.rightChildren(parseTreeIdx).isEmpty) parseTree.rightChildren(parseTreeIdx).last else null

          if (thisRightmostDep == null || trueRightmostDep == null) {
            // if one is null then they both should be
            if (thisRightmostDep != null)
              Assert.assertEquals(s"Rightmost dependency of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisRightmostDep.string)
            else
              Assert.assertNotNull(s"Rightmost dependency of token ``${tok.string}'' incorrect.", thisRightmostDep)
          } else {
            // should be the same word
            Assert.assertEquals(s"Rightmost dependency of token ``${tok.string}'' incorrect.", trueRightmostDep.string, thisRightmostDep.string)

            // 2nd leftmost dependents should be the same
            val thisRightmostDep2 = sentence(state.rightmostDependent2(idx))
            val trueRightmostDep2 = if (!trueRightmostDep.parseRightChildren.isEmpty) trueRightmostDep.parseRightChildren.last else null
            if (thisRightmostDep2 == null || trueRightmostDep2 == null) {
              // if one is null then they both should be
              if (thisRightmostDep2 != null)
                Assert.assertEquals(s"2nd rightmost dependency of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisRightmostDep2.string)
              else
                Assert.assertNotNull(s"2nd rightmost dependency of token ``${tok.string}'' incorrect.", thisRightmostDep2)
            } else {
              // should be same word
              Assert.assertEquals(s"2nd rightmost dependency of token ``${tok.string}'' incorrect.", trueRightmostDep2.string, thisRightmostDep2.string)
            }
          }

          // left-nearest siblings should be the same
          val thisLeftNearestSib = sentence(state.leftNearestSibling(idx))
          val trueParentIdx = parseTree.sentence(parseTreeIdx).parseParentIndex
          val trueLeftNearestSib = {
            var i = parseTreeIdx - 1
            while (i >= 0 && parseTree.sentence(i).parseParentIndex != trueParentIdx) i -= 1
            if (i == -1) null else parseTree.sentence(i)
          }

          if (trueLeftNearestSib == null || thisLeftNearestSib == null) {
            // if one is null then they both should be
            if (thisLeftNearestSib != null)
              Assert.assertEquals(s"Left nearest sibling of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisLeftNearestSib.string)
            else
              Assert.assertNotNull(s"Left nearest sibling of token ``${tok.string}'' incorrect.", thisLeftNearestSib)
          } else {
            // should be same word
            Assert.assertEquals(s"Left nearest sibling of token ``${tok.string}'' incorrect.", trueLeftNearestSib.string, thisLeftNearestSib.string)
          }

          // right-nearest siblings should be the same
          val thisRightNearestSib = sentence(state.rightNearestSibling(idx))
          val trueRightNearestSib = {
            var i = parseTreeIdx + 1
            while (i < parseTree.sentence.size && parseTree.sentence(i).parseParentIndex != trueParentIdx) i += 1
            if (i == parseTree.sentence.size) null else parseTree.sentence(i)
          }

          if (trueRightNearestSib == null || thisRightNearestSib == null) {
            // if one is null then they both should be
            if (thisRightNearestSib != null)
              Assert.assertEquals(s"Right nearest sibling of token ``${tok.string}'' incorrect.", ParserConstants.NULL_STRING, thisRightNearestSib.string)
            else
              Assert.assertNotNull(s"Right nearest sibling of token ``${tok.string}'' incorrect.", thisRightNearestSib)
          } else {
            // should be same word
            Assert.assertEquals(s"Right nearest sibling of token ``${tok.string}'' incorrect.", trueRightNearestSib.string, thisRightNearestSib.string)
          }
        }
      }}
    })
    /* Print out the features for the first sentence */
    parseDecisions.head.foreach(decision => {
      print(s"${
        // convert decision to a nice verbose string (rather than ints)
        val transition = decision.categoryValue.split(" ")
        transition.take(2).map(x => ParserConstants(x.toInt)).mkString(" ") + " " + transition(2)
      }; ")
      println(s"feats: ${decision.features.activeCategories.mkString(", ")}")//domain.dimensionDomain.categories.zip(decision.features.value.toSeq).filter(_._2 == 1.0).map(_._1).mkString(" ")}")
      println()
    })
  }
}