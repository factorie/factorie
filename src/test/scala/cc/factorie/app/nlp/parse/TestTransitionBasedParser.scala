package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp.load._

import org.junit.Test
import org.junit.Before
import org.junit.Assert

class TestTransitionBasedParser {

  val nullTokenString = "<NULL>"
  val rootTokenString = "<ROOT>"
  
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
	
	val parseDecisions = testSentences.map(s => parser.generateDecisions(Seq(s), parser.ParserConstants.TRAINING, numThreads))

    /* Check that the relations between tokens are correct */
    parseDecisions.map(_.last).zip(testSentences).foreach(ds => {
      val parseTree = ds._2.attr[ParseTree]
      println(s"Sentence: ${ds._2.tokens.map(_.string).mkString(" ")}")
      val tokens = ds._1.state.sentenceTokens
      tokens.takeRight(tokens.length - 2).foreach(tok => {
        val tokTreeIdx = tok.thisIdx - 1
        val thisHead = if (tok.hasHead) tok.head.depToken else null
        val trueHead = parseTree.parent(tokTreeIdx)
        if (trueHead == null || thisHead == null) {
          // if one has no head then neither should, and this should be the root
          if (thisHead != null) {
            Assert.assertEquals(s"Head of token ``${tok.form}'' incorrect.", rootTokenString, thisHead.form)
            Assert.assertEquals(s"Label of token ``${tok.form}'' incorrect.", "root", tok.head.label)
          } else {
            Assert.assertNotNull(s"Head of token ``${tok.form}'' incorrect.", thisHead)
          }
        } else {
          // should be the same word
          Assert.assertEquals(s"Head of token ``${tok.form}'' incorrect.", trueHead.string, thisHead.form)

          // labels should be the same
          Assert.assertEquals(s"Label of token ``${tok.form}'' incorrect.", parseTree.label(tokTreeIdx).categoryValue, tok.head.label)

          // grandheads should be the same
          val thisGrandHead = if (tok.hasGrandHead) tok.grandHead.depToken else null
          val trueGrandHead = parseTree.parent(trueHead.positionInSentence)
          if (trueGrandHead == null || thisGrandHead == null) {
            // if one has no head then neither should, and this should be the root
            if (thisGrandHead != null) {
              Assert.assertEquals(s"Grandhead of token ``${tok.form}'' incorrect.", rootTokenString, thisGrandHead.form)
              Assert.assertEquals(s"Label of grandhead of ``${tok.form}'' incorrect.", "root", thisHead.head.label)
            } else {
              Assert.assertNotNull(s"Grandhead of token ``${tok.form}'' incorrect.", thisGrandHead)
            }
          } else {
            // should be the same word
            Assert.assertEquals(s"Grandhead of token ``${tok.form}'' incorrect.", trueGrandHead.string, thisGrandHead.form)

            // labels should be the same
            Assert.assertEquals(s"Label of grandhead of ``${tok.form}'' incorrect.", parseTree.label(trueHead.positionInSentence).categoryValue, thisHead.head.label)
          }

          // leftmost dependents should be the same
          val thisLeftmostDep = tok.leftmostDependent
          val trueLeftmostDep = if (!parseTree.leftChildren(tokTreeIdx).isEmpty) parseTree.leftChildren(tokTreeIdx).head else null
          if (thisLeftmostDep == null || trueLeftmostDep == null) {
            // if one is null then they both should be
            if (thisLeftmostDep != null)
              Assert.assertEquals(s"Leftmost dependency of token ``${tok.form}'' incorrect.", nullTokenString, thisLeftmostDep.form)
            else
              Assert.assertNotNull(s"Leftmost dependency of token ``${tok.form}'' incorrect.", thisLeftmostDep)
          } else {
            // should be the same word
            Assert.assertEquals(s"Leftmost dependency of token ``${tok.form}'' incorrect.", trueLeftmostDep.string, thisLeftmostDep.form)

            // 2nd leftmost dependents should be the same
            val thisLeftmostDep2 = tok.leftmostDependent2
            val trueLeftmostDep2 = if (!trueLeftmostDep.parseLeftChildren.isEmpty) trueLeftmostDep.parseLeftChildren.head else null
            if (thisLeftmostDep2 == null || trueLeftmostDep2 == null) {
              // if one is null then they both should be
              if (thisLeftmostDep != null)
                Assert.assertEquals(s"2nd leftmost dependency of token ``${tok.form}'' incorrect.", nullTokenString, thisLeftmostDep2.form)
              else
                Assert.assertNotNull(s"2nd leftmost dependency of token ``${tok.form}'' incorrect.", thisLeftmostDep2)
            } else {
              // should be same word
              Assert.assertEquals(s"2nd leftmost dependency of token ``${tok.form}'' incorrect.", trueLeftmostDep2.string, thisLeftmostDep2.form)
            }
          }

          // rightmost dependents should be the same
          val thisRightmostDep = tok.rightmostDependent
          val trueRightmostDep = if (!parseTree.rightChildren(tokTreeIdx).isEmpty) parseTree.rightChildren(tokTreeIdx).last else null

          if (thisRightmostDep == null || trueRightmostDep == null) {
            // if one is null then they both should be
            if (thisRightmostDep != null)
              Assert.assertEquals(s"Rightmost dependency of token ``${tok.form}'' incorrect.", nullTokenString, thisRightmostDep.form)
            else
              Assert.assertNotNull(s"Rightmost dependency of token ``${tok.form}'' incorrect.", thisRightmostDep)
          } else {
            // should be the same word
            Assert.assertEquals(s"Rightmost dependency of token ``${tok.form}'' incorrect.", trueRightmostDep.string, thisRightmostDep.form)

            // 2nd leftmost dependents should be the same
            val thisRightmostDep2 = tok.rightmostDependent2
            val trueRightmostDep2 = if (!trueRightmostDep.parseRightChildren.isEmpty) trueRightmostDep.parseRightChildren.last else null
            if (thisRightmostDep2 == null || trueRightmostDep2 == null) {
              // if one is null then they both should be
              if (thisRightmostDep2 != null)
                Assert.assertEquals(s"2nd rightmost dependency of token ``${tok.form}'' incorrect.", nullTokenString, thisRightmostDep2.form)
              else
                Assert.assertNotNull(s"2nd rightmost dependency of token ``${tok.form}'' incorrect.", thisRightmostDep2)
            } else {
              // should be same word
              Assert.assertEquals(s"2nd rightmost dependency of token ``${tok.form}'' incorrect.", trueRightmostDep2.string, thisRightmostDep2.form)
            }
          }

          // left-nearest siblings should be the same
          val thisLeftNearestSib = tok.leftNearestSibling
          val trueParentIdx = parseTree.sentence(tokTreeIdx).parseParentIndex
          val trueLeftNearestSib = {
            var i = tokTreeIdx - 1
            while (i >= 0 && parseTree.sentence(i).parseParentIndex != trueParentIdx) i -= 1
            if (i == -1) null else parseTree.sentence(i)
          }

          if (trueLeftNearestSib == null || thisLeftNearestSib == null) {
            // if one is null then they both should be
            if (thisLeftNearestSib != null)
              Assert.assertEquals(s"Left nearest sibling of token ``${tok.form}'' incorrect.", nullTokenString, thisLeftNearestSib.form)
            else
              Assert.assertNotNull(s"Left nearest sibling of token ``${tok.form}'' incorrect.", thisLeftNearestSib)
          } else {
            // should be same word
            Assert.assertEquals(s"Left nearest sibling of token ``${tok.form}'' incorrect.", trueLeftNearestSib.string, thisLeftNearestSib.form)
          }

          // right-nearest siblings should be the same
          val thisRightNearestSib = tok.rightNearestSibling
          val trueRightNearestSib = {
            var i = tokTreeIdx + 1
            while (i < parseTree.sentence.size && parseTree.sentence(i).parseParentIndex != trueParentIdx) i += 1
            if (i == parseTree.sentence.size) null else parseTree.sentence(i)
          }

          if (trueRightNearestSib == null || thisRightNearestSib == null) {
            // if one is null then they both should be
            if (thisRightNearestSib != null)
              Assert.assertEquals(s"Right nearest sibling of token ``${tok.form}'' incorrect.", nullTokenString, thisRightNearestSib.form)
            else
              Assert.assertNotNull(s"Right nearest sibling of token ``${tok.form}'' incorrect.", thisRightNearestSib)
          } else {
            // should be same word
            Assert.assertEquals(s"Right nearest sibling of token ``${tok.form}'' incorrect.", trueRightNearestSib.string, thisRightNearestSib.form)
          }
        }
      })

    })
    /* Print out the features for the first sentence */
    parseDecisions.head.foreach(decision => {
      print(s"${ // convert decision to a nice verbose string (rather than ints)
        val transition = decision.categoryValue.split(" ")
        transition.take(2).map(x => parser.ParserConstants.getString(x.toInt)).mkString(" ") + " " + transition(2)
      }; ")
      println(s"feats: ${decision.features.domain.dimensionDomain.categories.zip(decision.features.value.toSeq).filter(_._2 == 1.0).map(_._1).mkString(" ")}")
      println()
    })
  }
}