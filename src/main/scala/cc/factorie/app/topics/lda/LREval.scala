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
package cc.factorie.app.topics.lda
import java.io.File

import cc.factorie.variable.{CategoricalSeqDomain, DiscreteDomain, DiscreteSeqDomain, DiscreteSeqVariable}

import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 *  Left-to-right evaluation algorithm described on page 65 in Wallach's PhD thesis.
 *
 *  @param wSeqDomain word domain
 *  @param zDomain topic domain
 *  @param alpha an array storing the alpha parameters of the Dirichlet prior on the document-topic distributions
 *  @param alphaSum the sum of the alpha parameters, i.e., the concentration parameter of the Dirichlet distribution
 *  @param beta parameter of the Dirichlet prior on the topic-word distributions; same for every word type
 *  @param betaSum the sum of the beta parameters
 *  @param topicCounts an array storing the count for every topic
 *  @param typeTopicCounts for every word type this data structure stores the number of times the word appears in each
 *         topic; only topics with non-zero counts should be specified.
 */
class LREval(val wSeqDomain: CategoricalSeqDomain[String], val zDomain: DiscreteDomain, val alpha: Array[Double], val alphaSum: Double,
                      val beta: Double, val betaSum: Double, val topicCounts: Array[Int], val typeTopicCounts: Array[HashMap[Int, Int]]){

  def numTopics = zDomain.size
  object zSeqDomain extends DiscreteSeqDomain { def elementDomain = zDomain }
  class Zs(intValues:Seq[Int]) extends DiscreteSeqVariable(intValues) {
    def this(len:Int) = this(Seq.fill(len)(0))
    def domain = zSeqDomain
  }

  /** Sets up the test documents and computes the information rate */
  def calcLR(testFile: String, numParticles: Int, useResampling: Boolean)(implicit random: scala.util.Random): Double = {
    doCalc(setUpDocs(testFile), numParticles, useResampling)
  }

  /** Creates documents from a file */
  def setUpDocs(testFile: String)(implicit random: scala.util.Random): ArrayBuffer[Document]= {

    val docs = new ArrayBuffer[Document]()
    val mySegmenter = new cc.factorie.app.strings.RegexSegmenter("\\p{Alpha}+".r)

    val source = scala.io.Source.fromFile(new File(testFile))
    var docInd = -1
    for(line <- source.getLines()){
      docInd += 1
      val doc = Document.fromString(wSeqDomain, "doc:"+docInd, line, segmenter = mySegmenter)
      doc.zs = new Zs(Array.tabulate(doc.ws.length)(i => random.nextInt(numTopics)))
      docs += doc
    }

    source.close()
    docs
  }

  /** Computes the information rate */
  def doCalc(testDocs: ArrayBuffer[Document], numParticles: Int, useResampling: Boolean)(implicit random: scala.util.Random): Double = {
    var heldoutLogLike = 0.0

    var docInd = 0
    var numTokens = 0
    val logNumParticles = math.log(numParticles)

    testDocs.foreach(doc => {
      docInd += 1
      println("Processing document " + docInd)

      val docTopicCounts: Array[Int] = new Array[Int](numTopics)
      val docSize = doc.zs.length
      numTokens += docSize

      for (position <- 0 until docSize){
        val wi = doc.ws.intValue(position)
        val currentTypeTopicCounts = typeTopicCounts(wi)
        var positionProb = 0.0

        for (particleInd <- 0 until numParticles){

          if (useResampling)
            for (positionPrime <- 0 until position)
              sampleAtPosition(positionPrime, doc, docTopicCounts, true, false)

          var tokenProb = 0.0
          for(zInd <- 0 until numTopics){
            tokenProb += (alpha(zInd) + docTopicCounts(zInd)) * (beta + currentTypeTopicCounts.getOrElse(zInd, 0)) /
              ((alphaSum + position) * (betaSum + topicCounts(zInd)))
          }
          positionProb += tokenProb
        }

        heldoutLogLike += math.log(positionProb) - logNumParticles
        sampleAtPosition(position, doc, docTopicCounts, false, false)
      }
    })

    -heldoutLogLike / numTokens
  }

  /** Samples a topic a assignment for a given position in a document. */
  def sampleAtPosition(position: Int, doc: Doc, docTopicCounts: Array[Int], decrDocCounts: Boolean, decrTopicCounts: Boolean)(implicit random: scala.util.Random) {

    val ti = doc.zs.intValue(position)
    val wi = doc.ws.intValue(position)
    val currentTypeTopicCounts: HashMap[Int,Int] = typeTopicCounts(wi)

    // Decrement doc counts
    if (decrDocCounts){
      docTopicCounts(ti) -= 1
    }

    // Decrement topic counts
    if (decrTopicCounts){
      topicCounts(ti) -=1

      val newCount: Int = currentTypeTopicCounts.getOrElse(ti, 0) - 1
      assert(newCount > 0, "Should have seen this topic before!")
      currentTypeTopicCounts += ti -> newCount
    }

    var totalMass = 0.0
    var zInd = 0
    val topicMassContrib = new Array[Double](numTopics)
    while(zInd < numTopics){

      val massContrib = (alpha(zInd) + docTopicCounts(zInd)) * (beta + currentTypeTopicCounts.getOrElse(zInd, 0)) / (betaSum + topicCounts(zInd))
      totalMass += massContrib
      topicMassContrib(zInd) = massContrib

      zInd += 1
    }

    // Sample a topic
    var sample = random.nextDouble() * totalMass
		var newTi = -1
		while (sample > 0.0) {
		  newTi += 1
		  sample -= topicMassContrib(newTi)
		}
    assert(newTi != -1, "Topic not sampled!")

    // Increment doc counts
    docTopicCounts(newTi) += 1

    if (decrTopicCounts){
      // Increment topic counts
      topicCounts(newTi) +=1

      val newCount: Int = currentTypeTopicCounts.getOrElse(newTi, 0) + 1
      currentTypeTopicCounts += newTi -> newCount
    }

    doc.zs.set(position, newTi)(null)
  }
}
