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
import cc.factorie.directed.{DirectedModel, DiscreteMixtureCounts, PlatedCategoricalMixture}
import cc.factorie.util.DoubleSeq
import cc.factorie.variable.{CategoricalDomain, DiscreteDomain, DiscreteSeqVariable, ProportionsVar}

class SparseLDAInferencer(
    val zDomain:DiscreteDomain,
    val wordDomain:CategoricalDomain[String],
    var phiCounts:DiscreteMixtureCounts[String],
    initialAlphas:DoubleSeq,
    initialBeta1:Double,
    model:DirectedModel,
    random: scala.util.Random,
    val localPhiCounts: DiscreteMixtureCounts[String] = null)
{
  var verbosity = 0
  var smoothingOnlyCount = 0; var topicBetaCount = 0; var topicTermCount = 0 // Just diagnostics
  def samplesCount = smoothingOnlyCount + topicBetaCount + topicTermCount
  private var alphas: Array[Double] = null
  private var beta1: Double = 0.01
  private var betaSum = beta1 * wordDomain.size
  var smoothingMass: Double = 0.0
  val numTopics = zDomain.size
  private val cachedCoefficients = new Array[Double](numTopics)
  private val cachedSmoothing = new Array[Double](numTopics)
  private val cachedDenominators = new Array[Double](numTopics)

  private val topicTermScores = new Array[Double](numTopics)

  if (verbosity > 0) println("Finished initializing phiCounts")
  if (verbosity > 5) println("nt "+phiCounts.mixtureCounts.mkString(" "))

  private var _topicDocCounts: Array[Array[Int]] = null
  def topicDocCounts = _topicDocCounts
  // Initialize alphas, beta1, smoothingMass and cachedCoefficients; must be done after phiCounts initialized
  resetSmoothing(initialAlphas, initialBeta1)

  def resetCached():Unit = {
    smoothingMass = 0

    var t = 0
    while(t < numTopics){
      cachedDenominators(t) = 1.0 / (phiCounts.mixtureCounts(t) + betaSum)
      val topicSmoothing = alphas(t) * cachedDenominators(t)
      cachedCoefficients(t) = topicSmoothing
      cachedSmoothing(t) = topicSmoothing
      smoothingMass += topicSmoothing
      t += 1
    }
    smoothingMass *= beta1
  }

  def resetSmoothing(newAlphas:DoubleSeq, newBeta1:Double): Unit = {
    require(numTopics == newAlphas.length)
    require(newBeta1 > 0.0)
    require(newAlphas.forall(_ >= 0.0))
    alphas = newAlphas.toArray
    beta1 = newBeta1
    betaSum = beta1 * wordDomain.size
    //smoothingMass = recalcSmoothingMass
    // The first term in the per-topic summand for q, just missing *n_{w|t}  [Mimno "Sparse LDA"]

    resetCached()
    assert(smoothingMass > 0.0, smoothingMass)
  }

  def initializeHistograms(maxDocSize: Int) {
    _topicDocCounts = Array.ofDim[Int](numTopics, maxDocSize+1)
    for (i <- 0 until numTopics; j <- 0 to maxDocSize) _topicDocCounts(i)(j) = 0
  }

  private def recalcSmoothingMass: Double = {
    // s = \sum_t ( \alpha_t \beta ) / ( |V| \beta + n_t )  [Mimno "Sparse LDA"]
    var i = 0; var sm = 0.0
    while (i < numTopics) { sm += alphas(i) * beta1 / (phiCounts.mixtureCounts(i) + betaSum); i += 1 }
    sm
  }

  def export(phis:Seq[ProportionsVar], beta1:Double = 0.0, numTopics: Int=0): Unit = {
    phis.foreach(_.value.masses.zero())
    for (wi <- 0 until wordDomain.size)  {
      (0 until numTopics).foreach(ti => phis(ti).value.masses.+=(wi, beta1))
      phiCounts(wi).forCounts((ti,count) => phis(ti).value.masses.+=(wi, count))
    }
  }

  def exportThetas(docs:Iterable[Doc]): Unit = {
    for (doc <- docs) {
      val theta = doc.theta
      theta.value.masses.zero()
      for (dv <- doc.zs.discreteValues) theta.value.masses.+=(dv.intValue, 1.0)
    }
  }

  /** Sample the Zs for one document. */
  def process(zs:DiscreteSeqVariable, timeToEstAlphas: Boolean = false): Unit = {

    // TODO In some cases "smoothingMass" seems to drift low, perhaps due to math precision in its adjustments
    // So reset it here for each document
    //println(smoothingMass+" "+recalcSmoothingMass)
    //smoothingMass = recalcSmoothingMass
    //assert(smoothingMass > 0.0)
    //println("process doc "+zs.words.asInstanceOf[Document].file)
    val ws = model.childFactors(zs).head.asInstanceOf[PlatedCategoricalMixture.Factor]._1 //words
    //assert(ws.length == zs.length)
    // r = sum_t ( \beta n_{t|d} ) ( n_t + |V| \beta )  [Mimno "Sparse LDA"]
    var topicBetaMass = 0.0

    val localTopicCounts = new Array[Int](numTopics)
    val localTopicIndex = new Array[Int](numTopics)

    var zp = 0
    while(zp < zs.length){
      val zi = zs.intValue(zp)
      localTopicCounts(zi) += 1
      zp += 1
    }

    var tInd = 0
    var denseIndex = 0
    while(tInd < numTopics){
      if (localTopicCounts(tInd) != 0){
        localTopicIndex(denseIndex) = tInd
        denseIndex += 1
      }
      tInd += 1
    }
    var nonZeroTopics = denseIndex

    denseIndex = 0
    while(denseIndex < nonZeroTopics){
      //val ti = docTopicCounts.indexAtPosition(p) // topic index
      val ti = localTopicIndex(denseIndex)
      val ntd = localTopicCounts(ti) // n_{t|d}
      topicBetaMass += ntd * cachedDenominators(ti)
      cachedCoefficients(ti) = (alphas(ti) + ntd) * cachedDenominators(ti)
      denseIndex += 1
    }

    topicBetaMass *= beta1

    //println("\ndocTopicCounts "+docTopicCounts.counts+"  "+List(smoothingOnlyCount, topicBetaCount, topicTermCount).mkString(","))
    //println("cachedCoefficients "+cachedCoefficients.mkString(" "))
    //println("nt "+phiCounts.mixtureCounts.mkString(" "))

    // sample each z
    zp = 0
    while (zp < zs.length){
      val ti = zs.intValue(zp) // intValue of z, "topic index"
      //assert(ti < numTopics)
      val wi = ws.intValue(zp) // intValue of word, "word index"
      //assert(wi < WordDomain.size)
      val ntd = localTopicCounts(ti); //assert(ntd > 0) // n_{t|d}
      val phiCountsWi = phiCounts(wi)
      //assert(phiCountsWi.countOfIndex(ti) > 0) //ANTON: Note that this is a time-consuming statement.

      // Remove this token from various sufficient statistics
      // Don't yet remove counts from docTopicCounts or phiCounts b/c it might involve shifting holes
      // and we are fairly likely to just re-sample zi to match its old value anyway.
      val origSmoothingMass = smoothingMass; //assert(smoothingMass > 0.0) // Remember for later in case newTi == ti
      val origTopicBetaMass = topicBetaMass
      val origCachedCoefficientTi = cachedCoefficients(ti)
      val origCachedSmoothingTi = cachedSmoothing(ti)
      val origCachedDenominatorTi = cachedDenominators(ti)

      smoothingMass -= cachedSmoothing(ti) * beta1
      topicBetaMass -= beta1 * ntd * cachedDenominators(ti)

      cachedDenominators(ti) = 1.0 / (betaSum + phiCounts.mixtureCounts(ti) -1)
      cachedSmoothing(ti) = alphas(ti) * cachedDenominators(ti)
      smoothingMass += cachedSmoothing(ti) * beta1
      topicBetaMass += beta1 * (ntd-1) * cachedDenominators(ti)

      // Reset cachedCoefficients
      // If this is the last ti in this document and newTi != ti, then this value matches the "smoothing only" cachedCoefficient we want when we are done with this document
      cachedCoefficients(ti) = (alphas(ti) + (ntd-1)) * cachedDenominators(ti)

      // q = \sum_t ...  [Mimno "Sparse LDA"]
      var phiCountsWiTiPosition = -1

      var topicTermMass = 0.0

      var tp = 0
      while(tp < phiCountsWi.numPositions){
        val ti2 = phiCountsWi.indexAtPosition(tp)
        if(ti2 == ti) phiCountsWiTiPosition = tp

        val wiTi2Count = if(ti2 == ti) phiCountsWi.countAtPosition(tp)-1 else phiCountsWi.countAtPosition(tp)
        val score = cachedCoefficients(ti2) * wiTi2Count
        //assert(score >= 0)
        topicTermScores(tp) = score
        topicTermMass += score

        tp += 1
      }

      //assert(smoothingMass > 0)
      //assert(topicBetaMass > 0, "topicBetaMass="+topicBetaMass+" doc.length="+zs.length)
      //assert(topicTermMass >= 0)
      val r = random.nextDouble()
      var sample = r * (smoothingMass + topicBetaMass + topicTermMass)
      val origSample = sample
      //println("smoothingMass="+smoothingMass+" topicBetaMass="+topicBetaMass+" topicTermMass="+topicTermMass+" sample="+sample)


      var newTi = -1 // new topic index
      var newPosition = -1
      if (sample < topicTermMass) { // Sampling from within q  [Mimno]
        topicTermCount += 1
        var i = -1
        while (sample > 0) {
          i += 1
          sample -= topicTermScores(i)
        }
        newPosition = i
        newTi = phiCountsWi.indexAtPosition(i)

      } else if (sample < topicTermMass + topicBetaMass) { // Sampling from within r [Mimno]
        topicBetaCount += 1
        sample -= topicTermMass
        sample /= beta1

        denseIndex = 0
        while (sample > 0.0 && denseIndex < nonZeroTopics) {

          newTi = localTopicIndex(denseIndex)
          var nNewTiD = localTopicCounts(newTi)

          if (newTi == ti) {
            nNewTiD -= 1
          }

          sample -= nNewTiD * cachedDenominators(newTi)
          denseIndex += 1
        }
        // Allow for rounding error, but not too much
        if (sample * beta1 > 0.00001) throw new Error("Too much mass sample="+sample+" origSample="+origSample+" smoothingMass="+smoothingMass+" topicBetaMass="+topicBetaMass+" topicTermMass="+topicTermMass)
      } else {
        smoothingOnlyCount += 1
        sample -= topicTermMass + topicBetaMass
        sample /= beta1
        newTi = -1
        while (sample > 0) {
          newTi += 1
          if (newTi == numTopics) throw new Error("Too much mass sample="+sample+" r="+r)
          sample -= cachedSmoothing(newTi)
        }
      }
      /*newTi = ti // TODO Remove this!!!! It undoes all sampling.
      docTopicCounts.incrementCountAtIndex(ti, -1)
      docTopicCounts.incrementCountAtIndex(newTi, 1)
      phiCountsWi.incrementCountAtIndex(ti, -1)
      phiCountsWi.incrementCountAtIndex(newTi, 1)*/

      if (ti != newTi) {
        //println("ti="+newTi)
        localTopicCounts(ti) -= 1
        if (localTopicCounts(ti) == 0) {
					denseIndex = 0
          while (localTopicIndex(denseIndex) != ti)
						denseIndex += 1

					while (denseIndex < nonZeroTopics) {
						if (denseIndex < localTopicIndex.length - 1)
							localTopicIndex(denseIndex) = localTopicIndex(denseIndex + 1)

						denseIndex += 1
					}

					nonZeroTopics -= 1
				}

        localTopicCounts(newTi) += 1
        if (localTopicCounts(newTi) == 1) {
				  denseIndex = nonZeroTopics

          while (denseIndex > 0 && localTopicIndex(denseIndex - 1) > newTi) {
					 localTopicIndex(denseIndex) = localTopicIndex(denseIndex - 1)
					 denseIndex -= 1
				  }

				  localTopicIndex(denseIndex) = newTi
				  nonZeroTopics += 1
			  }

        if(newPosition != -1) {
          // We've sampled from the topicTermMass
          phiCounts.incrementCountsAtPositions(wi, ti, -1, phiCountsWiTiPosition, newTi, 1, newPosition)
        } else {
          phiCounts.increment(wi, ti, -1)
          phiCounts.increment(wi, newTi, 1)
        }

        if (localPhiCounts != null) {
          localPhiCounts.increment(wi, ti, -1)
          localPhiCounts.increment(wi, newTi, 1)
        }

        val newNt = phiCounts.mixtureCounts(newTi)
        val newNtd = localTopicCounts(newTi) // n_{t|d}
        smoothingMass -= cachedSmoothing(newTi) * beta1
        topicBetaMass -= beta1 * (newNtd-1) * cachedDenominators(newTi)

        cachedDenominators(newTi) = 1.0 / (betaSum + newNt)
        cachedSmoothing(newTi) = alphas(newTi) * cachedDenominators(newTi)
        smoothingMass += cachedSmoothing(newTi) * beta1
        if (smoothingMass <= 0.0) {
          println("smoothingMass="+smoothingMass+" alphas(ti)=%f beta1=%f newNt=%d betaSum=%f".format(alphas(ti), beta1, newNt, betaSum))
          val smoothingMass2 = (0 until numTopics).foldLeft(0.0)((sum,t) => sum + (alphas(t) * beta1 / (phiCounts.mixtureCounts(t) + betaSum))) // TODO This foldLeft does boxing.
          println("recalc smoothingMass="+smoothingMass2)
        }
        topicBetaMass += beta1 * newNtd * cachedDenominators(newTi)
        cachedCoefficients(newTi) = (alphas(newTi) + newNtd) * cachedDenominators(newTi)
        //cachedCoefficients(ti) = (alphas(ti) + (ntd-1)) / ((nt-1) + betaSum) // Already done above
        zs.set(zp, newTi)(null)  // Set the new value of z!
      } else {
        smoothingMass = origSmoothingMass
        topicBetaMass = origTopicBetaMass
        cachedCoefficients(ti) = origCachedCoefficientTi
        cachedSmoothing(ti) = origCachedSmoothingTi
        cachedDenominators(ti) = origCachedDenominatorTi
      }

      //val cachedCoefficients = Array.tabulate(numTopics)(t => alphas(t) / (phiCounts.mixtureCounts(t) + betaSum))
      //println("countsTotal = "+phiCounts.countsTotal)
      zp += 1
    }

    // Put back cachedCoefficients to only smoothing
    denseIndex = 0
    while(denseIndex < nonZeroTopics){
      val ti = localTopicIndex(denseIndex) // topic index
      cachedCoefficients(ti) = cachedSmoothing(ti)
      denseIndex += 1
    }

    if(timeToEstAlphas){

      denseIndex = 0
      while(denseIndex < nonZeroTopics){
        val ti = localTopicIndex(denseIndex) // topic index
        _topicDocCounts(ti)(localTopicCounts(ti)) += 1
        denseIndex += 1
      }

    }
  }

}

object SparseLDAInferencer {
  def apply(zDomain:DiscreteDomain,
    wordDomain:CategoricalDomain[String],
    docs:Iterable[Doc],
    initialAlphas:DoubleSeq,
    initialBeta1:Double,
    model:DirectedModel)(implicit random: scala.util.Random) : SparseLDAInferencer = {

    // Create and populate the (word,topic) counts
    val phiCounts = new DiscreteMixtureCounts(wordDomain, zDomain)

    for (doc <- docs)
      phiCounts.incrementFactor(model.parentFactor(doc.ws).asInstanceOf[PlatedCategoricalMixture.Factor], 1)

    new SparseLDAInferencer(zDomain, wordDomain, phiCounts, initialAlphas, initialBeta1, model, random)
  }
}
