package cc.factorie.bp.specialized

import cc.factorie._
import cc.factorie.maths.{sumLogProb, sumLogProbs}
import la.{SparseVector, DenseVector, Vector}
import scala.math.exp
import collection.mutable.Map

/**
 * Author: martin
 * Date: 2/28/12
 */

object ForwardBackward {

  private def nodeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]]): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size)(Array.fill(ds)(Double.NaN))

    // exponentiate all alphas and betas, normalize them, and then multiply them together, normalize, and store as marginals
    var vi = 0
    while (vi < alpha.size) {
      var logsum = Double.NegativeInfinity
      var i = 0
      while (i < ds) {
        marginal(vi)(i) = alpha(vi)(i) + beta(vi)(i)
        logsum = sumLogProb(logsum, marginal(vi)(i))
        i += 1
      }

      // normalize the node marginal
      logNormalize(marginal(vi), logsum)
      vi += 1
    }

    marginal
  }

  private def edgeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]], transWeights: (Int,Int) => Double): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size-1)(Array.fill(ds * ds)(Double.NaN))

    var vi = 0
    while (vi < alpha.size - 1) {
      var logsum = Double.NegativeInfinity
      var i = 0
      while (i < ds) {
        var j = 0
        while (j < ds) {
          val tmp = alpha(vi)(i) + transWeights(i,j) + beta(vi+1)(j)
          marginal(vi)(i * ds + j) = tmp
          logsum = sumLogProb(logsum, tmp)
          j += 1
        }
        i += 1
      }

      // normalize the edge marginal
      logNormalize(marginal(vi), logsum)
      vi += 1
    }

    marginal
  }

  private def logNormalize(a: Array[Double], logsum: Double) {
    var i = 0
    var sum = 0.0
    while (i < a.length) {
      a(i) = exp(a(i) - logsum)
        sum += a(i)
        i += 1
      }
      assert(math.abs(sum - 1.0) < 0.0001, "sum is "+sum)
  }

  private def getLocalScores[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
         vs: Seq[LV],
         localTemplate: TemplateWithDotStatistics2[LV, OV],
         biasTemplate: TemplateWithDotStatistics1[LV] = null
       ): Array[Array[Double]] = {

    val arrays = Array.fill(vs.size)(Array.fill(vs.head.domain.size)(Double.NaN))
    if (biasTemplate ne null) {
      val biasScores = biasTemplate.weights
      for ((v,vi) <- vs.zipWithIndex) {
        val localFactor = localTemplate.factors(v).head
        for ((_,di) <- v.settings.zipWithIndex)
          arrays(vi)(di) = localFactor.score + biasScores(di)
      }
    }
    else {
      for ((v,vi) <- vs.zipWithIndex) {
        val localFactor = localTemplate.factors(v).head
        for ((_,di) <- v.settings.zipWithIndex)
          arrays(vi)(di) = localFactor.score
      }
    }

    arrays
  }

  private def getTransScores[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
         transTemplate: TemplateWithDotStatistics2[LV, LV]
       ): (Int,Int) => Double = {
    val ds = transTemplate.statisticsDomains(1).dimensionSize // optimization
    (i: Int, j: Int) => transTemplate.weights(i * ds + j)
  }
  
  private def elementwiseSum(as: Array[Array[Double]]): Array[Double] = {
    val result = Array.fill(as(0).size)(0.0)
    var i = 0
    while (i < as.size) {
      val a = as(i)
      var j = 0
      while (j < a.size) {
        result(j) += a(j)
        j += 1
      }
      i += 1
    }
    result
  }

  private def vectorFromArray(a: Array[Double]): Vector = {
    val v = new DenseVector(a.size)
    var i = 0
    while (i < a.size) {
      v.update(i, a(i))
      i += 1
    }
    v
  }

  def featureExpectationsAndLogZ[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
            vs: Seq[LV],
            localTemplate: TemplateWithDotStatistics2[LV, OV],
            transTemplate: TemplateWithDotStatistics2[LV, LV],
            biasTemplate: TemplateWithDotStatistics1[LV] = null
          ): (Map[DotFamily, Vector], Double) = {

    val (alpha, beta) = search(vs, localTemplate, transTemplate, biasTemplate)

    val nodeMargs = nodeMarginals(alpha, beta)
    val edgeMargs = edgeMarginals(alpha, beta, getTransScores(transTemplate))

    val nodeExp = new SparseVector(localTemplate.statisticsVectorLength)

    // get the node feature expectations
    for ((v, vi) <- vs.zipWithIndex) { // for variables
      var i = 0
      while (i < nodeMargs(vi).length) {
        v.set(i)(null)
        val m = nodeMargs(vi)(i)
        val stats = localTemplate.factors(v).head.values.statistics.vector
        nodeExp += (stats * m)
        i += 1
      }
    }

    // sum edge marginals
    val edgeExp = vectorFromArray(elementwiseSum(edgeMargs))

    val logZ = sumLogProbs(alpha(alpha.length-1))

    (Map(localTemplate -> nodeExp, transTemplate -> edgeExp), logZ)
  }

  def search[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
            vs: Seq[LV],
            localTemplate: TemplateWithDotStatistics2[LV, OV],
            transTemplate: TemplateWithDotStatistics2[LV, LV],
            biasTemplate: TemplateWithDotStatistics1[LV] = null
          ): (Array[Array[Double]], Array[Array[Double]]) = {

    if (vs.isEmpty) throw new Error("Can't run forwardBackward without variables.")

    val ds = vs.head.domain.size

    val localScores = getLocalScores(vs, localTemplate, biasTemplate)
    val transScores = getTransScores(transTemplate)

    val alpha = Array.fill(vs.size)(Array.fill(ds)(Double.NaN))
    val beta = Array.fill(vs.size)(Array.fill(ds)(Double.NaN))
    
    // forward
    var vi = 0
    var i = 0
    while (i < ds) {
      alpha(vi)(i) = localScores(vi)(i)
      i += 1
    }
    vi += 1
    while (vi < vs.size) {
      i = 0
      while(i < ds) {
        var newAlpha = Double.NegativeInfinity
        var j = 0
        while (j < ds) {
          newAlpha = sumLogProb(newAlpha, alpha(vi-1)(j) + transScores(j,i) + localScores(vi)(i))
          j += 1
        }
        alpha(vi)(i) = newAlpha
        i += 1
      }
      vi += 1
    }

    // backward
    vi = vs.size-1
    i = 0
    while (i < ds) {
      beta(vi)(i) = localScores(vi)(i)
      i += 1
    }
    vi -= 1
    while (vi >= 0) {
      i = 0
      while (i < ds) {
        var newBeta = Double.NegativeInfinity
        var j = 0
        while (j < ds) {
          newBeta = sumLogProb(newBeta, localScores(vi)(i) + transScores(i, j) + beta(vi+1)(j))
          j += 1
        }
        beta(vi)(i) = newBeta
        i += 1
      }
      vi -= 1
    }


    (alpha, beta)
  }

}


