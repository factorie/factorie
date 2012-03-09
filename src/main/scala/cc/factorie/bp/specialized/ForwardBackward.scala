package cc.factorie.bp.optimized

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

    val marginal = Array.fill(alpha.size)(Array.ofDim[Double](ds))

    // exponentiate all alphas and betas, normalize them, and then multiply them together, normalize, and store as marginals
    var vi = 0
    while (vi < alpha.size) {

      var i = 0
      var logsum = Double.NegativeInfinity
      while (i < ds) {
        marginal(vi)(i) = alpha(vi)(i) + beta(vi)(i)
        logsum = sumLogProb (logsum, marginal(vi)(i))
        i += 1
      }

      // normalize the node marginal
      i = 0
      while (i < ds) {
        marginal(vi)(i) = exp(marginal(vi)(i) - logsum)
        i += 1
      }

      vi += 1
    }

    marginal
  }

  private def edgeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]], transWeights: (Int,Int) => Double): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size-1)(Array.ofDim[Double](ds * ds))

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
      i = 0
      while (i < marginal(vi).length) {
        marginal(vi)(i) = exp(marginal(vi)(i) - logsum)
        i += 1
      }
      vi += 1
    }

    marginal
  }
  
  private def getlocalScores[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
         vs: Seq[LV],
         localTemplate: TemplateWithDotStatistics2[LV, OV],
         biasTemplate: TemplateWithDotStatistics1[LV] = null
       ): Array[Array[Double]] = {

    val biasScores: Vector = {
      if (biasTemplate ne null)
        biasTemplate.weights
      else
        new DenseVector(localTemplate.statisticsDomains(0).dimensionSize) // todo: this allocation is unnecessary, and the domain size is awkward
    }

    val arrays = Array.fill(vs.size)(Array.fill(vs.head.domain.size)(Double.NaN))
    for ((v,vi) <- vs.zipWithIndex) {
      val localFactor = localTemplate.factors(v).head
      for ((_,di) <- v.settings.zipWithIndex)
        arrays(vi)(di) = localFactor.score + biasScores(di)
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
        val stats = localTemplate.unroll1(v).head.values.statistics.vector
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

    val localScores = getlocalScores(vs, localTemplate, biasTemplate)
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
        var newAlpha = 0.0
        var di = 0
        while (di < ds) {
          val prev = alpha(vi-1)(di)
          newAlpha = sumLogProb(newAlpha, prev + transScores(di,i) + localScores(vi)(i))
          di += 1
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
      beta(vi)(i) = 0.0
      i += 1
    }
    vi -= 1
    while (vi >= 0) {
      i = 0
      while (i < ds) {
        var newBeta = 0.0
        var di = 0
        while (di < ds) {
          val prev = beta(vi+1)(di)
          newBeta = sumLogProb(newBeta, prev + transScores(i, di) + localScores(vi)(i))
          di += 1
        }
        beta(vi)(i) = newBeta
        i += 1
      }
      vi -= 1
    }


    (alpha, beta)
  }

}


