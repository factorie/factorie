package cc.factorie.bp.specialized

import cc.factorie._
import cc.factorie.maths.sumLogProbs
import cc.factorie.la._
import scala.math.exp
import collection.mutable.{HashMap, Map}

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
      var i = 0
      while (i < ds) {
        marginal(vi)(i) = alpha(vi)(i) + beta(vi)(i)
        i += 1
      }

      // normalize the node marginal
      logNormalize(marginal(vi), sumLogProbs(marginal(vi)))
      vi += 1
    }

    marginal
  }

  private def edgeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]], transWeights: (Int,Int) => Double): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size-1)(Array.fill(ds * ds)(Double.NaN))

    var vi = 0
    while (vi < alpha.size - 1) {
      var i = 0
      while (i < ds) {
        var j = 0
        while (j < ds) {
          marginal(vi)(i * ds + j) = alpha(vi)(i) + transWeights(i,j) + beta(vi+1)(j)
          j += 1
        }
        i += 1
      }

      // normalize the edge marginal
      logNormalize(marginal(vi), sumLogProbs(marginal(vi)))
      vi += 1
    }

    marginal
  }

  private def logNormalize(a: Array[Double], logSum: Double) {
    var i = 0
    var sum = 0.0
    while (i < a.length) {
      a(i) = exp(a(i) - logSum)
      sum += a(i)
      i += 1
    }
    assert(math.abs(sum - 1.0) < 0.0001, "sum is "+sum)
  }

  private def getLocalScores[OV <: DiscreteTensorVar, LV <: LabelVariable[_]](
         vs: Seq[LV],
         localTemplate: TemplateWithDotStatistics2[LV, OV],
         biasTemplate: TemplateWithDotStatistics1[LV] = null
       ): Array[Array[Double]] = {

    val arrays = Array.fill(vs.size)(Array.ofDim[Double](vs.head.domain.size))
    if (biasTemplate ne null) {
      val biasScores = biasTemplate.weights
      var vi = 0
      while (vi < vs.size) {
        val localFactor = localTemplate.factors(vs(vi)).head
        val settings = vs(vi).settings
        var di = 0
        while (settings.hasNext) {
          settings.next()
          arrays(vi)(di) = biasScores(di) + localFactor.score 
          di += 1
        }
        vi += 1
      }
    }
    else {
      var vi = 0
      while (vi < vs.size) {
        val localFactor = localTemplate.factors(vs(vi)).head
        val settings = vs(vi).settings
        var di = 0
        while (settings.hasNext) {
          settings.next()
          arrays(vi)(di) = localFactor.score
          di += 1
        }
        vi += 1
      }
    }

    arrays
  }

  private def getTransScores[OV <: DiscreteTensorVar, LV <: LabelVariable[_]](
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

  private def vectorFromArray(a: Array[Double]): Tensor = {
    val v = new DenseTensor1(a.size)
    var i = 0
    while (i < a.size) {
      v.update(i, a(i))
      i += 1
    }
    v
  }

  def featureExpectationsAndLogZ[OV <: DiscreteTensorVar, LV <: LabelVariable[_]](
            vs: Seq[LV],
            localTemplate: TemplateWithDotStatistics2[LV, OV],
            transTemplate: TemplateWithDotStatistics2[LV, LV],
            biasTemplate: TemplateWithDotStatistics1[LV] = null
          ): (Map[DotFamily, Tensor], Double) = {

    val (alpha, beta) = search(vs, localTemplate, transTemplate, biasTemplate)

    val nodeMargs = nodeMarginals(alpha, beta)
    val edgeMargs = edgeMarginals(alpha, beta, getTransScores(transTemplate))

    // sum edge marginals
    // TODO Instead of new SparseTensor1 consider something like Tensor.newSparse(transTemplate.weights)
    val edgeExp = if (vs.length > 1) vectorFromArray(elementwiseSum(edgeMargs)) else new SparseTensor1(vs(0).domain.size*vs(0).domain.size)

    // get the node feature expectations
    val nodeExp = new SparseTensor1(localTemplate.weights.length) // statisticsVectorLength
    for ((v, vi) <- vs.zipWithIndex) { // for variables
      var i = 0
      while (i < nodeMargs(vi).length) {
        v.set(i)(null)
        val m = nodeMargs(vi)(i)
        val stats = localTemplate.factors(v).head.values.statistics.tensor
        nodeExp += (stats * m)
        i += 1
      }
    }

    val expMap: Map[DotFamily, Tensor] = HashMap(localTemplate -> nodeExp, transTemplate -> edgeExp)

    val logZ = sumLogProbs(alpha(alpha.length-1))

    (expMap, logZ)
  }

  def search[OV <: DiscreteTensorVar, LV <: LabelVariable[_]](
            vs: Seq[LV],
            localTemplate: TemplateWithDotStatistics2[LV, OV],
            transTemplate: TemplateWithDotStatistics2[LV, LV],
            biasTemplate: TemplateWithDotStatistics1[LV] = null
          ): (Array[Array[Double]], Array[Array[Double]]) = {

    if (vs.isEmpty) throw new Error("Can't run forwardBackward without variables.")

    val ds = vs.head.domain.size

    val localScores = getLocalScores(vs, localTemplate, biasTemplate)
    val transScores = getTransScores(transTemplate)

    val alpha = Array.fill(vs.size)(Array.ofDim[Double](ds))
    val beta = Array.fill(vs.size)(Array.ofDim[Double](ds))
    val tmpCol = Array.ofDim[Double](ds)
    
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
        var j = 0
        while (j < ds) {
          tmpCol(j) = alpha(vi-1)(j) + transScores(j,i) + localScores(vi)(i)
          j += 1
        }
        alpha(vi)(i) = sumLogProbs(tmpCol)
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
        var j = 0
        while (j < ds) {
          tmpCol(j) = transScores(i, j) + localScores(vi+1)(j) + beta(vi+1)(j)
          j += 1
        }
        beta(vi)(i) = sumLogProbs(tmpCol)
        i += 1
      }
      vi -= 1
    }

    (alpha, beta)
  }

}


