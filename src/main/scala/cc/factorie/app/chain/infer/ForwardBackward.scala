package cc.factorie.app.chain.infer

import cc.factorie._
import cc.factorie.maths.sumLogProbs
import cc.factorie.util.ArrayDoubleSeq
import cc.factorie.la._
import scala.math.exp
import collection.mutable
import cc.factorie.variable.{TensorVar, MutableDiscreteVar}
import cc.factorie.model.{DotFamilyWithStatistics2, DotFamilyWithStatistics1}

/**
 * Author: martin
 * Date: 2/28/12
 */

object ForwardBackward {

  def nodeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]]): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size)(Array.ofDim[Double](ds))

    // exponentiate all alphas and betas, normalize them, and then multiply them together, normalize, and store as marginals
    var vi = 0
    while (vi < alpha.size) {
      var i = 0
      while (i < ds) {
        marginal(vi)(i) = alpha(vi)(i) + beta(vi)(i)
        i += 1
      }

      // normalize the node marginal
      logNormalize(marginal(vi), sumLogProbs(new ArrayDoubleSeq(marginal(vi))))
      vi += 1
    }

    marginal
  }

  def edgeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]], transWeights: (Int,Int) => Double, localScores: Array[Array[Double]]): Array[Array[Double]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size-1)(Array.ofDim[Double](ds * ds))

    var vi = 0
    while (vi < alpha.size - 1) {
      var i = 0
      while (i < ds) {
        var j = 0
        while (j < ds) {
          marginal(vi)(i * ds + j) = alpha(vi)(i) + transWeights(i,j) + beta(vi+1)(j) + localScores(vi+1)(j)
          j += 1
        }
        i += 1
      }

      // normalize the edge marginal
      logNormalize(marginal(vi), sumLogProbs(new ArrayDoubleSeq(marginal(vi))))
      vi += 1
    }

    marginal
  }
  
  def dim2edgeMarginals(alpha: Array[Array[Double]], beta: Array[Array[Double]], transWeights: (Int,Int) => Double, localScores: Array[Array[Double]]): Array[Array[Array[Double]]] = {
    val ds = alpha(0).size

    val marginal = Array.fill(alpha.size-1)(Array.ofDim[Double](ds, ds))

    var vi = 0
    while (vi < alpha.size - 1) {
      var i = 0
      while (i < ds) {
        var j = 0
        while (j < ds) {
          marginal(vi)(i)(j) = alpha(vi)(i) + transWeights(i,j) + beta(vi+1)(j) + localScores(vi+1)(j)
          j += 1
        }
        i += 1
      }

      // normalize the edge marginal
      //logNormalize(marginal(vi), sumLogProbs(new ArrayDoubleSeq(marginal(vi))))
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

  private def getLocalScores[OV <: TensorVar, LV <: MutableDiscreteVar](
         vs: Seq[LV],
         localTemplate: DotFamilyWithStatistics2[LV, OV],
         biasTemplate: DotFamilyWithStatistics1[LV],
         labelToFeatures: LV => OV
       ): Array[Array[Double]] = {

    val arrays = Array.fill(vs.size)(Array.ofDim[Double](vs.head.domain.size))
    if (biasTemplate ne null) {
      val biasScores = biasTemplate.weights.value
      var vi = 0
      while (vi < vs.size) {
        val localFactor = localTemplate.Factor(vs(vi), labelToFeatures(vs(vi)))
        val settings = vs(vi).settings
        var di = 0
        while (settings.hasNext) {
          settings.next()
          arrays(vi)(di) = biasScores(di) + localFactor.currentScore
          di += 1
        }
        vi += 1
      }
    }
    else {
      var vi = 0
      while (vi < vs.size) {
        val localFactor = localTemplate.Factor(vs(vi), labelToFeatures(vs(vi)))
        val settings = vs(vi).settings
        var di = 0
        while (settings.hasNext) {
          settings.next()
          arrays(vi)(di) = localFactor.currentScore
          di += 1
        }
        vi += 1
      }
    }

    arrays
  }

  private def getTransScores[LV <: TensorVar](
         transTemplate: DotFamilyWithStatistics2[LV, LV]
       ): (Int,Int) => Double = {
    val weightsValue = transTemplate.weights.value
    val ds = weightsValue.dim2 // optimization // TODO: this is from Feb 2012, is it still faster? -brian
    (i: Int, j: Int) => weightsValue(i * ds + j)
  }

  def elementwiseSum(as: Array[Array[Double]]): Array[Double] = {
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

  def marginalsAndLogZ[OV <: TensorVar, LV <: MutableDiscreteVar](
            vs: Seq[LV],
            localTemplate: DotFamilyWithStatistics2[LV, OV],
            transTemplate: DotFamilyWithStatistics2[LV, LV],
            biasTemplate: DotFamilyWithStatistics1[LV],
            LabelToFeatures: LV => OV
          ): (Array[Array[Double]], Array[Array[Double]], Double) = {

    val (alpha, beta, localScores) = search(vs, localTemplate, transTemplate, biasTemplate, LabelToFeatures)

    val nodeMargs = nodeMarginals(alpha, beta)
    val edgeMargs = edgeMarginals(alpha, beta, getTransScores(transTemplate), localScores)

    val logZ = sumLogProbs(new ArrayDoubleSeq(alpha(alpha.length-1)))

    (nodeMargs, edgeMargs, logZ)
  }

  def nodeEdgeMarginalsAndLogZ[OV <: TensorVar, LV <: MutableDiscreteVar](
            vs: Seq[LV],
            localTemplate: DotFamilyWithStatistics2[LV, OV],
            transTemplate: DotFamilyWithStatistics2[LV, LV],
            biasTemplate: DotFamilyWithStatistics1[LV],
            LabelToFeatures: LV => OV
          ): (Array[Array[Double]], Array[Array[Array[Double]]], Double) = {

    val (alpha, beta, localScores) = search(vs, localTemplate, transTemplate, biasTemplate, LabelToFeatures)

    val nodeMargs = nodeMarginals(alpha, beta)
    val edgeMargs = dim2edgeMarginals(alpha, beta, getTransScores(transTemplate), localScores)

    val logZ = sumLogProbs(new ArrayDoubleSeq(alpha(alpha.length-1)))

    (nodeMargs, edgeMargs, logZ)
  }


  def search[OV <: TensorVar, LV <: MutableDiscreteVar](
            vs: Seq[LV],
            localTemplate: DotFamilyWithStatistics2[LV, OV],
            transTemplate: DotFamilyWithStatistics2[LV, LV],
            biasTemplate: DotFamilyWithStatistics1[LV],
            labelToFeatures: LV => OV
          ): (Array[Array[Double]], Array[Array[Double]], Array[Array[Double]]) = {

    if (vs.isEmpty) throw new Error("Can't run forwardBackward without variables.")
    if (vs.size < 2) throw new Error("Can't run forwardBackward on one variable.")

    val ds = vs.head.domain.size

    val localScores = getLocalScores(vs, localTemplate, biasTemplate, labelToFeatures)
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
        alpha(vi)(i) = sumLogProbs(new ArrayDoubleSeq(tmpCol))
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
        beta(vi)(i) = sumLogProbs(new ArrayDoubleSeq(tmpCol))
        i += 1
      }
      vi -= 1
    }

    (alpha, beta, localScores)
  }

}


