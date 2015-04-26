/* Copyright (C) 2008-2015 University of Massachusetts Amherst.
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
package cc.factorie.optimize

import java.io._
import java.util.zip.GZIPInputStream

import cc.factorie._
import cc.factorie.la._
import cc.factorie.variable.HashFeatureVectorVariable

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * The Proximal Stochastic Dual Coordinate Ascent method for l1/l2 regularized learning from
 * Shalev-Shwartz and Zhang, "Proximal Stochastic Dual Coordinate Ascent"
 *
 * Currently only implemented for binary classification using smoothed hinge loss but can also be used for
 * multiclass and structured learning.
 *
 * Label "ys" should be -1 or 1
 *
 */
class SDCA(
  xs: Seq[Tensor], ys: Seq[Int],
  dualUpdate: DualUpdate[Tensor, Double, Int],
  l1: Double = 5.0,
  l2: Double = 2.5,
  eps: Double = 1e-4,
  maxIterations: Int = 100,
  evaluate: Option[Array[Double] => Unit] = None,
  logEveryN: Int = 1,
  sampleWithReplacement: Boolean = false)(implicit rng: Random) {

  val numExamples: Int = xs.length
  val dim: Int = xs.headOption.map(_.length).getOrElse(sys.error("Can't call SDCA with 0 instances"))
  // TODO: this should be a WeightsMap, so we can learn multiclass, CRF, etc. xs and ys should be changed as well.
  // dual iterate
  val vs: Array[Double] = new Array[Double](dim)
  // TODO: this should be a Array[Dual], with some method of combining xs with them to get WeightsMap, so we can learn e.g. multiclass or CRF
  // dual variables
  val alphas: Array[Double] = new Array[Double](numExamples)

  def learn(): Array[Double] = {
    val N = numExamples
    val M = maxIterations
    var epoch = 0
    var currentPermutation = Array.tabulate(numExamples)(i => i)

    while (epoch < M) {
      println("Starting epoch: " + epoch)
      val startEpoch = System.currentTimeMillis()
      currentPermutation = currentPermutation.sortBy(_ => rng.nextDouble())
      var j = 0
      while (j < N) {
        val i = if (sampleWithReplacement) rng.nextInt(numExamples) else currentPermutation(j)
        val newAlpha = dualUpdate.updateDual(alphas(i), l2, xs(i), predict(xs(i)), ys(i))
        updateAlpha(i, newAlpha)
        j += 1
      }

      val endEpoch = System.currentTimeMillis()

      println("Time to run epoch " + epoch + ": " + ((endEpoch - startEpoch) / 1000.0))

      val (primalLoss, primalRegularizer) = calculatePrimalLossAndRegularizerValue()
      val primal = (primalLoss + primalRegularizer) / N
      println("Primal objective: " + primal)
      val (dualLoss, dualRegularizer) = calculateDualLossAndRegularizerValue()
      val dual = (dualLoss - dualRegularizer) / N
      println("Dual objective: " + dual)
      val gap = primal - dual
      println("Duality gap: " + gap)
      
      // assert that Fenchel-Young holds with equality for regularizer:
      // i.e. g^*(v) + g(w) = <v, w>
      println("g^*(v) + g(w) = " + (primalRegularizer + dualRegularizer))
      println("<v, w> = " + vDotW())

      val nnz = buildWeights().count(0.0 !=)
      println("NNZ: " + nnz)
      println("Sparsity %: " + (nnz * 1.0 / N))

      if (epoch % logEveryN == 0) evaluate.foreach(_(buildWeights()))

      if (gap < eps)
        epoch = M

      epoch += 1
    }

    evaluate.foreach(_(buildWeights()))

    buildWeights()
  }

  def calculatePrimalLossAndRegularizerValue(): (Double, Double) = {
    var obj = 0.0
    var i = 0
    val len = numExamples
    while (i < len) {
      obj -= dualUpdate.objective.valueAndGradient(predict(xs(i)), ys(i))._1
      i += 1
    }
    var j = 0
    var oneNorm = 0.0
    var twoNormSq = 0.0
    val len2 = dim
    while (j < len2) {
      twoNormSq += weight(j) * weight(j) * 0.5 * l2
      oneNorm += l1 * math.abs(weight(j))
      j += 1
    }

    println("Primal norm: " + (twoNormSq + oneNorm))
    println("Primal loss: " + obj)

    (obj, twoNormSq + oneNorm)
  }

  def calculateDualLossAndRegularizerValue(): (Double, Double) = {
    var obj = 0.0
    var i = 0
    val len = numExamples
    while (i < len) {
      val dualLoss = dualUpdate.dualValueAndGradient(alphas(i), ys(i))._1
      assert(dualLoss >= 0, "Dual loss should be >= 0, instead got: " + dualLoss)
      obj += dualLoss
      i += 1
    }
    var j = 0
    var dualNorm = 0.0
    val len2 = dim
    while (j < len2) {
      val v = ISTAHelper.truncate(vs(j), l1)
      dualNorm += v * v * (0.5 / l2)
      j += 1
    }

    println("Dual norm: " + dualNorm)
    println("Dual loss: " + obj)

    (obj, dualNorm)
  }

  def updateAlpha(idx: Int, newAlphaPos: Double): Unit = {
    val x = xs(idx)
    val oldAlphaPos = alphas(idx)
    if (newAlphaPos == oldAlphaPos) return
    alphas(idx) = newAlphaPos
    val oldSignedAlpha = oldAlphaPos * ys(idx)
    val newSignedAlpha = newAlphaPos * ys(idx)
    val deltaAlpha = newSignedAlpha - oldSignedAlpha
    x match {
      case o: SparseTensor =>
        val len = o.activeDomainSize
        val indices = o._indices
        val values = o._valuesSeq
        var i = 0
        while (i < len) {
          val idx = indices(i)
          val value = values(i)
          vs(idx) += value * deltaAlpha
          i += 1
        }
      case o: DenseTensor =>
        val arr = o.asArray
        var idx = 0
        while (idx < arr.length) {
          val value = arr(idx)
          vs(idx) += value * deltaAlpha
          idx += 1
        }
      case t: Tensor =>
        t.foreachActiveElement((idx, value) => {
          vs(idx) += value * deltaAlpha
        })
    }
  }
  def buildWeights() = Array.tabulate(dim)(i => weight(i))
  def predict(x: Tensor): Double = x.foldActiveElements(0.0, (i, v, acc) => acc + weight(i) * v)
  def weight(i: Int): Double = (1.0 / l2) * ISTAHelper.truncate(vs(i), l1)
  def vDotW(): Double = {
    var ret = 0.0
    var i = 0
    while (i < dim) {
      ret += vs(i) * weight(i)
      i += 1
    }
    ret
  }
}

trait DualUpdate[Features, Prediction, Output] {
  def objective: OptimizableObjective[Prediction, Output]
  def dualValueAndGradient(alpha: Double, label: Output): (Double, Double)
  def updateDual(oldAlpha: Double, l2: Double, feats: Features, pred: Prediction, label: Output): Double
  def projectDual(alpha: Double, label: Output): Double
}
trait UnivariateDualUpdate[Output] extends DualUpdate[Tensor, Double, Output]

object UnivariateDualUpdate {
  def smoothHingeBinaryDualUpdate(gamma: Double = 1.0, margin: Double = 1.0, posCost: Double = 1.0, negCost: Double = 1.0) =
    new SmoothHingeBinaryDualUpdate(gamma, margin, posCost, negCost)

  class SmoothHingeBinaryDualUpdate(val gamma: Double = 1.0, val margin: Double = 1.0, val posCost: Double = 1.0, val negCost: Double = 1.0)
    extends UnivariateDualUpdate[Int] {
    val objective = OptimizableObjectives.smoothHingeBinary(gamma, margin, posCost, negCost)
    // TODO: this doesn't take margin or cost into account, and always projects to [0,1] rather than looking at label
    def updateDual(oldAlpha: Double, l2: Double, feats: Tensor, pred: Double, label: Int): Double = {
      val u = (1 - pred * label - gamma * oldAlpha) / (feats.twoNormSquared / l2 + gamma) + oldAlpha
      math.max(0, math.min(1, u))
    }
    // TODO: this doesn't take label into account
    def dualValueAndGradient(alpha: Double, label: Int): (Double, Double) = {
      val cost = if (label == 1) posCost else negCost
      val value = alpha * margin - gamma / (2 * cost) * alpha * alpha
      val grad = if (alpha == 0.0) 0.0 else margin - gamma / cost * alpha
      (value, grad)
    }
    def projectDual(alpha: Double, label: Int): Double = {
      val cost = if (label == 1) posCost else negCost
      if (label == 1) math.min(0, math.max(-cost, alpha))
      else math.min(cost, math.max(0, alpha))
    }
  }
}

object RCV1DemoSDCA {
  def main(args: Array[String]): Unit = {
    implicit val rng = new scala.util.Random(1)

    val hashDomainSize = 1 << 22

    val rcv1Path = """../../data/rcv1"""
    val rcv1TrainPath = rcv1Path + "/" + "rcv1.train.txt.gz"
    val rcv1TestPath = rcv1Path + "/" + "rcv1.test.txt.gz"

    val trainInstances = SVMLightHelpers.loadSVMLight(rcv1TrainPath, hashDomainSize).shuffle
    val testInstances = SVMLightHelpers.loadSVMLight(rcv1TestPath, hashDomainSize)

    def evaluate(arr: Array[Double]): Unit = {
      val weights = new DenseTensor1(arr)
      var testCorrect = 0.0
      for ((x, y2) <- testInstances; y = if (y2 == 0) -1 else 1) {
        if ((weights dot x) * y > 0) testCorrect += 1.0
      }
      val acc = testCorrect / testInstances.size
      println("Test accuracy: " + acc)
    }

    println("Loaded " + trainInstances.size + " train instances.")

    val sdca = new SDCA(trainInstances.map(_._1), trainInstances.map(t => if (t._2 == 0) -1 else 1), UnivariateDualUpdate.smoothHingeBinaryDualUpdate(), evaluate = Some(evaluate))

    sdca.learn()
  }

  object SVMLightHelpers {
    val svmLightSeparatorRegex = """\s|:""".r
    def loadSVMLight[T](file: String, hashDomainSize: Int): Seq[(SparseIndexedTensor1, Int)] = {
      val source = readFile(new java.io.File(file), gzip = true)
      val instances = ArrayBuffer[(SparseIndexedTensor1, Int)]()
      var lineNum = 0
      var line = null: String
      while ( {line = source.readLine(); line != null /*&& lineNum < 1000*/}) {
        lineNum += 1
        val fields = svmLightSeparatorRegex.split(line)
        val label = fields(0)
        val instance = new SparseIndexedTensor1(hashDomainSize)
        var i = 1
        val len = fields.length
        while (i < len) {
          val idx = fields(i).toInt
          val value = fields(i + 1).toDouble
          val bits = idx
          val hashIdx = HashFeatureVectorVariable.index(bits, hashDomainSize)
          val hashSign = HashFeatureVectorVariable.sign(bits)
          instance +=(hashIdx, value * hashSign)
          i += 2
        }
        instance._makeReadable()
        instances += ((instance, if (label.toInt == -1) 0 else 1))
      }
      instances
    }
    def readFile(file: File, gzip: Boolean = false): BufferedReader = {
      val fileStream = new BufferedInputStream(new FileInputStream(file))
      new BufferedReader(new InputStreamReader(if (gzip) new GZIPInputStream(fileStream) else fileStream))
    }
  }
}