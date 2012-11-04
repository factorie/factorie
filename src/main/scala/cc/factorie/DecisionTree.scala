/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie

import la.{SparseBinaryTensorLike1, SparseTensor1, Tensor1, Tensor}
import collection.mutable.{ArrayBuilder, Stack, ArrayBuffer}

/**A template for factors who scores are the log-probability of
    label S1 given feature vector S2, according to a decision tree.
    @author Luke Vilnis*/
abstract class DecisionTreeTemplateWithStatistics2[V1 <: DiscreteVar, V2 <: DiscreteTensorVar]
  (val labelToFeatures: V1 => V2, val labelDomain: DiscreteDomain, val featureDomain: DiscreteTensorDomain)
  (implicit m1: Manifest[V1], m2: Manifest[V2])
  extends Template2[V1, V2] {

  type S1 = V1#Value
  type S2 = V2#Value
  type StatisticsType = (S1, S2)

  def unroll1(label: V1) = Factor(label, labelToFeatures(label))
  def unroll2(features: V2) = throw new Error("Cannot unroll from feature variables.")
  override def statisticsScore(t:Tensor): Double = throw new Error("How should this work??")

  override def statistics(value1:S1, value2:S2): StatisticsType = (value1, value2)
  // Fixme: DiscreteVar should have value: Value not value: DiscreteValue... but then can't override. Need VarLike?
  def statistics(f: FactorType): StatisticsType = (f._1.value.asInstanceOf[S1], f._2.value.asInstanceOf[S2])
  def train(labels: Iterable[V1]): Unit = train(labels.flatMap(unroll1(_)).map(statistics(_)), instanceWeights = None)
  def train(labels: Iterable[V1], instanceWeights: Tensor1): Unit =
    train(labels.map(unroll1(_)).flatten.map(f => statistics(f._1.value.asInstanceOf[S1], f._2.value.asInstanceOf[S2])), instanceWeights = Some(instanceWeights))

  type State
  def getPerNodeState(stats: Seq[StatisticsType]): State
  def evaluateSplittingCriteria(s: State, withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double
  def shouldStop(stats: Seq[StatisticsType], depth: Int): Boolean
  def prune(tree: DTree, pruningSet: Seq[StatisticsType]): DTree

  // each branch in the decision tree corresponds to an index in the feature vector (a feature) and threshold for the feature (for continuous values)
  // each leaf in the decision tree corresponds to a weighted proportions of each label
  sealed trait DTree {
    lazy val leaves: Set[DTLeaf] = this match {
      case l: DTLeaf => Set(l)
      case DTBranch(yes, no, _, _) => yes.leaves ++ no.leaves
    }
  }
  case class DTBranch(yes: DTree, no: DTree, featureIndex: Int, threshold: Double) extends DTree
  case class DTLeaf(proportions: Proportions) extends DTree

  lazy val numLabels: Int = labelDomain.size

  var decisionTree = None: Option[DTree]

  def splittingFeatures: Seq[Int] = {
    def inner(tree: DTree): Seq[Int] = tree match {
      case DTBranch(yes, no, idx, thresh) => idx +: (inner(yes) ++ inner(no))
      case _ => Seq.empty[Int]
    }
    decisionTree.toSeq.flatMap(inner(_))
  }

  def train(stats: Iterable[StatisticsType], maxDepth: Int = 100, instanceWeights: Option[Tensor1] = None): Unit = {
    val tree =
      if (maxDepth < 1000) trainTree(stats.toSeq, 0, maxDepth, Set.empty[(Int, Double)], instanceWeights)
      else trainTreeNoMaxDepth(stats.toSeq, maxDepth, instanceWeights)
    //println(tree)
    decisionTree = Some(tree)
  }

  def prune(pruningSet: Iterable[StatisticsType]): Unit =
    decisionTree = Some(prune(decisionTree.get, pruningSet.toSeq))

  def score(s: StatisticsType): Double = score(s, decisionTree.get)

  def score(s1: S1, s2: S2): Double = score(statistics(s1, s2), decisionTree.get)

  private def trainTree(stats: Seq[StatisticsType], depth: Int, maxDepth: Int, usedFeatures: Set[(Int, Double)], instanceWeights: Option[Tensor1]): DTree = {
    if (maxDepth < depth || stats.map(_._1).distinct.length == 1 || shouldStop(stats, depth)) {
      DTLeaf(makeProportions(stats))
    } else {
      val (featureIdx, threshold) = getSplittingFeatureAndThreshold(stats.toArray, usedFeatures, instanceWeights)
      val (statsWithFeature, statsWithoutFeature) = stats.partition(hasFeature(featureIdx, _, threshold))
      @inline def trainChildTree(childStats: Seq[StatisticsType]): DTree =
        if (childStats.length > 0) trainTree(childStats, depth + 1, maxDepth, usedFeatures + ((featureIdx, threshold)), instanceWeights)
        else DTLeaf(makeProportions(stats))
      DTBranch(trainChildTree(statsWithFeature), trainChildTree(statsWithoutFeature), featureIdx, threshold)
    }
  }

  private def trainTreeNoMaxDepth(startingSamples: Seq[StatisticsType], maxDepth: Int, instanceWeights: Option[Tensor1]): DTree = {
    // Use arraybuffer as dense mutable int-indexed map - no IndexOutOfBoundsException, just expand to fit
    type DenseIntMap[T] = ArrayBuffer[T]
    def updateIntMap[@specialized T](ab: DenseIntMap[T], idx: Int, item: T, dfault: T = null.asInstanceOf[T]) = {
      if (ab.length <= idx) { ab.insertAll(ab.length, Iterable.fill(idx - ab.length + 1)(dfault)) }
      ab.update(idx, item)
    }
    var currentChildId = 0 // get childIdx or create one if it's not there already
    def child(childMap: DenseIntMap[Int], heapIdx: Int) =
      if (childMap.length > heapIdx && childMap(heapIdx) != -1) childMap(heapIdx)
      else {currentChildId += 1; updateIntMap(childMap, heapIdx, currentChildId, -1); currentChildId }
    // go down
    val leftChildren, rightChildren = new DenseIntMap[Int]() // heapIdx -> childHeapIdx
    val todo = Stack((startingSamples, Set.empty[(Int, Double)], 0, 0)) // samples, usedFeatures, depth, heapIdx
    val branches = new Stack[(Int, Int, Double)]() // heapIdx, featureIdx, threshold
    val nodes = new DenseIntMap[DTree]() // heapIdx -> node
    while (!todo.isEmpty) {
      val (samples, usedFeatures, depth, heapIdx) = todo.pop()
      if (maxDepth < depth || samples.map(_._1).distinct.length == 1 || shouldStop(samples, depth)) {
        updateIntMap(nodes, heapIdx, DTLeaf(makeProportions(samples)))
      } else {
        val (featureIdx, threshold) = getSplittingFeatureAndThreshold(samples.toArray, usedFeatures, instanceWeights)
        @inline def pushChildWork(childStats: Seq[StatisticsType], childIdx: Int) =
          if (childStats.length > 0) todo.push((childStats, usedFeatures + ((featureIdx, threshold)), depth + 1, childIdx))
          else updateIntMap(nodes, childIdx, DTLeaf(makeProportions(samples)))
        val (statsWithFeature, statsWithoutFeature) = samples.partition(hasFeature(featureIdx, _, threshold))
        pushChildWork(statsWithFeature, child(leftChildren, heapIdx))
        pushChildWork(statsWithoutFeature, child(rightChildren, heapIdx))
        branches.push((heapIdx, featureIdx, threshold))
      }
    }
    // go up
    while (!branches.isEmpty) {
      val (heapIdx, featureIdx, threshold) = branches.pop()
      updateIntMap(nodes, heapIdx, DTBranch(nodes(child(leftChildren, heapIdx)), nodes(child(rightChildren, heapIdx)), featureIdx, threshold))
    }
    nodes(0)
  }

  private def score(s: StatisticsType, node: DTree): Double = node match {
    // returns -Infinity for prob 0, which is still uniform, so I guess its ok...
    case DTLeaf(proportions) => proportions.logpr(labelIndex(s))
    case DTBranch(yes, no, idx, threshold) => score(s, if (hasFeature(idx, s, threshold)) yes else no)
  }

  def label(s:StatisticsType, node: DTree): Int = node match {
    case DTLeaf(proportions) => proportions.maxIndex
    case DTBranch(yes, no, idx, threshold) => label(s, if (hasFeature(idx, s, threshold)) yes else no)
  }

  // crazy... this is actually faster in practice even for binary features, because we can skip
  // infogaining all the features that are uniform valued over the whole sample set!!
  private def getPossibleFeatureThresholds(stats: Array[StatisticsType]): Array[Array[Double]] = {
    val numFeatures = stats(0)._2.length
    val numSamples = stats.length
    val possibleThresholds = new Array[Array[Double]](numFeatures)
    var s = 0
    val featureValues = Array.fill(numFeatures)(ArrayBuilder.make[Double]())
    while (s < numSamples) {
      stats(s)._2 match {
        case sT: SparseTensor1 =>
          val sIndices = sT._indices
          val sValues = sT._values
          val len = sValues.length
          var i = 0
          while (i < len) {
            featureValues(sIndices(i)) += sValues(i)
            i += 1
          }
        case sT: SparseBinaryTensorLike1 =>
          val dom = sT.activeDomain1
          val len = dom.length
          val dArr = dom.asArray
          var i = 0
          while (i < len) {
            featureValues(dArr(i)) += 1.0
            i += 1
          }
        case sT => sT.foreachActiveElement((f, v) => featureValues(f) += v)
      }
      s += 1
    }
    var f = 0
    while (f < numFeatures) {
      val ab = featureValues(f)
      ab += 0.0
      val sorted = featureValues(f).result()
      java.util.Arrays.sort(sorted)
      val thresholds = ArrayBuilder.make[Double]()
      var last = sorted(0)
      var s = 0
      while (s < sorted.length) {
        val srt = sorted(s)
        if (last < srt) { thresholds += ((srt + last) / 2); last = srt }
        s += 1
      }
      possibleThresholds(f) = thresholds.result()
      f += 1
    }
    possibleThresholds
  }

  private def getSplittingFeatureAndThreshold(stats: Array[StatisticsType], usedFeatures: Set[(Int, Double)], instanceWeights: Option[Tensor1]): (Int, Double) = {
    val useInstanceWeights = instanceWeights.isDefined
    val instanceWeight = instanceWeights.getOrElse(null)
    val state = getPerNodeState(stats)
    val numSamples = stats.length
    val numFeatures = stats.head._2.length
    val possibleFeatureThresholds = getPossibleFeatureThresholds(stats)
    var featureIdx = 0
    var maxIdx = 0
    var maxThreshold = Double.NegativeInfinity
    var maxValue = Double.NegativeInfinity
    while (featureIdx < numFeatures) {
      val thresholds = possibleFeatureThresholds(featureIdx)
      var thresholdIdx = 0
      while (thresholdIdx < thresholds.length) {
        val threshold = thresholds(thresholdIdx)
        if (!usedFeatures((featureIdx, threshold))) {
          val proportionsWith = new DenseProportions1(numLabels)
          val proportionsWithout = new DenseProportions1(numLabels)
          var sampleIdx = 0
          while (sampleIdx < numSamples) {
            val props = if (hasFeature(featureIdx, stats(sampleIdx), threshold)) proportionsWith else proportionsWithout
            props.masses += (labelIndex(stats(sampleIdx)), if (!useInstanceWeights) 1.0 else instanceWeight(sampleIdx))
            sampleIdx += 1
          }
          if (proportionsWith.masses.massTotal > 0 && proportionsWithout.masses.massTotal > 0) {
            val infogain = evaluateSplittingCriteria(state, proportionsWith, proportionsWithout)
            if (infogain > maxValue) {
              maxValue = infogain
              maxIdx = featureIdx
              maxThreshold = threshold
            }
          }
        }
        thresholdIdx += 1
      }
      featureIdx += 1
    }
    (maxIdx, maxThreshold)
  }

  @inline def hasFeature(featureIdx: Int, s: StatisticsType, threshold: Double) = s._2(featureIdx) > threshold
  @inline def labelIndex(s: StatisticsType) = s._1.intValue
  @inline def makeProportions(stats: Seq[StatisticsType]): DenseProportions1 = {
    val labelProps = new DenseProportions1(numLabels)
    stats.foreach(s => labelProps.masses += (labelIndex(s), 1.0))
    labelProps
  }
}

// Provides default implementation of State and splitting criteria that ignores it, for strategies that do not require
// state to be stored to efficiently calculate the splitting feature
trait NoTrainingState[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  type State = Unit
  def getPerNodeState(stats: Seq[StatisticsType]): State = ()
  def evaluateSplittingCriteria(s: State, withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double =
    evaluateSplittingCriteria(withFeature, withoutFeature)
  def evaluateSplittingCriteria(withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double
}

trait InfoGainSplitting[S1 <: DiscreteVar, S2 <: DiscreteTensorVar]
  extends NoTrainingState[S1, S2] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  def evaluateSplittingCriteria(withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double = {
    // we're using information gain modulo the constant factor of base entropy
    val numFeatures = withFeature.masses.massTotal + withoutFeature.masses.massTotal
    val pctWith = withFeature.masses.massTotal / numFeatures
    val pctWithout = withoutFeature.masses.massTotal / numFeatures
    val infoGainMinusBaseEntropy = -(pctWith * withFeature.entropy + pctWithout * withoutFeature.entropy)
    infoGainMinusBaseEntropy
  }
}

trait GainRatioSplitting[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  type State = Double
  def getPerNodeState(stats: Seq[StatisticsType]): State =
    makeProportions(stats).entropy
  def evaluateSplittingCriteria(baseEntropy: State, withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double = {
    val numFeatures = withFeature.masses.massTotal + withoutFeature.masses.massTotal
    val pctWith = withFeature.masses.massTotal / numFeatures
    val pctWithout = withoutFeature.masses.massTotal / numFeatures
    val infoGain = baseEntropy - (pctWith * withFeature.entropy + pctWithout * withoutFeature.entropy)
    val intrinsicValue = -(pctWith * math.log(pctWith) / math.log(2) + pctWithout * math.log(pctWithout) / math.log(2))
    infoGain / intrinsicValue
  }
}

trait SampleSizeStopping[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  def minSampleSize: Int
  def shouldStop(stats: Seq[StatisticsType], depth: Int) = stats.size < minSampleSize
}

trait UniformLabelStopping[S1 <: DiscreteVar, S2 <: DiscreteTensorVar]
  extends SampleSizeStopping[S1, S2] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  val minSampleSize = 1
}

trait MaxDepthStopping[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  def maxDepth: Int
  def shouldStop(stats: Seq[StatisticsType], depth: Int) = depth > maxDepth
}

trait NoPruning[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>
  def prune(tree: DTree, pruningSet: Seq[StatisticsType]) = tree
}

trait ErrorBasedPruning[S1 <: DiscreteVar, S2 <: DiscreteTensorVar] {
  this: DecisionTreeTemplateWithStatistics2[S1, S2] =>

  def prune(tree: DTree, pruningSet: Seq[StatisticsType]): DTree = tree match {
    case l: DTLeaf => l
    case DTBranch(yes, no, featureIndex, threshold) =>
      val prunedSubtree = DTBranch(prune(yes, pruningSet), prune(no, pruningSet), featureIndex, threshold)
      val leafProps = new DenseProportions1(numLabels)
      prunedSubtree.leaves.foreach(l => leafProps.masses += l.proportions)
      val testLeaf = DTLeaf(leafProps)
      if (errorPct(testLeaf, pruningSet) >= errorPct(prunedSubtree, pruningSet)) testLeaf else prunedSubtree
  }

  def errorPct(node: DTree, stats: Seq[StatisticsType]): Double =
    stats.map(s => if (labelIndex(s) == label(s, node)) 1.0 else 0.0).sum / stats.length
}

class C45DecisionTreeTemplate[V1 <: DiscreteVar, V2 <: DiscreteTensorVar]
  (labelToFeatures: V1 => V2, labelDomain: DiscreteDomain, featureDomain: DiscreteTensorDomain)
  (implicit m1: Manifest[V1], m2: Manifest[V2])
  extends DecisionTreeTemplateWithStatistics2[V1, V2](labelToFeatures, labelDomain, featureDomain)
  with GainRatioSplitting[V1, V2]
  with SampleSizeStopping[V1, V2]
  with ErrorBasedPruning[V1, V2] {
  val minSampleSize = 4
}

class ID3DecisionTreeTemplate[V1 <: DiscreteVar, V2 <: DiscreteTensorVar]
  (labelToFeatures: V1 => V2, labelDomain: DiscreteDomain, featureDomain: DiscreteTensorDomain)
  (implicit m1: Manifest[V1], m2: Manifest[V2])
  extends DecisionTreeTemplateWithStatistics2[V1, V2](labelToFeatures, labelDomain, featureDomain)
  with InfoGainSplitting[V1, V2]
  with SampleSizeStopping[V1, V2]
  with NoPruning[V1, V2] {
  val minSampleSize = 4
}

class DecisionStumpTemplate[V1 <: DiscreteVar, V2 <: DiscreteTensorVar]
  (labelToFeatures: V1 => V2, labelDomain: DiscreteDomain, featureDomain: DiscreteTensorDomain)
  (implicit m1: Manifest[V1], m2: Manifest[V2])
  extends DecisionTreeTemplateWithStatistics2[V1, V2](labelToFeatures, labelDomain, featureDomain)
  with InfoGainSplitting[V1, V2]
  with MaxDepthStopping[V1, V2]
  with NoPruning[V1, V2] {
  val maxDepth = 0
}

