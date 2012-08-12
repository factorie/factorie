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

import la.Tensor
import collection.mutable.{ArrayBuilder, Stack, ArrayBuffer}

/** Statistics for factors who scores are the log-probability of
    label S1 given feature vector S2, according to a decision tree.
    @author Luke Vilnis */
trait DecisionTreeStatistics2Base[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] extends TensorStatistics2[S1, S2] {

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

  lazy val numLabels: Int = statisticsDomains._1.asInstanceOf[DiscreteDomain].size

  var decisionTree = None: Option[DTree]

  def splittingFeatures: Seq[Int] = {
    def inner(tree: DTree): Seq[Int] = tree match {
      case DTBranch(yes, no, idx, thresh) => idx +: (inner(yes) ++ inner(no))
      case _ => Seq.empty[Int]
    }
    decisionTree.map(inner(_)).flatten.toSeq
  }

  def train(stats: Iterable[StatisticsType], maxDepth: Int = 100, getInstanceWeight: Option[Int => Double] = None): Unit = {
    val tree =
      if (maxDepth < 1000) trainTree(stats.toSeq, 0, maxDepth, Set.empty[(Int, Double)], getInstanceWeight)
      else trainTreeNoMaxDepth(stats.toSeq, maxDepth, getInstanceWeight)
    println(tree)
    decisionTree = Some(tree)
  }

  def prune(pruningSet: Iterable[StatisticsType]): Unit =
    decisionTree = Some(prune(decisionTree.get, pruningSet.toSeq))

  override def score(s: StatisticsType): Double = score(s, decisionTree.get)

  def score(s1: S1, s2: S2): Double = score(Stat(s1, s2), decisionTree.get)

  def score(t: Tensor): Double = throw new Exception("??")

  private def trainTree(stats: Seq[StatisticsType], depth: Int, maxDepth: Int, usedFeatures: Set[(Int, Double)], getInstanceWeight: Option[Int => Double]): DTree = {
    if (maxDepth < depth || stats.map(_._1).distinct.length == 1 || shouldStop(stats, depth)) {
      DTLeaf(makeProportions(stats))
    } else {
      val (featureIdx, threshold) = getSplittingFeatureAndThreshold(stats.toArray, usedFeatures, getInstanceWeight)
      val (statsWithFeature, statsWithoutFeature) = stats.partition(hasFeature(featureIdx, _, threshold))
      @inline def trainChildTree(childStats: Seq[StatisticsType]): DTree =
        if (childStats.length > 0) trainTree(childStats, depth + 1, maxDepth, usedFeatures + ((featureIdx, threshold)), getInstanceWeight)
        else DTLeaf(makeProportions(stats))
      DTBranch(trainChildTree(statsWithFeature), trainChildTree(statsWithoutFeature), featureIdx, threshold)
    }
  }

  private def trainTreeNoMaxDepth(startingSamples: Seq[StatisticsType], maxDepth: Int, getInstanceWeight: Option[Int => Double]): DTree = {
    // Use arraybuffer as dense mutable int-indexed map - no IndexOutOfBoundsException, just expand to fit
    type DenseIntMap[T] = ArrayBuffer[T]
    def updateIntMap[@specialized T](ab: DenseIntMap[T], idx: Int, item: T, dfault: T = null.asInstanceOf[T]) = {
      if (ab.length <= idx) {ab.insertAll(ab.length, Iterable.fill(idx - ab.length + 1)(dfault)) }
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
        val (featureIdx, threshold) = getSplittingFeatureAndThreshold(samples.toArray, usedFeatures, getInstanceWeight)
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
    var f = 0
    while (f < numFeatures) {
      val featureValues = ArrayBuilder.make[Double]()
      var s = 0
      while (s < numSamples) {
        featureValues += stats(s)._2(f)
        s += 1
      }
      val sorted = featureValues.result()
      java.util.Arrays.sort(sorted)
      val thresholds = ArrayBuilder.make[Double]()
      var last = sorted(0)
      s = 0
      while (s < sorted.length) {
        if (last < sorted(s)) {thresholds += ((sorted(s) + last) / 2); last = sorted(s) }
        s += 1
      }
      possibleThresholds(f) = thresholds.result()
      f += 1
    }
    possibleThresholds
  }

  private def getSplittingFeatureAndThreshold(stats: Array[StatisticsType], usedFeatures: Set[(Int, Double)], getInstanceWeight: Option[Int => Double]): (Int, Double) = {
    val useInstanceWeights = getInstanceWeight.isDefined
    val instanceWeight = getInstanceWeight.getOrElse(null)
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
          if (proportionsWith.massTotal > 0 && proportionsWithout.massTotal > 0) {
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

  override def save(dirname: String, gzip: Boolean = false): Unit = {
    super.save(dirname, gzip)
    throw new Error("Not yet implemented")
  }
  override def load(dirname: String, gzip: Boolean = false): Unit = {
    super.load(dirname, gzip)
    throw new Error("Not yet implemented")
  }
}

// Provides default implementation of State and splitting criteria that ignores it, for strategies that do not require
// state to be stored to efficiently calculate the splitting feature
trait NoTrainingState[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  type State = Unit
  def getPerNodeState(stats: Seq[StatisticsType]): State = ()
  def evaluateSplittingCriteria(s: State, withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double =
    evaluateSplittingCriteria(withFeature, withoutFeature)
  def evaluateSplittingCriteria(withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double
}

trait InfoGainSplitting[S1 <: DiscreteValue, S2 <: DiscreteTensorValue]
  extends NoTrainingState[S1, S2] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  def evaluateSplittingCriteria(withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double = {
    // we're using information gain modulo the constant factor of base entropy
    val numFeatures = withFeature.massTotal + withoutFeature.massTotal
    val pctWith = withFeature.massTotal / numFeatures
    val pctWithout = withoutFeature.massTotal / numFeatures
    val infoGainMinusBaseEntropy = -(pctWith * withFeature.entropy + pctWithout * withoutFeature.entropy)
    infoGainMinusBaseEntropy
  }
}

trait GainRatioSplitting[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  type State = Double
  def getPerNodeState(stats: Seq[StatisticsType]): State =
    makeProportions(stats).entropy
  def evaluateSplittingCriteria(baseEntropy: State, withFeature: DenseProportions1, withoutFeature: DenseProportions1): Double = {
    val numFeatures = withFeature.massTotal + withoutFeature.massTotal
    val pctWith = withFeature.massTotal / numFeatures
    val pctWithout = withoutFeature.massTotal / numFeatures
    val infoGain = baseEntropy - (pctWith * withFeature.entropy + pctWithout * withoutFeature.entropy)
    val intrinsicValue = -(pctWith * math.log(pctWith) / math.log(2) + pctWithout * math.log(pctWithout) / math.log(2))
    infoGain / intrinsicValue
  }
}

trait SampleSizeStopping[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  def minSampleSize: Int
  def shouldStop(stats: Seq[StatisticsType], depth: Int) = stats.size < minSampleSize
}

trait UniformLabelStopping[S1 <: DiscreteValue, S2 <: DiscreteTensorValue]
  extends SampleSizeStopping[S1, S2] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  val minSampleSize = 1
}

trait MaxDepthStopping[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  def maxDepth: Int
  def shouldStop(stats: Seq[StatisticsType], depth: Int) = depth > maxDepth
}

trait NoPruning[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>
  def prune(tree: DTree, pruningSet: Seq[StatisticsType]) = tree
}

trait ErrorBasedPruning[S1 <: DiscreteValue, S2 <: DiscreteTensorValue] {
  this: DecisionTreeStatistics2Base[S1, S2] =>

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

trait C45DecisionTreeStatistics2[S1 <: DiscreteValue, S2 <: DiscreteTensorValue]
  extends DecisionTreeStatistics2Base[S1, S2]
  with GainRatioSplitting[S1, S2]
  with SampleSizeStopping[S1, S2]
  with ErrorBasedPruning[S1, S2] {
  val minSampleSize = 4
}

trait ID3DecisionTreeStatistics2[S1 <: DiscreteValue, S2 <: DiscreteTensorValue]
  extends DecisionTreeStatistics2Base[S1, S2]
  with InfoGainSplitting[S1, S2]
  with SampleSizeStopping[S1, S2]
  with NoPruning[S1, S2] {
  val minSampleSize = 4
}

trait StumpDecisionTreeStatistics2[S1 <: DiscreteValue, S2 <: DiscreteTensorValue]
  extends DecisionTreeStatistics2Base[S1, S2]
  with InfoGainSplitting[S1, S2]
  with MaxDepthStopping[S1, S2]
  with NoPruning[S1, S2] {
  val maxDepth = 0
}

/**A template for factors who scores are the log-probability of
    label S1 given feature vector S2, according to a decision tree.
    @author Andrew McCallum */
abstract class DecisionTreeTemplateWithStatistics2[S1 <: DiscreteVar, S2 <: DiscreteTensorVar](implicit m1: Manifest[S1], m2: Manifest[S2])
  extends Template2[S1, S2] {
  this: DecisionTreeStatistics2Base[S1#ValueType, S2#ValueType] =>
  def statistics(values: Values) = Stat(values._1, values._2)
  def train(labels: Iterable[S1]): Unit = train(labels.map(unroll1(_)).flatten.map(_.statistics: StatisticsType))
  def train(labels: Iterable[S1], getInstanceWeight: Int => Double): Unit =
    train(labels.map(unroll1(_)).flatten.map(_.statistics: StatisticsType), getInstanceWeight = Some(getInstanceWeight))
}

class ID3DecisionTreeTemplate[L <: DiscreteVar, F <: DiscreteTensorVar](
  val labelToFeatures: L => F, val labelDomain: DiscreteDomain, val featureDomain: DiscreteTensorDomain)(implicit m1: Manifest[L], m2: Manifest[F])
  extends DecisionTreeTemplateWithStatistics2[L, F]()(m1, m2) with ID3DecisionTreeStatistics2[DiscreteValue, F#ValueType] {
  def statisticsDomains = Tuple(labelDomain, featureDomain)
  def unroll1(label: L) = Factor(label, labelToFeatures(label))
  def unroll2(features: F) = throw new Error("Cannot unroll from feature variables.")
}

class DecisionStumpTemplate[L <: DiscreteVar, F <: DiscreteTensorVar](
  val labelToFeatures: L => F, val labelDomain: DiscreteDomain, val featureDomain: DiscreteTensorDomain)(implicit m1: Manifest[L], m2: Manifest[F])
  extends DecisionTreeTemplateWithStatistics2[L, F]()(m1, m2) with StumpDecisionTreeStatistics2[DiscreteValue, F#ValueType] {
  def statisticsDomains = Tuple(labelDomain, featureDomain)
  def unroll1(label: L) = Factor(label, labelToFeatures(label))
  def unroll2(features: F) = throw new Error("Cannot unroll from feature variables.")
}
