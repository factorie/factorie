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
package cc.factorie.app.classify.backend

import cc.factorie._
import cc.factorie.la._
import cc.factorie.model.Template2
import cc.factorie.util.StoreFetchCubbie
import cc.factorie.variable.{HashFeatureVectorVariable, LabeledMutableDiscreteVar, TensorVar}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

class DecisionTreeMulticlassTrainer[Label](treeTrainer: DecisionTreeTrainer = new ID3DecisionTreeTrainer)
  (implicit random: Random)
  extends MulticlassClassifierTrainer[DecisionTreeMulticlassClassifier] {

  def baseTrain(classifier: DecisionTreeMulticlassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (DecisionTreeMulticlassClassifier) => Unit) {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(classifier.labelSize, _))).zip(weights).map({
      case ((feat, label), weight) => DecisionTreeTrainer.Instance(feat, label, weight)
    })
    val dtree = treeTrainer.train(instances)
    classifier.tree = dtree
    evaluate(classifier)
  }
  def newModel(featureSize: Int, labelSize: Int) = new DecisionTreeMulticlassClassifier(null, labelSize)
}

// TODO this threading stuff makes it non-deterministic, fix -luke
class RandomForestMulticlassTrainer(numTrees: Int, numFeaturesToUse: Int, numInstancesToSample: Int, maxDepth: Int = 25,
  useParallel: Boolean = true, numThreads: Int = Runtime.getRuntime.availableProcessors(), treeTrainer: DecisionTreeTrainer = new ID3DecisionTreeTrainer)
  (implicit random: Random)
  extends MulticlassClassifierTrainer[RandomForestMulticlassClassifier] {

  def baseTrain(classifier: RandomForestMulticlassClassifier, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: (RandomForestMulticlassClassifier) => Unit) {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(classifier.labelSize, _))).zip(weights).map({
      case ((feat, label), weight) => DecisionTreeTrainer.Instance(feat, label, weight)
    })
    val trees = util.Threading.parMap(0 until numTrees, numThreads)(_ => {
      val bootstrap = (0 until numInstancesToSample).map(_ => instances(random.nextInt(instances.length)))
      treeTrainer.maxDepth = maxDepth // TODO ugh but otherwise we can't override it in the trait - just use maxdepth stopping like before -luke
      treeTrainer.train(bootstrap, numFeaturesToUse = numFeaturesToUse)
    })
    classifier.trees = trees.toSeq
    evaluate(classifier)
  }

  def newModel(featureSize: Int, labelSize: Int) = new RandomForestMulticlassClassifier(null, labelSize)
}

class RandomForestMulticlassClassifier(var trees: Seq[DTree], val labelSize: Int) extends MulticlassClassifier[Tensor1] {
  self =>
  def predict(features: Tensor1) = {
    // TODO why not train an SVM on these predictions instead of just doing majority voting? -luke
    val res = trees.map(t => DTree.score(features, t)).map(t => {t.maxNormalize(); t}).reduce(_ + _)
    res.normalize()
    // FIXME these should be logged along with the regular decision tree scores
    res
  }
  def asTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: ClassTag[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2(l2f, this)
}

class DecisionTreeMulticlassClassifier(var tree: DTree, val labelSize: Int) extends MulticlassClassifier[Tensor1] {
  def predict(features: Tensor1) =
    DTree.score(features, tree)
  def asTemplate[T <: LabeledMutableDiscreteVar](l2f: T => TensorVar)(implicit ml: ClassTag[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2[T](l2f, this)
}

class RandomForestCubbie extends StoreFetchCubbie[RandomForestMulticlassClassifier] {
  val trees = CubbieListSlot[TreeNodeCubbie]("trees", () => new TreeNodeCubbie)
  val labelSize = IntSlot("labelSize")
  trees := Seq()
  labelSize := 0
  def store(c: RandomForestMulticlassClassifier): Unit = {
    trees := c.trees.map(t => {val c = new TreeNodeCubbie; c.store(t); c})
    labelSize := c.labelSize
  }
  def fetch(): RandomForestMulticlassClassifier = new RandomForestMulticlassClassifier(trees.value.map(_.fetch()), labelSize.value)
}

class TreeNodeCubbie extends StoreFetchCubbie[DTree] {
  // Need to store leaves and branches in the same sort of cubbie since we need to create the cubbies before knowing what's in them
  val isLeaf = BooleanSlot("isLeaf")
  val feature = IntSlot("feature")
  val threshold = DoubleSlot("threshold")
  val left = CubbieSlot[TreeNodeCubbie]("left", () => new TreeNodeCubbie)
  val right = CubbieSlot[TreeNodeCubbie]("right", () => new TreeNodeCubbie)
  val tensor = TensorSlot("tensor")
  // FIXME I really wish we didn't have to do this to build the slots eagerly
  threshold := 0
  feature := 0
  isLeaf := false
  def store(t: DTree): Unit = t match {
    case t: DTBranch =>
      isLeaf := false
      feature := t.featureIndex
      threshold := t.threshold
      left := toCubbie(t.yes)
      right := toCubbie(t.no)
    case t: DTLeaf =>
      isLeaf := true
      tensor := t.pred
  }
  def fetch(): DTree =
    if (!isLeaf.value) DTBranch(left.value.fetch(), right.value.fetch(), feature.value, threshold.value)
    else DTLeaf(tensor.value.asInstanceOf[Tensor1])
  private def toCubbie(dt: DTree): TreeNodeCubbie = {
    val c = new TreeNodeCubbie; c.store(dt); c
  }
}

class DecisionTreeCubbie extends StoreFetchCubbie[DecisionTreeMulticlassClassifier] {
  val tree = CubbieSlot[TreeNodeCubbie]("tree", () => new TreeNodeCubbie)
  val labelSize = IntSlot("labelSize")
  def store(t: DecisionTreeMulticlassClassifier): Unit = {
    val bc = new TreeNodeCubbie
    bc.store(t.tree)
    labelSize := t.labelSize
    tree := bc
  }
  def fetch(): DecisionTreeMulticlassClassifier =
    new DecisionTreeMulticlassClassifier(tree.value.fetch(), labelSize.value)
}

// TODO add regression tree -luke

class RegressionTreeTrainer
  extends DecisionTreeTrainer
  with TensorSumSqDiagStatsAndLabels
  with DiagonalCovarianceSplitting
  with SampleSizeStopping
  with RMSEBasedPruning {
  var minSampleSize = 4
}

class C45DecisionTreeTrainer
  extends DecisionTreeTrainer
  with TensorSumStatsAndLabels
  with GainRatioSplitting
  with SampleSizeStopping
  with AccuracyBasedPruning {
  val minSampleSize = 4
}

class DecisionStumpTrainer
  extends DecisionTreeTrainer
  with TensorSumStatsAndLabels
  with GainRatioSplitting
  with SampleSizeStopping
  with NoPruning {
  maxDepth = 1
  val minSampleSize = 4
}

class ID3DecisionTreeTrainer
  extends DecisionTreeTrainer
  with TensorSumStatsAndLabels
  with InfoGainSplitting
  with SampleSizeStopping
  with NoPruning {
  val minSampleSize = 4
}

class CARTDecisionTreeTrainer
  extends DecisionTreeTrainer
  with TensorSumStatsAndLabels
  with GiniSplitting
  with SampleSizeStopping
  with NoPruning {
  val minSampleSize = 4
}

object DecisionTreeTrainer {
  case class Instance(feats: Tensor1, label: Tensor1, weight: Double)
  // helper to do sparse sampling of features - instead of sorting all indices by some random number and picking the first N,
  // for each feature we see, compute a hash + per-node salt, mod into the domain and if it falls in the first N then we include it...
  def shouldIncludeFeature(featIdx: Int, salt: Int, domainSize: Int, numFeaturesToUse: Int): Boolean =
    numFeaturesToUse == -1 || HashFeatureVectorVariable.index(featIdx + salt, domainSize) < numFeaturesToUse
}

trait DecisionTreeTrainer {
  this: DTreeBucketStats =>

  type State
  def getBucketState(labels: Iterable[Instance]): State

  def +=(left: BucketStats, right: BucketStats): Unit
  def -=(left: BucketStats, right: BucketStats): Unit
  def accumulate(stats: BucketStats, inst: Instance): Unit

  def prune(tree: DTree, pruningSet: Seq[Instance]): DTree

  def getEmptyBucketStats(inst: Instance): BucketStats
  def getBucketStats(labels: Iterable[Instance]): BucketStats
  def getPrediction(stats: BucketStats): Label
  def makeLeaf(stats: BucketStats): DTree

  def samePred(labels: Seq[Label]): Boolean

  def evaluateSplittingCriteria(s: State, withStats: BucketStats, withoutStats: BucketStats): Double

  // what's a good value for this?
  var maxDepth = 1000

  def shouldStop(stats: Seq[Instance], depth: Int): Boolean = false
  def getBucketPrediction(labels: Seq[Instance]): Label = getPrediction(getBucketStats(labels))

  def train(trainInstances: Seq[Instance], pruneInstances: Seq[Instance] = Nil, numFeaturesToUse: Int = -1)(implicit rng: Random): DTree = {
    val tree =
      if (maxDepth <= 1000) trainTree(trainInstances.toSeq, 0, maxDepth, Set.empty[(Int, Double)], numFeaturesToUse)
      else trainTreeNoMaxDepth(trainInstances.toSeq, maxDepth, numFeaturesToUse)
    if (pruneInstances.size == 0) tree else prune(tree, pruneInstances)
  }

  private def trainTree(stats: Seq[Instance], depth: Int, maxDepth: Int, usedFeatures: Set[(Int, Double)], numFeaturesToChoose: Int)(implicit rng: Random): DTree = {
    // FIXME this is an ugly way to figure out if the bucket has only one label -luke
    if (maxDepth < depth || samePred(stats.map(_.label)) || shouldStop(stats, depth)) {
      makeLeaf(getBucketStats(stats))
    } else {
      getSplittingFeatureAndThreshold(stats.toArray, usedFeatures, numFeaturesToChoose) match {
        case Some((featureIdx, threshold)) =>
          val (statsWithFeature, statsWithoutFeature) = stats.partition(s => hasFeature(featureIdx, s.feats, threshold))
          @inline def trainChildTree(childStats: Seq[Instance]): DTree =
            if (childStats.length > 0) trainTree(childStats, depth + 1, maxDepth, usedFeatures + ((featureIdx, threshold)), numFeaturesToChoose)
            else makeLeaf(getBucketStats(stats))
          DTBranch(trainChildTree(statsWithFeature), trainChildTree(statsWithoutFeature), featureIdx, threshold)
        case None => makeLeaf(getBucketStats(stats))
      }
    }
  }

  private def trainTreeNoMaxDepth(startingSamples: Seq[Instance], maxDepth: Int, numFeaturesToChoose: Int)(implicit rng: Random): DTree = {
    // Use arraybuffer as dense mutable int-indexed map - no IndexOutOfBoundsException, just expand to fit
    type DenseIntMap[T] = ArrayBuffer[T]
    def updateIntMap[@specialized T](ab: DenseIntMap[T], idx: Int, item: T, dfault: T = null.asInstanceOf[T]) = {
      if (ab.length <= idx) {ab.insertAll(ab.length, Iterable.fill(idx - ab.length + 1)(dfault))}
      ab.update(idx, item)
    }
    var currentChildId = 0 // get childIdx or create one if it's not there already
    def child(childMap: DenseIntMap[Int], heapIdx: Int) =
      if (childMap.length > heapIdx && childMap(heapIdx) != -1) childMap(heapIdx)
      else {currentChildId += 1; updateIntMap(childMap, heapIdx, currentChildId, -1); currentChildId}
    // go down
    val leftChildren, rightChildren = new DenseIntMap[Int]() // heapIdx -> childHeapIdx
    val todo = mutable.Stack((startingSamples, Set.empty[(Int, Double)], 0, 0)) // samples, usedFeatures, depth, heapIdx
    val branches = new mutable.Stack[(Int, Int, Double)]() // heapIdx, featureIdx, threshold
    val nodes = new DenseIntMap[DTree]() // heapIdx -> node
    while (!todo.isEmpty) {
      val (samples, usedFeatures, depth, heapIdx) = todo.pop()
      // FIXME this "distinct" doesn't work
      if (maxDepth < depth || samePred(samples.map(_.label)) || shouldStop(samples, depth)) {
        updateIntMap(nodes, heapIdx, makeLeaf(getBucketStats(samples)))
      } else {
        getSplittingFeatureAndThreshold(samples.toArray, usedFeatures, numFeaturesToChoose) match {
          case Some((featureIdx, threshold)) =>
            @inline def pushChildWork(childStats: Seq[Instance], childIdx: Int) =
              if (childStats.length > 0) todo.push((childStats, usedFeatures + ((featureIdx, threshold)), depth + 1, childIdx))
              else updateIntMap(nodes, childIdx, makeLeaf(getBucketStats(samples)))
            val (statsWithFeature, statsWithoutFeature) = samples.partition(s => hasFeature(featureIdx, s.feats, threshold))
            pushChildWork(statsWithFeature, child(leftChildren, heapIdx))
            pushChildWork(statsWithoutFeature, child(rightChildren, heapIdx))
            branches.push((heapIdx, featureIdx, threshold))
          case None =>
            updateIntMap(nodes, heapIdx, makeLeaf(getBucketStats(samples)))
        }
      }
    }
    // go up
    while (!branches.isEmpty) {
      val (heapIdx, featureIdx, threshold) = branches.pop()
      updateIntMap(nodes, heapIdx, DTBranch(nodes(child(leftChildren, heapIdx)), nodes(child(rightChildren, heapIdx)), featureIdx, threshold))
    }
    nodes(0)
  }

  private def getSplittingFeatureAndThreshold(instances: Array[Instance], usedFeatures: Set[(Int, Double)], numFeaturesToChoose: Int)(implicit rng: Random): Option[(Int, Double)] = {
    val possibleFeatureThresholds = getPossibleFeatureThresholds(instances, numFeaturesToChoose)
    val criteria = evaluateSplittingCriteria(instances, possibleFeatureThresholds)
    var maxValue = Double.NegativeInfinity
    var maxFeature = 0
    var maxThreshold = 0.0
    for ((f, thresholds) <- possibleFeatureThresholds.toSeq) {
      val criteriaValues = criteria(f)
      val numThresholds = thresholds.length
      var t = 0
      while (t < numThresholds) {
        val thresh = thresholds(t)
        val crit = criteriaValues(t)
        if (crit > maxValue) {
          maxValue = crit
          maxFeature = f
          maxThreshold = thresh
        }
        t += 1
      }
    }
    if (maxValue > Double.NegativeInfinity) Some((maxFeature, maxThreshold)) else None
  }

  def evaluateSplittingCriteria(instances: Seq[Instance], possibleFeatureThresholds: mutable.HashMap[Int, Array[Double]]): mutable.HashMap[Int, Array[Double]] = {
    val headInst = instances.head
    val allStats = getBucketStats(instances)
    val withFeatureStats = new mutable.HashMap[Int, Array[Any]].asInstanceOf[mutable.HashMap[Int, Array[BucketStats]]]
    val costReductions = new mutable.HashMap[Int, Array[Double]]

    for ((f, thresh) <- possibleFeatureThresholds.toSeq) {
      withFeatureStats(f) = Array.fill[Any](thresh.length)(getEmptyBucketStats(headInst)).asInstanceOf[Array[BucketStats]]
      costReductions(f) = Array.fill(thresh.length)(0.0)
    }

    for (inst <- instances)
      inst.feats.foreachActiveElement((i, v) => {
        possibleFeatureThresholds.get(i) match {
          case Some(thresholds) =>
            val split = thresholds.length - thresholds.count(fv => fv > v) - 1
            // if we are lower than all the thresholds, then we don't need to accumulate stats for this split
            if (split > -1) {
              val stats = withFeatureStats(i)(split)
              accumulate(stats, inst)
            }
          case None =>
        }
      })

    val baseEntropy = getBucketState(instances)

    for ((f, thresholds) <- possibleFeatureThresholds.toSeq) {
      for (t <- 0 until thresholds.length) {
        val withStats = withFeatureStats(f)(t)
        val withoutStats = getEmptyBucketStats(headInst)
        +=(withoutStats, allStats)
        -=(withoutStats, withStats)
        costReductions(f)(t) = evaluateSplittingCriteria(baseEntropy, withStats, withoutStats)
      }
    }
    costReductions
  }

  @inline def hasFeature(featureIdx: Int, feats: Tensor1, threshold: Double): Boolean = feats(featureIdx) > threshold

  private def getPossibleFeatureThresholds(stats: Array[Instance], numFeaturesToChoose: Int)(implicit rng: Random): mutable.HashMap[Int, Array[Double]] = {
    val numFeatures = stats(0).feats.length
    val numInstances = stats.length
    val salt = rng.nextInt(numFeatures)
    val splits = new mutable.HashMap[Int, Array[Double]]
    val binary = stats(0).feats.isInstanceOf[SparseBinaryTensor]
    if (binary) {
      var i = 0
      while (i < numInstances) {
        val inst = stats(i).feats
        inst.foreachActiveElement((i, v) => {
          splits.get(i) match {
            case None =>
              if (DecisionTreeTrainer.shouldIncludeFeature(i, salt, numFeatures, numFeaturesToChoose))
                splits(i) = Array[Double](0.5)
            case Some(_) =>
          }
        })
        i += 1
      }
      return splits
    }
    var s = 0
    val splitBuilders = new mutable.HashMap[Int, mutable.ArrayBuilder[Double]]
    while (s < numInstances) {
      stats(s).feats match {
        case sT: SparseIndexedTensor =>
          val len = sT.activeDomainSize
          val sIndices = sT._indices
          val sValues = sT._values
          var i = 0
          while (i < len) {
            val idx = sIndices(i)
            splitBuilders.get(idx) match {
              case None =>
                if (DecisionTreeTrainer.shouldIncludeFeature(idx, salt, numFeatures, numFeaturesToChoose)) {
                  splitBuilders(idx) = mutable.ArrayBuilder.make[Double]()
                  splitBuilders(idx) += sValues(i)
                }
              case Some(_) => splitBuilders(idx) += sValues(i)

            }
            i += 1
          }
        case sT => sT.foreachActiveElement((f, v) => {
          splitBuilders.get(f) match {
            case None =>
              if (DecisionTreeTrainer.shouldIncludeFeature(f, salt, numFeatures, numFeaturesToChoose)) {
                splitBuilders(f) = mutable.ArrayBuilder.make[Double]()
                splitBuilders(f) += v
              }
            case Some(_) => splitBuilders(f) += v
          }
        })
      }
      s += 1
    }
    for ((f, ab) <- splitBuilders.toSeq) {
      ab += 0.0
      val sorted = splitBuilders(f).result()
      java.util.Arrays.sort(sorted)
      val thresholds = mutable.ArrayBuilder.make[Double]()
      var last = sorted(0)
      var s = 0
      while (s < sorted.length) {
        val srt = sorted(s)
        if (last < srt) {thresholds += (srt + last) / 2; last = srt}
        s += 1
      }
      splits(f) = thresholds.result()
    }
    splits
  }
}

// TODO full covariance splitting requires a determinant - make a JBLAS-backed tensor?

trait DiagonalCovarianceSplitting {
  this: DecisionTreeTrainer with TensorSumSqDiagStatsAndLabels =>
  type State = Double
  // TODO add some criterion for stopping splitting when std dev gets low enough
  def samePred(labels: Seq[Label]): Boolean = false
  def getBucketState(instances: Iterable[Instance]): Double = getStdDev(getBucketStats(instances))
  def getStdDev(stats: BucketStats): Double = {
    // TODO fix this it is quite numerically unstable -luke
    val diag = stats.sumSq / stats.mult - (stats.sum / stats.mult outer stats.sum / stats.mult).asInstanceOf[Tensor2].diag
    val variance = diag.asArray.product
    math.sqrt(variance)
  }
  def evaluateSplittingCriteria(baseStdDev: Double, withFeature: MutableBucketStats, withoutFeature: MutableBucketStats): Double = {
    if (!(withFeature.mult > 0.0 && withoutFeature.mult > 0.0)) return Double.NegativeInfinity
    val numInstances = withFeature.mult + withoutFeature.mult
    val pctWith = withFeature.mult * 1.0 / numInstances
    val pctWithout = withoutFeature.mult * 1.0 / numInstances
    val infoGain = baseStdDev - (pctWith * getStdDev(withFeature) + pctWithout * getStdDev(withoutFeature))
    infoGain
  }
}

trait GiniSplitting {
  this: DecisionTreeTrainer with TensorSumStatsAndLabels =>
  type State = Double
  def samePred(labels: Seq[Label]): Boolean = labels.map(_.maxIndex).distinct.length == 1
  def getBucketState(instances: Iterable[Instance]): Double = getGini(getBucketStats(instances))
  def getGini(stats: BucketStats): Double = (stats.sum / stats.mult).toSeq.foldLeft(0.0)((acc, p) => acc + p * (1 - p))
  def evaluateSplittingCriteria(baseGini: Double, withFeature: MutableBucketStats, withoutFeature: MutableBucketStats): Double = {
    if (!(withFeature.mult > 0.0 && withoutFeature.mult > 0.0)) return Double.NegativeInfinity
    val numInstances = withFeature.mult + withoutFeature.mult
    val pctWith = withFeature.mult * 1.0 / numInstances
    val pctWithout = withoutFeature.mult * 1.0 / numInstances
    baseGini - (pctWith * getGini(withFeature) + pctWithout * getGini(withoutFeature))
  }
}

trait InfoGainSplitting {
  this: DecisionTreeTrainer with TensorSumStatsAndLabels =>
  type State = Double
  def samePred(labels: Seq[Label]): Boolean = labels.map(_.maxIndex).distinct.length == 1
  def getBucketState(instances: Iterable[Instance]): Double = getEntropy(getBucketStats(instances))
  def getEntropy(stats: BucketStats): Double = (stats.sum / stats.mult).entropy
  def evaluateSplittingCriteria(baseEntropy: Double, withFeature: MutableBucketStats, withoutFeature: MutableBucketStats): Double = {
    if (!(withFeature.mult > 0.0 && withoutFeature.mult > 0.0)) return Double.NegativeInfinity
    val numInstances = withFeature.mult + withoutFeature.mult
    val pctWith = withFeature.mult * 1.0 / numInstances
    val pctWithout = withoutFeature.mult * 1.0 / numInstances
    val infoGain = baseEntropy - (pctWith * getEntropy(withFeature) + pctWithout * getEntropy(withoutFeature))
    infoGain
  }
}

// FIXME this is bugged (at least it's giving terrible results) - make sure we're not dividing by zero, etc -luke
trait GainRatioSplitting {
  this: DecisionTreeTrainer with TensorSumStatsAndLabels =>
  type State = Double
  def samePred(labels: Seq[Label]): Boolean = labels.map(_.maxIndex).distinct.length == 1
  def getBucketState(instances: Iterable[Instance]): Double = getEntropy(getBucketStats(instances))
  def getEntropy(stats: BucketStats): Double = (stats.sum / stats.mult).entropy
  def evaluateSplittingCriteria(baseEntropy: Double, withFeature: MutableBucketStats, withoutFeature: MutableBucketStats): Double = {
    if (!(withFeature.mult > 0.0 && withoutFeature.mult > 0.0)) return Double.NegativeInfinity
    val numInstances = withFeature.mult + withoutFeature.mult
    val pctWith = withFeature.mult * 1.0 / numInstances
    val pctWithout = withoutFeature.mult * 1.0 / numInstances
    val infoGain = baseEntropy - (pctWith * getEntropy(withFeature) + pctWithout * getEntropy(withoutFeature))
    val intrinsicValue = -(pctWith * math.log(pctWith) / math.log(2) + pctWithout * math.log(pctWithout) / math.log(2))
    infoGain / intrinsicValue
  }
}

trait SampleSizeStopping {
  this: DecisionTreeTrainer with DTreeBucketStats =>
  def minSampleSize: Int
  override def shouldStop(stats: Seq[Instance], depth: Int) = stats.size < minSampleSize
}

// Since we always check for uniform labels in the main algo anyhow, this does very little
trait UniformLabelStopping
  extends SampleSizeStopping {
  this: DecisionTreeTrainer with DTreeBucketStats =>
  val minSampleSize = 1
}

trait NoPruning {
  this: DecisionTreeTrainer with DTreeBucketStats =>
  def prune(tree: DTree, pruningSet: Seq[Instance]) = tree
}

trait RMSEBasedPruning {
  this: DecisionTreeTrainer with DTreeBucketStats =>
  def prune(tree: DTree, pruningSet: Seq[Instance]): DTree = tree match {
    case l: DTLeaf => l
    case DTBranch(yes, no, featureIndex, threshold) =>
      val prunedSubtree = DTBranch(prune(yes, pruningSet), prune(no, pruningSet), featureIndex, threshold)
      val leafProps = prunedSubtree.leaves.map(_.pred).reduce(_ + _)
      leafProps /= prunedSubtree.leaves.size
      val testLeaf = DTLeaf(leafProps)
      if (RMSE(testLeaf, pruningSet) >= RMSE(prunedSubtree, pruningSet)) testLeaf else prunedSubtree
  }
  def RMSE(node: DTree, stats: Seq[Instance]): Double =
    math.sqrt(stats.map(s => (s.label - label(s.feats, node)).twoNormSquared).sum * 1.0 / stats.length)
  def label(feats: Tensor1, node: DTree): Tensor1 = node match {
    case DTLeaf(proportions) => proportions
    case DTBranch(yes, no, idx, threshold) => label(feats, if (hasFeature(idx, feats, threshold)) yes else no)
  }
}

trait AccuracyBasedPruning {
  this: DecisionTreeTrainer with DTreeBucketStats =>
  def prune(tree: DTree, pruningSet: Seq[Instance]): DTree = tree match {
    case l: DTLeaf => l
    case DTBranch(yes, no, featureIndex, threshold) =>
      val prunedSubtree = DTBranch(prune(yes, pruningSet), prune(no, pruningSet), featureIndex, threshold)
      val leafProps = prunedSubtree.leaves.map(_.pred).reduce(_ + _)
      leafProps /= prunedSubtree.leaves.size
      val testLeaf = DTLeaf(leafProps)
      if (errorPct(testLeaf, pruningSet) >= errorPct(prunedSubtree, pruningSet)) testLeaf else prunedSubtree
  }
  def errorPct(node: DTree, stats: Seq[Instance]): Double =
    stats.count(s => s.label.maxIndex == label(s.feats, node)) * 1.0 / stats.length
  def label(feats: Tensor1, node: DTree): Int = node match {
    case DTLeaf(proportions) => proportions.maxIndex
    case DTBranch(yes, no, idx, threshold) => label(feats, if (hasFeature(idx, feats, threshold)) yes else no)
  }
}

trait DTreeBucketStats {
  type BucketStats
  type Instance = DecisionTreeTrainer.Instance
  type Label = Tensor1
  def getEmptyBucketStats(inst: Instance): BucketStats
  def getBucketStats(labels: Iterable[Instance]): BucketStats
  def +=(left: BucketStats, right: BucketStats): Unit
  def -=(left: BucketStats, right: BucketStats): Unit
  def accumulate(stats: BucketStats, inst: Instance): Unit
  def getPrediction(stats: BucketStats): Label
  def makeLeaf(stats: BucketStats): DTree
}

trait TensorSumSqFullStatsAndLabels extends DTreeBucketStats {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  class MutableBucketStats(size: Int) {
    val sum = new DenseTensor1(size)
    val sumSq = new DenseTensor2(size, size)
    var mult: Double = 0.0
  }
  // weighted avg and weighted count
  def getEmptyBucketStats(inst: Instance): BucketStats = new MutableBucketStats(inst.label.size)
  def getBucketStats(labels: Iterable[Instance]): BucketStats = {
    val numLabels = labels.head.label.length
    val stats = new MutableBucketStats(numLabels)
    labels.foldLeft(stats)((acc, el) => {
      accumulate(acc, el)
      acc
    })
  }
  def +=(left: BucketStats, right: BucketStats): Unit = {
    left.sum += right.sum
    left.sumSq += right.sumSq
    left.mult += right.mult
  }
  def -=(left: BucketStats, right: BucketStats): Unit = {
    left.sum -= right.sum
    left.sumSq -= right.sumSq
    val leftSumArr = left.sum.asArray
    val leftSumSqArr = left.sum.asArray
    var i = 0; val len = leftSumArr.length
    while (i < len) {
      if (leftSumArr(i) < 0.0) leftSumArr(i) = 0.0
      if (leftSumSqArr(i) < 0.0) leftSumSqArr(i) = 0.0
      i += 1
    }
    left.mult -= right.mult
  }
  def accumulate(stats: BucketStats, inst: Instance): Unit = {
    val instWeight = inst.weight
    stats.sum += (inst.label, instWeight)
    stats.sumSq += ((inst.label outer inst.label).asInstanceOf[Tensor2], instWeight)
    stats.mult += instWeight
  }
  def getPrediction(stats: BucketStats): Label = stats.sum / stats.mult
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(getPrediction(stats))
}

trait TensorSumSqDiagStatsAndLabels extends DTreeBucketStats {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  class MutableBucketStats(size: Int) {
    val sum = new DenseTensor1(size)
    val sumSq = new DenseTensor1(size)
    var mult: Double = 0.0
  }
  // weighted avg and weighted count
  def getEmptyBucketStats(inst: Instance): BucketStats = new MutableBucketStats(inst.label.size)
  def getBucketStats(labels: Iterable[Instance]): BucketStats = {
    val numLabels = labels.head.label.length
    val stats = new MutableBucketStats(numLabels)
    labels.foldLeft(stats)((acc, el) => {
      accumulate(acc, el)
      acc
    })
  }
  def +=(left: BucketStats, right: BucketStats): Unit = {
    left.sum += right.sum
    left.sumSq += right.sumSq
    left.mult += right.mult
  }
  def -=(left: BucketStats, right: BucketStats): Unit = {
    left.sum -= right.sum
    left.sumSq -= right.sumSq
    val leftSumArr = left.sum.asArray
    val leftSumSqArr = left.sum.asArray
    var i = 0; val len = leftSumArr.length
    while (i < len) {
      if (leftSumArr(i) < 0.0) leftSumArr(i) = 0.0
      if (leftSumSqArr(i) < 0.0) leftSumSqArr(i) = 0.0
      i += 1
    }
    left.mult -= right.mult
  }
  def accumulate(stats: BucketStats, inst: Instance): Unit = {
    val instWeight = inst.weight
    stats.sum += (inst.label, instWeight)
    stats.sumSq += ((inst.label outer inst.label).asInstanceOf[Tensor2].diag, instWeight)
    stats.mult += instWeight
  }
  def getPrediction(stats: BucketStats): Label = stats.sum / stats.mult
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(getPrediction(stats))
}

trait TensorSumStatsAndLabels extends DTreeBucketStats {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  class MutableBucketStats(size: Int) {
    val sum = new DenseTensor1(size)
//    val sumSq = new DenseTensor1(size)
    var mult: Double = 0.0
  }
  // weighted avg and weighted count
  def getEmptyBucketStats(inst: Instance): BucketStats = new MutableBucketStats(inst.label.size)
  def getBucketStats(labels: Iterable[Instance]): BucketStats = {
    val numLabels = labels.head.label.length
    val stats = new MutableBucketStats(numLabels)
    labels.foldLeft(stats)((acc, el) => {
      accumulate(acc, el)
      acc
    })
  }
  def +=(left: BucketStats, right: BucketStats): Unit = {
    left.sum += right.sum
    left.mult += right.mult
  }
  def -=(left: BucketStats, right: BucketStats): Unit = {
    left.sum -= right.sum
    val withoutStatsArr = left.sum.asArray
    var i = 0; val len = withoutStatsArr.length
    while (i < len) {if (withoutStatsArr(i) < 0.0) withoutStatsArr(i) = 0.0; i += 1}
    left.mult -= right.mult
  }
  def accumulate(stats: BucketStats, inst: Instance): Unit = {
    val instWeight = inst.weight
    stats.sum += (inst.label, instWeight)
    stats.mult += instWeight
  }
  def getPrediction(stats: BucketStats): Label = stats.sum / stats.mult
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(getPrediction(stats))
}

// TODO add more versions of the tree that can do labels that aren't tensors
sealed trait DTree {
  lazy val leaves: Set[DTLeaf] = this match {
    case l: DTLeaf => Set(l)
    case DTBranch(yes, no, _, _) => yes.leaves ++ no.leaves
  }
}

case class DTBranch(yes: DTree, no: DTree, featureIndex: Int, threshold: Double) extends DTree
case class DTLeaf(pred: Tensor1) extends DTree

object DTree {
  @inline def hasFeature(featureIdx: Int, features: Tensor1, threshold: Double): Boolean = features(featureIdx) > threshold
  def score(features: Tensor1, node: DTree): Tensor1 = node match {
    // returns -Infinity for prob 0, which is still uniform, so I guess its ok...
    case DTLeaf(logProbs) => logProbs // todo take the log here
    case DTBranch(yes, no, idx, threshold) => score(features, if (hasFeature(idx, features, threshold)) yes else no)
  }
}

// TODO add GINI splitting criterion