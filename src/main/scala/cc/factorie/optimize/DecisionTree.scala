package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.{FastSorting, StoreFetchCubbie}
import scala.util.Random

class DecisionTreeMultiClassTrainer[Label](treeTrainer: DecisionTreeTrainer with TensorLabels = new ID3DecisionTreeTrainer)
  (implicit random: Random)
  extends MultiClassTrainerBase[DecisionTreeMultiClassClassifier] {
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: DecisionTreeMultiClassClassifier => Unit): DecisionTreeMultiClassClassifier = {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(labelSize, _))).zip(weights).map({
      case ((feat, label), weight) => DecisionTreeTrainer.Instance[Tensor1](feat, label, weight)
    })
    val dtree = treeTrainer.train(instances)
    val classifier = new DecisionTreeMultiClassClassifier(dtree)
    evaluate(classifier)
    classifier
  }
}

// TODO this threading stuff makes it non-deterministic, fix -luke
class RandomForestMultiClassTrainer(numTrees: Int, numFeaturesToUse: Int, numInstancesToSample: Int, maxDepth: Int = 25,
  useParallel: Boolean = true, numThreads: Int = Runtime.getRuntime.availableProcessors(), treeTrainer: DecisionTreeTrainer with TensorLabels = new ID3DecisionTreeTrainer)
  (implicit random: Random)
  extends MultiClassTrainerBase[RandomForestMultiClassClassifier] {
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: RandomForestMultiClassClassifier => Unit): RandomForestMultiClassClassifier = {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(labelSize, _))).zip(weights).map({
      case ((feat, label), weight) => DecisionTreeTrainer.Instance[Tensor1](feat, label, weight)
    })
    val trees = TrainerHelpers.parMap(0 until numTrees, numThreads)(_ => {
      val bootstrap = (0 until numInstancesToSample).map(_ => instances(random.nextInt(instances.length)))
      treeTrainer.maxDepth = maxDepth // TODO ugh but otherwise we can't override it in the trait - just use maxdepth stopping like before -luke
      treeTrainer.train(bootstrap, numFeaturesToUse = numFeaturesToUse)
    })
    val classifier = new RandomForestMultiClassClassifier(trees.toSeq)
    evaluate(classifier)
    classifier
  }
}

class RandomForestMultiClassClassifier(val trees: Seq[DTree]) extends MultiClassClassifier[Tensor1] {
  self =>
  def score(features: Tensor1) = {
    // TODO why not train an SVM on these predictions instead of just doing majority voting? -luke
    val res = trees.map(t => DTree.score(features, t)).map(t => {t.maxNormalize(); t}).reduce(_ + _)
    res.normalize()
    // FIXME these should be logged along with the regular decision tree scores
    res
  }
  def asTemplate[T <: LabeledMutableDiscreteVar[_]](l2f: T => TensorVar)(implicit ml: Manifest[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2(l2f, this)
}

class DecisionTreeMultiClassClassifier(val tree: DTree) extends MultiClassClassifier[Tensor1] {
  def score(features: Tensor1) =
    DTree.score(features, tree)
  def asTemplate[T <: LabeledMutableDiscreteVar[_]](l2f: T => TensorVar)(implicit ml: Manifest[T]): Template2[T, TensorVar] =
    new ClassifierTemplate2[T](l2f, this)
}

class RandomForestCubbie extends StoreFetchCubbie[RandomForestMultiClassClassifier] {
  val trees = CubbieListSlot[TreeNodeCubbie]("trees", () => new TreeNodeCubbie)
  trees := Seq()
  def store(c: RandomForestMultiClassClassifier): Unit = {
    trees := c.trees.map(t => {val c = new TreeNodeCubbie; c.store(t); c})
  }
  def fetch(): RandomForestMultiClassClassifier = new RandomForestMultiClassClassifier(trees.value.map(_.fetch()))
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
  threshold := 0;
  feature := 0;
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

class DecisionTreeCubbie extends StoreFetchCubbie[DecisionTreeMultiClassClassifier] {
  val tree = CubbieSlot[TreeNodeCubbie]("tree", () => new TreeNodeCubbie)
  def store(t: DecisionTreeMultiClassClassifier): Unit = {
    val bc = new TreeNodeCubbie
    bc.store(t.tree)
    tree := bc
  }
  def fetch(): DecisionTreeMultiClassClassifier =
    new DecisionTreeMultiClassClassifier(tree.value.fetch())
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

object DecisionTreeTrainer {
  case class Instance[Label](feats: Tensor1, label: Label, weight: Double)
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
  var maxDepth = 500

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
    val numFeatures = possibleFeatureThresholds.length
    var f = 0
    while (f < numFeatures) {
      val thresholds = possibleFeatureThresholds(f)
      if (thresholds != null) {
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
      f += 1
    }
    if (maxValue > Double.NegativeInfinity) Some((maxFeature, maxThreshold)) else None
  }

  def evaluateSplittingCriteria(instances: Seq[Instance], possibleFeatureThresholds: Array[Array[Double]]): Array[Array[Double]] = {
    val headInst = instances.head
    val numFeatures = possibleFeatureThresholds.length
    val allStats = getBucketStats(instances)
    val withFeatureStats = new Array[Array[Any]](numFeatures).asInstanceOf[Array[Array[BucketStats]]]
    val costReductions = new Array[Array[Double]](numFeatures)
    var i = 0
    while (i < numFeatures) {
      withFeatureStats(i) =
        if (possibleFeatureThresholds(i) == null) null
        else Array.fill[Any](possibleFeatureThresholds(i).length)(getEmptyBucketStats(headInst)).asInstanceOf[Array[BucketStats]]
      costReductions(i) =
        if (possibleFeatureThresholds(i) == null) null
        else Array.fill(possibleFeatureThresholds(i).length)(0.0)
      i += 1
    }
    for (inst <- instances)
      inst.feats.foreachActiveElement((i, v) => {
        val featureValues = possibleFeatureThresholds(i)
        if (featureValues != null) {
          val split = featureValues.length - featureValues.count(fv => fv > v) - 1
          val stats = withFeatureStats(i)(split)
          accumulate(stats, inst)
        }
      })
    val baseEntropy = getBucketState(instances)
    for (f <- 0 until possibleFeatureThresholds.length) {
      val thresholds = possibleFeatureThresholds(f)
      if (thresholds != null)
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

  private def getPossibleFeatureThresholds(stats: Array[Instance], numFeaturesToChoose: Int)(implicit rng: Random): Array[Array[Double]] = {
    val numFeatures = stats(0).feats.length
    val numInstances = stats.length
    // null out all but "numFeaturesToChoose" of them
    def nullOutArray(arr: Array[_]): Unit =  {
      if (numFeaturesToChoose == -1) return
      val indices = new Array[Int](numFeatures)
      val randoms = new Array[Int](numFeatures)
      var i = 0
      while (i < numFeatures) {
        indices(i) = i
        randoms(i) = rng.nextInt(numFeatures)
        i += 1
      }
      FastSorting.quickSort(randoms, indices)
      var j = 0
      while (j < numFeatures - numFeaturesToChoose) {
        arr.asInstanceOf[Array[Any]](indices(j)) = null
        j += 1
      }
    }
    val binary = stats(0).feats.isInstanceOf[SparseBinaryTensor]
    if (binary) {
      val splits = new Array[Array[Double]](numFeatures)
      var i = 0
      while (i < numInstances) {
        val inst = stats(i).feats
        inst.foreachActiveElement((i, v) => {
          if (splits(i) == null) splits(i) = Array[Double](0.5)
        })
        i += 1
      }
      nullOutArray(splits)
      return splits
    }
    val possibleThresholds = new Array[Array[Double]](numFeatures)
    var s = 0
    val featureValues = new Array[mutable.ArrayBuilder[Double]](numFeatures)
    nullOutArray(featureValues)
    while (s < numInstances) {
      stats(s).feats match {
        case sT: SparseIndexedTensor =>
          val len = sT.activeDomainSize
          val sIndices = sT._indices
          val sValues = sT._values
          var i = 0
          while (i < len) {
            val idx = sIndices(i)
            if (featureValues(idx) == null) featureValues(idx) = mutable.ArrayBuilder.make[Double]()
            featureValues(idx) += sValues(i)
            i += 1
          }
        case sT: SparseBinaryTensor =>
          val len = sT.activeDomainSize
          val sIndices = sT._indices
          var i = 0
          while (i < len) {
            val idx = sIndices(i)
            if (featureValues(idx) == null) featureValues(idx) = mutable.ArrayBuilder.make[Double]()
            featureValues(idx) += 1.0
            i += 1
          }
        case sT => sT.foreachActiveElement((f, v) => if (featureValues(f) != null) featureValues(f) += v)
      }
      s += 1
    }
    var f = 0
    while (f < numFeatures) {
      val ab = featureValues(f)
      if (ab != null) {
        ab += 0.0
        val sorted = featureValues(f).result()
        java.util.Arrays.sort(sorted)
        val thresholds = mutable.ArrayBuilder.make[Double]()
        var last = sorted(0)
        var s = 0
        while (s < sorted.length) {
          val srt = sorted(s)
          if (last < srt) {thresholds += (srt + last) / 2; last = srt}
          s += 1
        }
        possibleThresholds(f) = thresholds.result()
      }
      f += 1
    }
    possibleThresholds
    //    possibleThresholds.toSeq.zipWithIndex.flatMap({case (arr, idx) => arr.toSeq.map((idx, _))})
  }
}

// TODO full covariance splitting requires a determinant

trait DiagonalCovarianceSplitting {
  this: DecisionTreeTrainer with TensorSumSqDiagStatsAndLabels =>
  type State = Double
  // TODO add some criterion for stopping splitting when std dev gets low enough
  def samePred(labels: Seq[Label]): Boolean = false
  def getBucketState(instances: Iterable[Instance]): Double = getStdDev(getBucketStats(instances))
  def getStdDev(stats: BucketStats): Double = {
    // TODO fix this it is quite numerically unstable -luke
    val diag = stats.sumSq / stats.mult - (stats.sum / stats.mult outer stats.sum / stats.mult).asInstanceOf[Tensor2].diag()
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

// is this right??
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
  this: DecisionTreeTrainer with DTreeBucketStats with TensorLabels =>
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
  this: DecisionTreeTrainer with DTreeBucketStats with TensorLabels =>
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

trait DTreeBucketStats extends TensorLabels {
// TODO make this work with non tensor labels
//  type Label
  type BucketStats
//  type Instance = DecisionTreeTrainer.Instance[Label]
  def getEmptyBucketStats(inst: Instance): BucketStats
  def getBucketStats(labels: Iterable[Instance]): BucketStats
  def +=(left: BucketStats, right: BucketStats): Unit
  def -=(left: BucketStats, right: BucketStats): Unit
  def accumulate(stats: BucketStats, inst: Instance): Unit
  def getPrediction(stats: BucketStats): Label
  def makeLeaf(stats: BucketStats): DTree
}

trait TensorSumSqFullStatsAndLabels extends DTreeBucketStats with TensorLabels {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  // diagonal covariance or densetensor2 for sumSq? kinda wasteful. plus if we restrict to diagonal we don't have to do the determinant calculation
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

trait TensorSumSqDiagStatsAndLabels extends DTreeBucketStats with TensorLabels {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  // diagonal covariance or densetensor2 for sumSq? kinda wasteful. plus if we restrict to diagonal we don't have to do the determinant calculation
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
    stats.sumSq += ((inst.label outer inst.label).asInstanceOf[Tensor2].diag(), instWeight)
    stats.mult += instWeight
  }
  def getPrediction(stats: BucketStats): Label = stats.sum / stats.mult
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(getPrediction(stats))
}

trait TensorLabels {
  type Instance = DecisionTreeTrainer.Instance[Label]
  type Label = Tensor1
}

trait TensorSumStatsAndLabels extends DTreeBucketStats with TensorLabels {
  this: DecisionTreeTrainer =>
  type BucketStats = MutableBucketStats
  // diagonal covariance or densetensor2 for sumSq? kinda wasteful. plus if we restrict to diagonal we don't have to do the determinant calculation
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

// TODO: make a decision tree cubbie -luke
object DTree {
  @inline def hasFeature(featureIdx: Int, features: Tensor1, threshold: Double): Boolean = features(featureIdx) > threshold
  def score(features: Tensor1, node: DTree): Tensor1 = node match {
    // returns -Infinity for prob 0, which is still uniform, so I guess its ok...
    case DTLeaf(logProbs) if logProbs.isInstanceOf[Tensor1] => logProbs.asInstanceOf[Tensor1] // todo take the log here
    case DTBranch(yes, no, idx, threshold) => score(features, if (hasFeature(idx, features, threshold)) yes else no)
  }
}

// TODO add GINI and StdDev splitting criteria