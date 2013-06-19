package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.StoreFetchCubbie
import java.util.Random

class DecisionTreeMultiClassTrainer(treeTrainer: DecisionTreeTrainer with TensorStatsAndLabels = new ID3DecisionTreeTrainer)
  extends MultiClassTrainerBase[DecisionTreeMultiClassClassifier] {
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: DecisionTreeMultiClassClassifier => Unit): DecisionTreeMultiClassClassifier = {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(labelSize, _)))
    val weightsMap = instances.zip(weights).toMap[(Tensor1, Tensor1), Double]
    val dtree = treeTrainer.train(instances, weightsMap)
    val classifier = new DecisionTreeMultiClassClassifier(dtree)
    evaluate(classifier)
    classifier
  }
}

class RandomForestMultiClassTrainer(numTrees: Int, numFeaturesToUse: Int, numInstancesToSample: Int, maxDepth: Int = 25, treeTrainer: DecisionTreeTrainer with TensorStatsAndLabels = new ID3DecisionTreeTrainer)
  extends MultiClassTrainerBase[RandomForestMultiClassClassifier] {
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: RandomForestMultiClassClassifier => Unit): RandomForestMultiClassClassifier = {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(labelSize, _)))
    val weightsMap = instances.zip(weights).toMap[(Tensor1, Tensor1), Double]
    val trees = for (i <- 0 until numTrees) yield {
      val bootstrap = (0 until numInstancesToSample).map(_ => instances(random.nextInt(instances.length)))
      treeTrainer.maxDepth = maxDepth // TODO ugh but otherwise we can't override it in the trait - just use maxdepth stopping like before -luke
      treeTrainer.train(bootstrap, weightsMap, numFeaturesToUse = numFeaturesToUse)
    }
    val classifier = new RandomForestMultiClassClassifier(trees)
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
  threshold := 0; feature := 0; isLeaf := false
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
  private def toCubbie(dt: DTree): TreeNodeCubbie = {val c = new TreeNodeCubbie; c.store(dt); c}
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

class C45DecisionTreeTrainer
  extends DecisionTreeTrainer
  with TensorStatsAndLabels
  with GainRatioSplitting
  with SampleSizeStopping
  with ErrorBasedPruning {
  val minSampleSize = 4
}

class DecisionStumpTrainer
  extends DecisionTreeTrainer
  with TensorStatsAndLabels
  with GainRatioSplitting
with SampleSizeStopping
  with NoPruning {
  maxDepth = 1
  val minSampleSize = 4
}

class ID3DecisionTreeTrainer
  extends DecisionTreeTrainer
  with TensorStatsAndLabels
  with InfoGainSplitting
  with SampleSizeStopping
  with NoPruning {
  val minSampleSize = 4
}

trait DecisionTreeTrainer {
  implicit var random = new scala.util.Random(0)

  type Instance = (Tensor1, Label)
  type Label
  type BucketStats
  type State

  def getPerNodeState(stats: Seq[Instance], instanceWeights: Instance => Double): State
  def evaluateSplittingCriteria(s: State, withFeature: BucketStats, withoutFeature: BucketStats): Double
  def prune(tree: DTree, pruningSet: Seq[Instance]): DTree

  def getBucketStats(labels: Iterable[Instance], weights: Instance => Double): BucketStats
  def getPrediction(stats: BucketStats): Label
  def makeLeaf(stats: BucketStats): DTree
  def shouldStop(stats: Seq[Instance], depth: Int): Boolean = false

  // what's a good value for this?
  var maxDepth = 500

  def getBucketPrediction(labels: Seq[Instance], weights: Instance => Double): Label = getPrediction(getBucketStats(labels, weights))

  def train(trainInstances: Seq[(Tensor1, Label)], instanceWeights: Instance => Double, pruneInstances: Seq[(Tensor1, Label)] = Nil, numFeaturesToUse: Int = -1): DTree = {
    val tree =
      if (maxDepth <= 1000) trainTree(trainInstances.toSeq, 0, maxDepth, Set.empty[(Int, Double)], instanceWeights, numFeaturesToUse)
      else trainTreeNoMaxDepth(trainInstances.toSeq, maxDepth, instanceWeights, numFeaturesToUse)
    if (pruneInstances.size == 0) tree else prune(tree, pruneInstances)
  }

  private def trainTree(stats: Seq[Instance], depth: Int, maxDepth: Int, usedFeatures: Set[(Int, Double)], instanceWeights: Instance => Double, numFeaturesToChoose: Int): DTree = {
    // FIXME this is an ugly way to figure out if the bucket has only one label -luke
    if (maxDepth < depth || stats.map(l => if (l._2.isInstanceOf[Tensor]) l._2.asInstanceOf[Tensor].maxIndex else l._2).distinct.length == 1 || shouldStop(stats, depth)) {
      makeLeaf(getBucketStats(stats, instanceWeights))
    } else {
      getSplittingFeatureAndThreshold(stats.toArray, usedFeatures, instanceWeights, numFeaturesToChoose) match {
        case Some((featureIdx, threshold)) =>
          val (statsWithFeature, statsWithoutFeature) = stats.partition(s => hasFeature(featureIdx, s._1, threshold))
          @inline def trainChildTree(childStats: Seq[Instance]): DTree =
            if (childStats.length > 0) trainTree(childStats, depth + 1, maxDepth, usedFeatures + ((featureIdx, threshold)), instanceWeights, numFeaturesToChoose)
            else makeLeaf(getBucketStats(stats, instanceWeights))
          DTBranch(trainChildTree(statsWithFeature), trainChildTree(statsWithoutFeature), featureIdx, threshold)
        case None => makeLeaf(getBucketStats(stats, instanceWeights))
      }
    }
  }

  private def trainTreeNoMaxDepth(startingSamples: Seq[Instance], maxDepth: Int, instanceWeights: Instance => Double, numFeaturesToChoose: Int): DTree = {
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
    val todo = mutable.Stack((startingSamples, Set.empty[(Int, Double)], 0, 0)) // samples, usedFeatures, depth, heapIdx
    val branches = new mutable.Stack[(Int, Int, Double)]() // heapIdx, featureIdx, threshold
    val nodes = new DenseIntMap[DTree]() // heapIdx -> node
    while (!todo.isEmpty) {
      val (samples, usedFeatures, depth, heapIdx) = todo.pop()
      if (maxDepth < depth || samples.map(_._2).distinct.length == 1 || shouldStop(samples, depth)) {
        updateIntMap(nodes, heapIdx, makeLeaf(getBucketStats(samples, instanceWeights)))
      } else {
        getSplittingFeatureAndThreshold(samples.toArray, usedFeatures, instanceWeights, numFeaturesToChoose) match {
          case Some((featureIdx, threshold)) =>
            @inline def pushChildWork(childStats: Seq[Instance], childIdx: Int) =
              if (childStats.length > 0) todo.push((childStats, usedFeatures + ((featureIdx, threshold)), depth + 1, childIdx))
              else updateIntMap(nodes, childIdx, makeLeaf(getBucketStats(samples, instanceWeights)))
            val (statsWithFeature, statsWithoutFeature) = samples.partition(s => hasFeature(featureIdx, s._1, threshold))
            pushChildWork(statsWithFeature, child(leftChildren, heapIdx))
            pushChildWork(statsWithoutFeature, child(rightChildren, heapIdx))
            branches.push((heapIdx, featureIdx, threshold))
          case None =>
            updateIntMap(nodes, heapIdx, makeLeaf(getBucketStats(samples, instanceWeights)))
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

  private def split[T](xs: Iterable[T])(pred: T => Boolean): (Iterable[T], Iterable[T]) = {
    val fst = new ArrayBuffer[T]; val snd = new ArrayBuffer[T]
    for (x <- xs) if (pred(x)) fst += x else snd += x
    (fst, snd)
  }

  private def getSplittingFeatureAndThreshold(stats: Array[Instance], usedFeatures: Set[(Int, Double)], instanceWeights: Instance => Double, numFeaturesToChoose: Int): Option[(Int, Double)] = {
    val state = getPerNodeState(stats, instanceWeights)
    val numFeatures = stats.head._1.length
    val possibleFeatureThresholds = getPossibleFeatureThresholds(stats, numFeaturesToChoose)
    var featureIdx = 0
    var maxIdx = 0
    var maxThreshold = Double.NegativeInfinity
    var maxValue = Double.NegativeInfinity
    while (featureIdx < numFeatures) {
      val thresholds = possibleFeatureThresholds(featureIdx)
      if (thresholds != null) {
        var thresholdIdx = 0
        while (thresholdIdx < thresholds.length) {
          val threshold = thresholds(thresholdIdx)
          if (!usedFeatures((featureIdx, threshold))) {
            val (instancesWith, instancesWithout) = split(stats)(s => hasFeature(featureIdx, s._1, threshold))
            if (instancesWith.size > 0 && instancesWithout.size > 0) {
              val criteria = evaluateSplittingCriteria(state, getBucketStats(instancesWith, instanceWeights), getBucketStats(instancesWithout, instanceWeights))
              if (criteria > maxValue) {
                maxValue = criteria
                maxIdx = featureIdx
                maxThreshold = threshold
              }
            }
          }
          thresholdIdx += 1
        }
      }
      featureIdx += 1
    }
    if (maxValue > Double.NegativeInfinity) Some((maxIdx, maxThreshold)) else None
  }

  @inline def hasFeature(featureIdx: Int, feats: Tensor1, threshold: Double): Boolean = feats(featureIdx) > threshold

  private def getPossibleFeatureThresholds(stats: Array[Instance], numFeaturesToChoose: Int): Array[Array[Double]] = {
    val numInstances = stats.length
    val numFeatures = stats(0)._1.length
    val possibleThresholds = new Array[Array[Double]](numFeatures)
    var s = 0
    val featureValues = Array.fill(numFeatures)(mutable.ArrayBuilder.make[Double]())
    // null out all but "numFeaturesToChoose" of them
    if (numFeaturesToChoose != -1)
      (0 until numFeatures).shuffle.take(numFeatures - numFeaturesToChoose).foreach(f => featureValues(f) = null)
    while (s < numInstances) {
      stats(s)._1 match {
        case sT: SparseIndexedTensor =>
          val len = sT.activeDomainSize
          val sIndices = sT._indices
          val sValues = sT._values
          var i = 0
          while (i < len) {
            val idx = sIndices(i)
            if (featureValues(idx) != null)
              featureValues(idx) += sValues(i)
            i += 1
          }
        case sT: SparseBinaryTensor =>
          val len = sT.activeDomainSize
          val sIndices = sT._indices
          var i = 0
          while (i < len) {
            val idx = sIndices(i)
            if (featureValues(idx) != null)
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
          if (last < srt) { thresholds += ((srt + last) / 2); last = srt }
          s += 1
        }
        possibleThresholds(f) = thresholds.result()
      }
      f += 1
    }
    possibleThresholds
  }
}

trait NoTrainingState {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
  type State = Unit
  def getPerNodeState(stats: Seq[Instance], instanceWeights: Instance => Double): State = ()
  def evaluateSplittingCriteria(s: State, withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double =
    evaluateSplittingCriteria(withFeature, withoutFeature)
  def evaluateSplittingCriteria(withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double
}

trait InfoGainSplitting
  extends NoTrainingState {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
  def evaluateSplittingCriteria(withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double = {
    // we're using information gain modulo the constant factor of base entropy
    val numFeatures = withFeature._2 + withoutFeature._2
    val pctWith = withFeature._2 * 1.0 / numFeatures
    val pctWithout = withoutFeature._2 * 1.0 / numFeatures
    val infoGainMinusBaseEntropy = -(pctWith * withFeature._1.entropy + pctWithout * withoutFeature._1.entropy)
    infoGainMinusBaseEntropy
  }
}

trait GainRatioSplitting {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
  type State = Double
  def getPerNodeState(stats: Seq[Instance], instanceWeights: Instance => Double): State =
    getBucketStats(stats, instanceWeights)._1.entropy
  def evaluateSplittingCriteria(baseEntropy: State, withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double = {
    val numInstances = withFeature._2 + withoutFeature._2
    val pctWith = withFeature._2 * 1.0 / numInstances
    val pctWithout = withoutFeature._2 * 1.0 / numInstances
    val infoGain = baseEntropy - (pctWith * withFeature._1.entropy + pctWithout * withoutFeature._1.entropy)
    val intrinsicValue = -(pctWith * math.log(pctWith) / math.log(2) + pctWithout * math.log(pctWithout) / math.log(2))
    infoGain / intrinsicValue
  }
}

trait SampleSizeStopping {
  this: DecisionTreeTrainer =>
  def minSampleSize: Int
  override def shouldStop(stats: Seq[Instance], depth: Int) = stats.size < minSampleSize
}

// is this right??
trait UniformLabelStopping
  extends SampleSizeStopping {
  this: DecisionTreeTrainer =>
  val minSampleSize = 1
}

trait NoPruning {
  this: DecisionTreeTrainer =>
  def prune(tree: DTree, pruningSet: Seq[Instance]) = tree
}

trait ErrorBasedPruning {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
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
    stats.count(s => s._1.maxIndex == label(s._1, node)) * 1.0 / stats.length
  def label(feats: Tensor1, node: DTree): Int = node match {
    case DTLeaf(proportions) => proportions.maxIndex
    case DTBranch(yes, no, idx, threshold) => label(feats, if (hasFeature(idx, feats, threshold)) yes else no)
  }
}

trait TensorStatsAndLabels {
  this: DecisionTreeTrainer =>
  type Label = Tensor1
  type BucketStats = (Tensor1, Double) // weighted avg and weighted count
  def getBucketStats(labels: Iterable[Instance], weights: Instance => Double): BucketStats = {
    val numLabels = labels.head._2.length
    val sum = labels.foldLeft(new DenseTensor1(numLabels))((acc, el) => {acc += (el._2, weights(el)); acc})
    val denom = labels.map(weights).sum
    sum /= denom
    (sum, denom)
  }
  def getPrediction(stats: BucketStats): Label = stats._1
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(stats._1)
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