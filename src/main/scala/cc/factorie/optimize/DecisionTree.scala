package cc.factorie.optimize

import cc.factorie._
import cc.factorie.la._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.StoreFetchCubbie
import scala.util.Random

class DecisionTreeMultiClassTrainer(treeTrainer: DecisionTreeTrainer with TensorStatsAndLabels = new ID3DecisionTreeTrainer)
  (implicit random: Random)
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

class RandomForestMultiClassTrainer(numTrees: Int, numFeaturesToUse: Int, numInstancesToSample: Int, maxDepth: Int = 25,
  useParallel: Boolean = true, numThreads: Int = Runtime.getRuntime.availableProcessors(), treeTrainer: DecisionTreeTrainer with TensorStatsAndLabels = new ID3DecisionTreeTrainer)
  (implicit random: Random)
  extends MultiClassTrainerBase[RandomForestMultiClassClassifier] {
  def simpleTrain(labelSize: Int, featureSize: Int, labels: Seq[Int], features: Seq[Tensor1], weights: Seq[Double], evaluate: RandomForestMultiClassClassifier => Unit): RandomForestMultiClassClassifier = {
    val instances = features.zip(labels.map(new SingletonBinaryTensor1(labelSize, _)))
    val weightsMap = instances.zip(weights).toMap[(Tensor1, Tensor1), Double]
    val trees = TrainerHelpers.parMap(0 until numTrees, numThreads)(_ => {
      val bootstrap = (0 until numInstancesToSample).map(_ => instances(random.nextInt(instances.length)))
      treeTrainer.maxDepth = maxDepth // TODO ugh but otherwise we can't override it in the trait - just use maxdepth stopping like before -luke
      treeTrainer.train(bootstrap, weightsMap, numFeaturesToUse = numFeaturesToUse)
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

  type Instance = (Tensor1, Label)
  type Label
  type BucketStats
  // TODO come back and rewrite to use perBucketStates and splitting criteria
  //  type State
  //
  //  def getBucketState(instances: Iterable[Instance], instanceWeights: Instance => Double): State
  def prune(tree: DTree, pruningSet: Seq[Instance]): DTree

  def getBucketStats(labels: Iterable[Instance], weights: Instance => Double): BucketStats
  def getPrediction(stats: BucketStats): Label
  def makeLeaf(stats: BucketStats): DTree
  def shouldStop(stats: Seq[Instance], depth: Int): Boolean = false
  def evaluateSplittingCriteria(instances: Seq[Instance], instanceWeights: Instance => Double, possibleFeatureThresholds: Array[Array[Double]]): Array[Array[Double]]
  //  def evaluateSplittingCriteria(state: State, statsWith: BucketStats, statsWithout: BucketStats): Double

  // what's a good value for this?
  var maxDepth = 500

  def getBucketPrediction(labels: Seq[Instance], weights: Instance => Double): Label = getPrediction(getBucketStats(labels, weights))

  def train(trainInstances: Seq[(Tensor1, Label)], instanceWeights: Instance => Double, pruneInstances: Seq[(Tensor1, Label)] = Nil, numFeaturesToUse: Int = -1)(implicit rng: Random): DTree = {
    val tree =
      if (maxDepth <= 1000) trainTree(trainInstances.toSeq, 0, maxDepth, Set.empty[(Int, Double)], instanceWeights, numFeaturesToUse)
      else trainTreeNoMaxDepth(trainInstances.toSeq, maxDepth, instanceWeights, numFeaturesToUse)
    if (pruneInstances.size == 0) tree else prune(tree, pruneInstances)
  }

  private def trainTree(stats: Seq[Instance], depth: Int, maxDepth: Int, usedFeatures: Set[(Int, Double)], instanceWeights: Instance => Double, numFeaturesToChoose: Int)(implicit rng: Random): DTree = {
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

  private def trainTreeNoMaxDepth(startingSamples: Seq[Instance], maxDepth: Int, instanceWeights: Instance => Double, numFeaturesToChoose: Int)(implicit rng: Random): DTree = {
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
    val fst = new ArrayBuffer[T];
    val snd = new ArrayBuffer[T]
    for (x <- xs) if (pred(x)) fst += x else snd += x
    (fst, snd)
  }

  private def getSplittingFeatureAndThreshold(instances: Array[Instance], usedFeatures: Set[(Int, Double)], instanceWeights: Instance => Double, numFeaturesToChoose: Int)(implicit rng: Random): Option[(Int, Double)] = {
    val possibleFeatureThresholds = getPossibleFeatureThresholds(instances, numFeaturesToChoose)
    val criteria = evaluateSplittingCriteria(instances, instanceWeights, possibleFeatureThresholds)
    if (criteria.size > 0) Some(criteria.flatten.zip(possibleFeatureThresholds.zipWithIndex.flatMap({case (arr, i) => arr.map((i, _))})).maxBy(_._1)._2) else None
  }

  @inline def hasFeature(featureIdx: Int, feats: Tensor1, threshold: Double): Boolean = feats(featureIdx) > threshold

  private def getPossibleFeatureThresholds(stats: Array[Instance], numFeaturesToChoose: Int)(implicit rng: Random): Array[Array[Double]] = {
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
          if (last < srt) {thresholds += ((srt + last) / 2); last = srt}
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

trait InfoGainSplitting {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
  type State = Double
  def getBucketState(instances: Iterable[Instance], instanceWeights: Instance => Double): Double = getBucketStats(instances, instanceWeights)._1.entropy
  def evaluateSplittingCriteria(baseEntropy: Double, withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double = {
    val numInstances = withFeature._2 + withoutFeature._2
    val pctWith = withFeature._2 * 1.0 / numInstances
    val pctWithout = withoutFeature._2 * 1.0 / numInstances
    val infoGain = baseEntropy - (pctWith * withFeature._1.entropy + pctWithout * withoutFeature._1.entropy)
    infoGain
  }
}

trait GainRatioSplitting {
  this: DecisionTreeTrainer with TensorStatsAndLabels =>
  // this can be factored out as this will be the same for std dev reduction, gain ratio, GINI, etc
  type State = Double
  def getBucketState(instances: Iterable[Instance], instanceWeights: Instance => Double): Double = getBucketStats(instances, instanceWeights)._1.entropy
  def evaluateSplittingCriteria(baseEntropy: Double, withFeature: (Tensor1, Double), withoutFeature: (Tensor1, Double)): Double = {
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
  type State
  type BucketStats = (Tensor1, Double)
  // weighted avg and weighted count
  def getBucketState(labels: Iterable[Instance], weights: Instance => Double): State
  def getBucketStats(labels: Iterable[Instance], weights: Instance => Double): BucketStats = {
    val numLabels = labels.head._2.length
    val sum = labels.foldLeft(new DenseTensor1(numLabels))((acc, el) => {acc +=(el._2, weights(el)); acc})
    val denom = labels.map(weights).sum
    sum /= denom
    (sum, denom)
  }
  def getPrediction(stats: BucketStats): Label = stats._1
  def makeLeaf(stats: BucketStats): DTree = DTLeaf(stats._1)
  def evaluateSplittingCriteria(instances: Seq[Instance], instanceWeights: Instance => Double, possibleFeatureThresholds: Array[Array[Double]]): Array[Array[Double]] = {
    val possThresholds = possibleFeatureThresholds
    val allStats = getBucketStats(instances, instanceWeights)
    class MutableBucketStats(val sum: DenseTensor1 = new DenseTensor1(instances(0)._2.length), var mult: Double = 0.0)
    val withFeatureStats = Array.tabulate(possThresholds.size)(i => Array.fill(possThresholds(i).size)(new MutableBucketStats))
    val costReductions = Array.tabulate(possThresholds.size)(i => Array.fill(possThresholds(i).size)(0.0))
    for (inst@(feats, label) <- instances) {
      feats.foreachActiveElement((i, v) => {
        val featureValues = possibleFeatureThresholds(i)
        val split = featureValues.count(fv => fv > v)
        val stats = withFeatureStats(i)(split)
        val instWeight = instanceWeights(inst)
        stats.sum +=(label, instWeight)
        stats.mult += instWeight
      })
    }
    val baseEntropy = getBucketState(instances, instanceWeights)
    for (f <- 0 until possibleFeatureThresholds.length) {
      val thresholds = possibleFeatureThresholds(f)
      for (t <- 0 until thresholds.length) {
        val withStats = withFeatureStats(f)(t)
        val withoutStats = new MutableBucketStats
        withoutStats.sum +=(allStats._1, allStats._2)
        withoutStats.mult += allStats._2
        withoutStats.sum -= withStats.sum
        // stupid floating point - this can go below zero
        val withoutStatsArr = withoutStats.sum.asArray
        var i = 0; val len = withoutStatsArr.length
        while (i < len) {if (withoutStatsArr(i) < 0.0) withoutStatsArr(i) = 0.0; i += 1}
        withoutStats.mult -= withStats.mult
        val infogain =
          if (withStats.mult > 0.0 && withoutStats.mult > 0.0)
            evaluateSplittingCriteria(baseEntropy, (withStats.sum / withStats.mult, withStats.mult), (withoutStats.sum / withoutStats.mult, withoutStats.mult))
          else
            Double.NegativeInfinity
        costReductions(f)(t) = infogain
      }
    }
    costReductions
  }
  def evaluateSplittingCriteria(s: State, withStats: BucketStats, withoutStats: BucketStats): Double
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