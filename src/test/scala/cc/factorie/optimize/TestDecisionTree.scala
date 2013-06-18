package cc.factorie.optimize

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.traversableExtras
import cc.factorie.la.{Tensor1, SparseIndexedTensor1, SparseBinaryTensor1}

class TestDecisionTree extends JUnitSuite {
  @Test def runTest(): Unit = {
    implicit val random = new scala.util.Random(0)
    val mean1 = (0 until 100).map(_ => random.nextDouble()).toSeq
    val mean2 = (0 until 100).map(_ => random.nextDouble()).toSeq
    val positiveExampleSeqs = (0 until 100).map(_ => (0 until 10).map(_ => mean1.zipWithIndex.sampleProportionally(_._1)._2))
    val negativeExampleSeqs = (0 until 100).map(_ => (0 until 10).map(_ => mean2.zipWithIndex.sampleProportionally(_._1)._2))
    val posExampleTensors = positiveExampleSeqs.map(pos => {
      val t = new SparseIndexedTensor1(100)
      pos.foreach(p => t += (p, 1.0))
      t
    })
    val negExampleTensors = negativeExampleSeqs.map(neg => {
      val t = new SparseIndexedTensor1(100)
      neg.foreach(p => t += (p, 1.0))
      t
    })

    // add truth feature - if this doesn't help, we have a bug
    // TODO add some tests for feature splitting
//    posExampleTensors.foreach(t => t += (100, 1.0))
    val (trainSet, testSet) = (posExampleTensors.map(p => (p, 1)) ++ negExampleTensors.map(n => (n, 0))).shuffle.split(0.5)
    val trainers = Seq(
      new BoostingMultiClassTrainer(100),
      new OnlineLinearMultiClassTrainer,
      new RandomForestMultiClassTrainer(100, 100, 100),
      new DecisionTreeMultiClassTrainer(new C45DecisionTreeTrainer))

    val trainFeatures = trainSet.map(_._1)
    val trainLabels = trainSet.map(_._2)
    val testFeatures = testSet.map(_._1)
    val testLabels = testSet.map(_._2)
    val evaluate = (c: MultiClassClassifier[Tensor1]) => {
      val accuracy = testFeatures.map(i => c.classification(i).bestLabelIndex)
        .zip(testLabels).count(i => i._1 == i._2).toDouble/testLabels.length
      println(f"Test accuracy: $accuracy%1.4f trainer: ${c.getClass.getName}")
      assert(accuracy > 0.66)
    }

    for (trainer <- trainers)
      trainer.simpleTrain(2, 100, trainLabels, trainFeatures, trainSet.map(_ => 1.0), evaluate)
  }
}
