package cc.factorie.app.uschema

import org.scalatest.junit.JUnitSuite
import cc.factorie.util
import org.junit.Test
import scala.util.Random
import org.junit.Assert._
import scala.Some

/**
 * Created by beroth on 2/20/15.
 */
class TestUniversalSchemaTrainer extends JUnitSuite  with util.FastLogging {

  @Test def testSplitRandomizedTest() {
    val numRows = 1000
    val numCols = 10000
    val nnz = 100000

    val numTopics = 100
    val noise1 = 0.1

    // Test whether objective function goes up
    for (seed <- 0 until 2) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1).prune(1,1)._1
      println("nnz: " + m.nnz())

      val stepsize = 0.1
      val regularizer = 0.01
      val dim = 10
      val iters = 10

      val model = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, m, model, random)
      val objectiveValues = trainer.train(iters)
      assertTrue(objectiveValues(0) < objectiveValues(9))
      assertTrue(objectiveValues(0) < objectiveValues(4))
      assertTrue(objectiveValues(4) < objectiveValues(9))
    }

    val numDevNNZ = 0
    val numTestNNZ = 150

    for (seed <- 0 until 2) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1).prune(1,1)._1
      println("nnz: " + m.nnz())
      val (mTrain,mDev,mTest) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(Set(0,1,2,3,4,5,6,7,8,9)), random)

      val stepsize = 0.1
      val regularizer = 0.01
      val dim = 10

      // Train model for different number of iterations
      val model0 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val model5 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer5 = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model5, random)
      trainer5.train(5)
      println("--")
      val model10 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer10 = new RegularizedBprUniversalSchemaTrainer(regularizer, stepsize, dim, mTrain, model10, random)
      trainer10.train(10)

      val result0 = model0.similaritiesAndLabels(mTrain, mTest)
      val result5 = model5.similaritiesAndLabels(mTrain, mTest)
      val result10 = model10.similaritiesAndLabels(mTrain, mTest)

      println("0 iters map: " + Evaluator.meanAveragePrecision(result0))
      println("5 iters map: " + Evaluator.meanAveragePrecision(result5))
      println("10 iters map: " + Evaluator.meanAveragePrecision(result10))

      assertTrue(Evaluator.meanAveragePrecision(result5) > Evaluator.meanAveragePrecision(result0))
      assertTrue(Evaluator.meanAveragePrecision(result10) > Evaluator.meanAveragePrecision(result5))
    }
  }

  @Test def normConstrainedTrainingTest() {
    println("Norm constrained BPR:")
    val numRows = 1000
    val numCols = 10000
    val nnz = 100000

    val numTopics = 100
    val noise1 = 0.1

    val maxNorm = 1

    // Test whether objective function goes up
    for (seed <- 0 until 2) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1).prune(1,1)._1
      println("nnz: " + m.nnz())

      val stepsize = 0.1
      val dim = 10
      val iters = 10

      val model = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer = new NormConstrainedBprUniversalSchemaTrainer(maxNorm, stepsize, dim, m, model, random)
      val objectiveValues = trainer.train(iters)
      assertTrue(objectiveValues(0) < objectiveValues(9))
      assertTrue(objectiveValues(0) < objectiveValues(4))
      assertTrue(objectiveValues(4) < objectiveValues(9))
    }

    val numDevNNZ = 0
    val numTestNNZ = 150

    for (seed <- 0 until 2) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1).prune(1,1)._1
      println("nnz: " + m.nnz())
      val (mTrain,mDev,mTest) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(Set(0,1,2,3,4,5,6,7,8,9)), random)

      val stepsize = 0.1
      val dim = 10

      // Train model for different number of iterations
      val model0 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val model5 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer5 = new NormConstrainedBprUniversalSchemaTrainer(maxNorm, stepsize, dim, mTrain, model5, random)
      trainer5.train(5)
      println("--")
      val model10 = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer10 = new NormConstrainedBprUniversalSchemaTrainer(maxNorm, stepsize, dim, mTrain, model10, random)
      trainer10.train(10)

      val result0 = model0.similaritiesAndLabels(mTrain, mTest)
      val result5 = model5.similaritiesAndLabels(mTrain, mTest)
      val result10 = model10.similaritiesAndLabels(mTrain, mTest)

      println("0 iters map: " + Evaluator.meanAveragePrecision(result0))
      println("5 iters map: " + Evaluator.meanAveragePrecision(result5))
      println("10 iters map: " + Evaluator.meanAveragePrecision(result10))

      assertTrue(Evaluator.meanAveragePrecision(result5) > Evaluator.meanAveragePrecision(result0))
      assertTrue(Evaluator.meanAveragePrecision(result10) > Evaluator.meanAveragePrecision(result5))
    }
  }
}
