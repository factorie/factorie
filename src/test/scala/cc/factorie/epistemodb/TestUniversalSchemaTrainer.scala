package cc.factorie.epistemodb

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
    val numRows = 10000
    val numCols = 1000
    val nnz = 100000
    val numDevNNZ = 100
    val numTestNNZ = 150

    val numTopics = 100
    val noise1 = 0.9

    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1).prune(1,1)._1
      println("nnz: " + m.nnz())
      //val (mTrain,mDev,mTest) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(Set(0,1,2,3,4,5,6,7,8,9)), random)

      val stepsize = 0.001
      val regularizer = 1.0
      val dim = 100
      val iters = 10

      val model = UniversalSchemaModel.randomModel(numRows, numCols, dim, random)
      val trainer = new BprUniversalSchemaTrainer(regularizer, stepsize, dim, m, model, random)
      val objectiveValues = trainer.train(iters)
      assertTrue(objectiveValues(0) < objectiveValues(9))
      assertTrue(objectiveValues(0) < objectiveValues(5))
      assertTrue(objectiveValues(5) < objectiveValues(9))



/*
      assertEquals(numDevNNZ, mDev.nnz())
      assertEquals(numTestNNZ, mTest.nnz())
      assertEquals(m.nnz(), mTrain.nnz() + mDev.nnz() + mTest.nnz())
      */
    }
  }

}
