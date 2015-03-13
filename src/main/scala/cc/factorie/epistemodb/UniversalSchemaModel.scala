package cc.factorie.epistemodb

import com.mongodb.DB
import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.OptimizableObjectives.UnivariateLinkFunction
import cc.factorie.optimize.{UnivariateOptimizableObjective, OptimizableObjectives}
import scala.collection.mutable
import scala.util.Random

/**
 * Created by beroth on 2/18/15.
 */
class UniversalSchemaModel(val rowVectors: IndexedSeq[DenseTensor1], val colVectors: IndexedSeq[DenseTensor1]) {
  //val objective: UnivariateOptimizableObjective[Int] = OptimizableObjectives.logBinary
  //val linkFunction: UnivariateLinkFunction = OptimizableObjectives.logisticLinkFunction
  //def predict(rowIdx: Int, colIdx: Int) = linkFunction(score(i,j))

  def similarity(vec1: DenseTensor1, vec2: DenseTensor1): Double = vec1.cosineSimilarity(vec2)
  // cosine similarity normalized to lie between 0 and one
  def similarity01(vec1: DenseTensor1, vec2: DenseTensor1): Double = (1.0 + vec1.cosineSimilarity(vec2)) / 2.0
  def score(rowIdx: Int, colIdx: Int): Double = rowVectors(rowIdx).dot(colVectors(colIdx))

  /**
   * Compute test scores for all cells that are not positive entries in the train and development sets.
   * These cells form the basis for evaluation. Additionally to this score, a indicator whether the scored cell is
   * positive in the test data is output.
   *
   * The positive test matrix cells, and all negative cells (from both training and test matrix) form the basis for the
   * test scores.
   *
   * The test scores can be passed to the Evaluator object, to compute evaluation measures such as mean average
   * precision and f1 score.
   */
  def similaritiesAndLabels(trainDevMatrix: CoocMatrix, testMatrix: CoocMatrix, testCols: Option[Set[Int]] = None):
    Map[Int, Seq[(Double, Boolean)]] = {
    val columns = testCols match {
      case Some(cols) => cols
      case None => testMatrix.nonZeroCols()
    }

    {for (col <- columns) yield {
      val scores = {for (row <- (0 until testMatrix.numRows());
      if trainDevMatrix.get(row, col) == 0) yield {
        val sim = similarity01(rowVectors(row), colVectors(col))
        val isTrueTest = testMatrix.get(row, col) != 0
        (sim, isTrueTest)
      }}.toSeq
      (col, scores)
    }}.toMap
  }

  def similaritiesAndLables(testMatrix: CoocMatrix, cells: Map[Int, Seq[Int]]):
    Map[Int, Seq[(Double, Boolean)]] = {
    {for (col <- cells.keys) yield {
      val scores = {for (row <- cells.get(col).get) yield {
        val sim = similarity01(rowVectors(row), colVectors(col))
        val isTrueTest = testMatrix.get(row, col) != 0
        (sim, isTrueTest)
      }}.toSeq
      (col, scores)
    }}.toMap
  }

  def getScoredColumns(v: DenseTensor1): Iterable[(Int, Double)] = {
    throw new UnsupportedOperationException
  }

  def getScoredRows(v: DenseTensor1): Iterable[(Int, Double)] = {
    throw new UnsupportedOperationException
  }

  def writeToMongo(mongoDb: DB, dropCollections: Boolean = true) {
    throw new UnsupportedOperationException
  }

}

object UniversalSchemaModel {
  type Row = (Int, mutable.HashMap[Int, Double])

  def fromMongo(mongoDb: DB): UniversalSchemaModel = {
    throw new UnsupportedOperationException
  }

  def randomModel(numRows: Int, numCols:Int, dim: Int, random: Random = new Random(0)): UniversalSchemaModel = {
    val scale = 1.0 / dim
    def initVector(): Array[Double] = Array.fill[Double](dim)(scale * random.nextGaussian())
    //def initVector(i: Int): Array[Double] = Array.fill[Double](latentDimensionality)(2*random.nextDouble() - 1.0)
    val rowVectors = (0 until numRows).map(i => new DenseTensor1(initVector))
    val colVectors = (0 until numCols).map(i => new DenseTensor1(initVector))
    new UniversalSchemaModel(rowVectors, colVectors)
  }

  def calculateProb(theta: Double): Double = {
    1.0 / (1.0 + math.exp(-theta))
  }
}
