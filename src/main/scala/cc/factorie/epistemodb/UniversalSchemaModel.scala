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

  // TODO:? (1 + relVec.cosineSimilarity(patVec)) / 2.0
  def similarity(vec1: DenseTensor1, vec2: DenseTensor1): Double = vec1.cosineSimilarity(vec2)
  def score(rowIdx: Int, colIdx: Int): Double = rowVectors(rowIdx).dot(colVectors(colIdx))

  /**
   * Compute test scores for all cells that are not positive entries in the train and development sets.
   * These cells form the basis for evaluation.
   *
   * Indicate which cells are positive cells in the test matrix.
   * The positive test matrix cells, and all negative cells form the basis for the evaluation scores, such as mean
   * average precision and f1 score.
   *
   * All columns with at least one positive entry in the test matrix
   *
   * The method returns a mapping from column
   */
  def scoresAndLabels(trainDevMatrix: CoocMatrix, testMatrix: CoocMatrix, testCols: Option[Set[Int]] = None):
    Map[Int, Seq[(Double, Boolean)]] = {
    throw new UnsupportedOperationException
  }

  def writeToMongo(mongoDb: DB, dropCollections: Boolean = true) {
    throw new UnsupportedOperationException
  }

/*
  def updateBprOnRows(exTrue: LightweightUniversalSchemaIntRelations, isTrain: Boolean,
                      exFalse: LightweightUniversalSchemaIntRelations, stepsize: Double,
                      model: UniversalSchemaModel): Double = {
    val rowTrueIdx = exTrue.epi
    val rowFalseIdx = exFalse.epi

    val colIndices1 = if (isTrain)
      exTrue.rels
    else
      exTrue.rels.filterNot(exTrue.schemaRels.contains(_))

    val colIndices2 = exFalse.rels
    // for only updating those were ranking is incorrect, check: model.score(rowTrueIdx, ci) < model.score(rowFalseIdx, ci)
    val data: Seq[Int] = colIndices1.filter(!colIndices2.contains(_))
    //    colIndices1.filter(!colIndices2.contains(_)).flatMap(ci => List((rowTrueIdx, rowFalseIdx, ci)))

    val shuffled = random.shuffle(data)
    val objectives = shuffled.map(ci => updateBprCells(model, rowTrueIdx, rowFalseIdx, ci, stepsize))
    //println("positive indices: " + colIndices1.length + "\n updates: " + data.length + "\n objective: " + objectives.sum)
    objectives.sum
  }*/

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
