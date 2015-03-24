package cc.factorie.epistemodb

import cc.factorie.util.Threading
import scala.util.Random
import scala.collection._
import cc.factorie.la.DenseTensor1

/**
 * Created by beroth on 2/19/15.
 */
abstract class BprUniversalSchemaTrainer {
  def stepsize: Double
  def dim: Int
  def matrix: CoocMatrix
  def model: UniversalSchemaModel
  def random: Random

  def updateBprCells(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double

  def updateBprOnRows(rowTrue: UniversalSchemaModel.Row, rowFalse: UniversalSchemaModel.Row): Double = {
    val rowIdxTrue = rowTrue._1
    val rowIdxFalse = rowFalse._1
    val colIndicesTrueRow = rowTrue._2.keys
    val colIndicesFalseRow = rowFalse._2.keySet
    // for only updating those where ranking is incorrect, check: model.score(rowTrueIdx, ci) < model.score(rowFalseIdx, ci)
    val data: Iterable[Int] = colIndicesTrueRow.filter(!colIndicesFalseRow.contains(_))
    //    colIndices1.filter(!colIndices2.contains(_)).flatMap(ci => List((rowTrueIdx, rowFalseIdx, ci)))
    val shuffled: Iterable[Int] = random.shuffle(data)
    val objectives = shuffled.map(ci => updateBprCells(rowIdxTrue, rowIdxFalse, ci))
    //println("positive indices: " + colIndices1.length + "\n updates: " + data.length + "\n objective: " + objectives.sum)
    objectives.sum
  }

  def train(numIters: Int): IndexedSeq[Double] = {
//    val pool = Threading.newFixedThreadPool(1)
// TODO: parallelize
    val pool = Threading.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    var printNext = 1;
    val objSeq = for (t <- 0 until numIters) yield {
      var objective = 0.0
      // random order, for bpr
      //println("shuffling ...")
      // : Array[mutable.Iterable[UniversalSchemaModel.Row]]
      val batchedRows = random.shuffle(matrix.rows).toSeq.grouped(1000).toArray
      //println("... done.")

      Threading.parForeach(batchedRows, pool)(rowBatch => {
        // Take a sliding window of two rows, and do bpr update.
        val thisObjective = rowBatch.sliding(2).foldLeft(0.0)((obj, rowPair) =>
          obj + updateBprOnRows(rowPair(0), rowPair(1))
        )
        // For non-bpr this would be:
        //val thisObjective = exs.foldLeft(0.0)((b, a) => b + updateOnRow(a._1, a._2, stepsize))
        objective += thisObjective
      })
      if (t == printNext || t == 0 || t == numIters - 1) {
        println("finished iter " + t + " objective = " + objective / matrix.rows.size)
        printNext *= 2
      }
      objective
    }
    pool.shutdown()
    objSeq
  }
}

class RegularizedBprUniversalSchemaTrainer(val regularizer: Double, val stepsize: Double, val dim: Int,
                                            val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random) extends
      BprUniversalSchemaTrainer {
  val rowRegularizer = regularizer
  val colRegularizer = regularizer

  override def updateBprCells(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    // TODO: include reguarizer into objective logged to output:
    // - sq(rowVectors) * model.rowRegularizer / 2
    // - sq(colVectors) * model.colRegularizer
    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).*=((1 - stepsize * colRegularizer))
    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)

    model.rowVectors(rowIndexTrue).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexTrue).+=(colVec, step)

    model.rowVectors(rowIndexFalse).*=((1 - stepsize * rowRegularizer))
    model.rowVectors(rowIndexFalse).+=(colVec, -step)

    thisObjective
  }
}

class NormConstrainedBprUniversalSchemaTrainer(val maxNorm: Double, val stepsize: Double, val dim: Int,
                                           val matrix: CoocMatrix, val model: UniversalSchemaModel, val random: Random) extends
    BprUniversalSchemaTrainer {

  val rowNormConstraint = maxNorm
  val colNormConstraint = maxNorm

  def constrain(vector: DenseTensor1, maxNorm: Double) {
    val norm = vector.twoNorm
    if (norm > maxNorm) {
      vector *= (maxNorm / norm)
    }
  }

  override def updateBprCells(rowIndexTrue: Int, rowIndexFalse: Int, colIndex: Int): Double = {
    val scoreTrueCell = model.score(rowIndexTrue, colIndex)
    val scoreFalseCell = model.score(rowIndexFalse, colIndex)
    val theta = scoreTrueCell - scoreFalseCell
    val prob = UniversalSchemaModel.calculateProb(theta)

    var thisObjective = math.log(prob)

    val step = stepsize * (1 - prob)
    val colVec = model.colVectors(colIndex).copy

    model.colVectors(colIndex).+=(model.rowVectors(rowIndexTrue) - model.rowVectors(rowIndexFalse), step)
    constrain(model.colVectors(colIndex), colNormConstraint)

    model.rowVectors(rowIndexTrue).+=(colVec, step)
    constrain(model.rowVectors(rowIndexTrue), rowNormConstraint)

    model.rowVectors(rowIndexFalse).+=(colVec, -step)
    constrain(model.rowVectors(rowIndexFalse), rowNormConstraint)

    thisObjective
  }
}



