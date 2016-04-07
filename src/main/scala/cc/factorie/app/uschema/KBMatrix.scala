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
package cc.factorie.app.uschema

import scala.util.Random
import com.google.common.collect.HashBiMap
import scala.Predef._
import scala.Some
import scala.collection.JavaConverters._

/**
 * Created by beroth on 3/4/15.
 */

class StringStringMatrix(val __rowMap: MatrixIndexMap[String]  = new MemoryIndexMap[String],
                          val __colMap: MatrixIndexMap[String] = new MemoryIndexMap[String],
                          val matrix: CoocMatrix = new CoocMatrix(0,0)) extends KBMatrix[StringStringMatrix, String, String] {
  def cloneWithNewCells(cells: CoocMatrix): StringStringMatrix = new StringStringMatrix(__rowMap, __colMap, cells)
  def createEmptyMatrix: StringStringMatrix = new StringStringMatrix()
}

// TODO:
// do we need this?
// how do the relationmentions interact with the documents and the matrix?
trait MatrixWithDocs {
}

trait KBMatrix[KBMatrixT <: KBMatrix[KBMatrixT, RowT, ColT], RowT, ColT] {
  //http://docs.scala-lang.org/tutorials/tour/variances.html
  val __rowMap: MatrixIndexMap[RowT]
  val __colMap: MatrixIndexMap[ColT]
  val matrix: CoocMatrix

  // TODO: have this static and with rowMap and ColMap set as arguments?
  def cloneWithNewCells(cells: CoocMatrix): KBMatrixT
  // TODO: have this static?
  def createEmptyMatrix: KBMatrixT

  def set(rowKey: RowT, colKey: ColT, cellVal: Double) {
    val rowNr = __rowMap.add(rowKey)
    val colNr = __colMap.add(colKey)
    matrix.set(rowNr, colNr, cellVal)
  }

  def get(rowKey: RowT, colKey: ColT): Double = {
    if (__rowMap.containsKey(rowKey) && __colMap.containsKey(colKey)) {
      val rowNr = __rowMap.keyToIndex(rowKey)
      val colNr = __colMap.keyToIndex(colKey)
      matrix.get(rowNr, colNr)
    } else {
      0.0
    }
  }

  def nnz() = matrix.nnz()
  def numRows(): Int = matrix.numRows()
  def numCols(): Int = matrix.numCols()

  def getColsForRow(rowKey: RowT): Iterable[ColT] = {
    if (__rowMap.containsKey(rowKey)) {
      val rowNr = __rowMap.keyToIndex(rowKey)
      matrix.getRow(rowNr).map(cell => __colMap.indexToKey(cell._1))
    } else {
      List()
    }
  }

  def hasSameContent(m2: KBMatrixT): Boolean = {
    m2.numRows() == numRows() &&
      m2.numCols() == numCols() &&
      m2.nnz() == nnz() &&
      __rowMap.keyIterator.forall(rowKey => {
        getColsForRow(rowKey).forall(colKey => get(rowKey, colKey) == m2.get(rowKey, colKey))
      })
  }

  /**
   * This call randomTestSplit on the underlying CoocMatrix, see the documentation their for details of the splitting
   * algorithm.
   * The results of this method is a triple of three KBMatrices:
   *  - a train matrix, containing (nnz() - numDevNNZ - numTestNNZ) non-zero entries
   *  - a development matrix, containing numDevNNZ non-zero entries
   *  - test matrix, containing numTestNNZ entries
   *
   *  The row and column maps are shared across all resulting KBMatrices, the the same as in this KBMatrix.
   *
   * @param numDevNNZ
   * @param numTestNNZ
   * @param testRows
   * @param testCols
   * @param random
   * @return
   */
  def randomTestSplit(numDevNNZ: Int, numTestNNZ: Int, testRows: Option[Set[RowT]] = None,
                      testCols: Option[Set[ColT]] = None, random:Random = new Random(0)): (KBMatrixT, KBMatrixT, KBMatrixT) = {
    // TODO mapping of row/column keys to ints using MatrixIndexMap
    val testRowIndices = testRows match {
      case Some(rowKeys) => Some( rowKeys.map(key => __rowMap.keyToIndex(key)).toSet )
      case None => None
    }
    val testColIndices = testCols match {
      case Some(colKeys) => Some( colKeys.map(key => __colMap.keyToIndex(key)).toSet )
      case None => None
    }

    val (trainCooc, devCooc, testCooc) =
      matrix.randomTestSplit(numDevNNZ, numTestNNZ, testRowIndices, testColIndices, random)

    val trainKB = this.cloneWithNewCells(trainCooc)
    val devKB = this.cloneWithNewCells(devCooc)
    val testKB = this.cloneWithNewCells(testCooc)

    (trainKB, devKB, testKB)
  }

  /* use prune(0,0) for no pruning
 * use prune(2,1) for moderate pruning on kb matrices
 */
  def prune(tRow: Int = 2, tCol: Int = 2): KBMatrixT = {
    val (prunedMatrix, oldToNewRow, oldToNewCol) = matrix.prune(tRow, tCol)
    val newKb: KBMatrixT = this.createEmptyMatrix

    val newToOldCol = oldToNewCol.map(_ swap)
    val newToOldRow = oldToNewRow.map(_ swap)

    for (rowNr <- 0 until prunedMatrix.numRows()) {
      for((colNr, cellVal) <- prunedMatrix.getRow(rowNr)) {
        val rowKey = __rowMap.indexToKey(newToOldRow.get(rowNr).get)
        val colKey = __colMap.indexToKey(newToOldCol.get(colNr).get)
        newKb.set(rowKey, colKey, cellVal)
      }
    }
    newKb
  }

}
