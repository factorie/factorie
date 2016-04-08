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

import scala.collection.mutable
import com.mongodb._
import org.bson.types.BasicBSONList
import scala.util.Random
import cc.factorie.la.{Tensor2, SparseTensor}

/**
 * Created by beroth on 1/30/15.
 */
/**
 * Holds a generic matrix that can be written to MongoDB.
 */
class CoocMatrix(var _numRows: Int, var _numCols:Int) extends MongoWritable {
  val rows = new mutable.HashMap[Int, mutable.HashMap[Int, Double]]
  // Backpointers for efficiency
  private var __cols = new mutable.HashMap[Int, mutable.Set[Int]]
  // number of non-zero cells
  private var __nnz = 0

  def copy(): CoocMatrix = {
    val m = new CoocMatrix(numRows(), numCols())
    getNnzCells().foreach(cell => {
      val rowNr = cell._1
      val colNr = cell._2
      val cellVal = get(rowNr, colNr)
      m.set(rowNr, colNr, cellVal)
    })
    m
  }

  def set(rowNr: Int, colNr: Int, cellValue: Double) {
    if (cellValue == 0 && get(rowNr, colNr) != 0) {
      // Keeping track of non-zero elements.
      __nnz -= 1
      // Data structures are only keeping keys to rows and columns if they contain non-zero elements:
      val row = rows.get(rowNr).get
      row.remove(colNr)
      if (row.isEmpty) {
        rows.remove(rowNr)
      }
      val col = __cols.get(colNr).get
      col-=rowNr
      if (col.isEmpty) {
        __cols.remove(colNr)
      }
    } else if (cellValue != 0) {
      if (get(rowNr, colNr) == 0) {
        __nnz += 1
      }
      val row = rows.getOrElseUpdate(rowNr, new mutable.HashMap[Int, Double]())
      row.update(colNr,cellValue)

      val col = __cols.getOrElseUpdate(colNr, new mutable.HashSet[Int]())
      col+=rowNr
    }

    // In any case grow the dimensions if necessary.
    if (rowNr + 1 > _numRows) _numRows = rowNr + 1
    if (colNr + 1 > _numCols) _numCols = colNr + 1
  }

  def nnz() = __nnz

  def nonZeroCols() = __cols.keySet

  /**
   * This is similar to an '==' method, however, we don't override it, since it relies on mutable fields (which may pose
   * problems e.g. in hash maps)
   *
   *   @param m2
   */
  def hasSameContent(m2: CoocMatrix ): Boolean = {
    return m2.numRows() == _numRows && m2.numCols() == _numCols && m2.getRows() == rows && m2.getCols() == __cols
  }

  def getRow(rowNr: Int): mutable.HashMap[Int, Double] = {
    rows.getOrElse(rowNr, new mutable.HashMap[Int, Double])
  }

  def get(rowNr: Int, colNr: Int) = {
    getRow(rowNr).getOrElse(colNr, 0.0)
  }

  def numRows() = _numRows
  def numCols() = _numCols

  protected def getRows() = rows
  protected def getCols() = __cols

  /**
   * Filters the matrix depending on row and column thresholds for the minimum number of non-zero cells.
   *
   * The filtering steps are:
   *  1. remove rows with number of cells <= tRow and columns with number of cells <= tCol
   *  2. find biggest connected component
   *  3. remove columns not in biggest connected component
   *  4. remove rows without (valid) column entries
   *  5. re-assign column and row numbering
   *
   *  It returns the pruned matrix, as well as mappings from old to new row and column indices.
   *
   * @param tRow
   * @param tCol
   */
  def prune(tRow: Int = 2, tCol: Int = 2): (CoocMatrix, Map[Int, Int], Map[Int, Int]) = {

    val marks = Array.fill[Int](_numRows + _numCols)(0)
    var components = 0

    val compToSize = mutable.Map[Int,Int]()

    for (r <- 0 until _numRows) {
      if (marks(r) == 0) {
        // Found a row r that has not been assigned to a component yet.
        // Start a new component and do a breadth-first search to mark all rows and columns connected to it.
        components += 1
        compToSize(components) = 1

        // Queue to hold all rows/columns to be marked.
        val q = new mutable.Queue[Int]()
        marks(r) = components
        q.enqueue(r)

        while (!q.isEmpty) {
          val v : Int = q.dequeue()
          if (rows.contains(v) ||
              __cols.contains(v - _numRows) ){
            var offset = 0
            // Current row/column has outgoing pointers.
            val adj: Iterable[Int] = if (v < _numRows) {
              // get the columns; use offset
              offset = _numRows
              rows.get(v).get.keys.filter(c => __cols.get(c).get.size > tCol)
            } else {
              __cols.get(v - _numRows).get.filter(r => rows.get(r).get.size > tRow)
              // get the rows; no offset
            }
            for  (a <- adj; if marks(a + offset) == 0) {
              marks(a + offset) = components
              compToSize(components) += 1
              q.enqueue(a + offset)
            }
          }
        } // marked all rows/columns belonging to new component
      }
    } // checked that all rows were marked

    val maxComp = compToSize.maxBy(_._2)._1

    // Build new, smaller matrix
    val prunedMatrix = new CoocMatrix(0,0) // grow on the fly

    val oldToNewCols = mutable.Map[Int,Int]()
    var newColIdx = 0
    for (c <- 0 until _numCols;
         if marks(c + _numRows) == maxComp) {
      oldToNewCols(c) = newColIdx
      newColIdx += 1
    }

    val oldToNewRows = mutable.Map[Int,Int]()
    var newRowIdx = 0
    for (r <- 0 until _numRows;
         if marks(r) == maxComp) {
      oldToNewRows.put(r, newRowIdx)
      for ((col, cellVal) <- getRow(r);
           if oldToNewCols.contains(col)) {
        prunedMatrix.set(newRowIdx, oldToNewCols.get(col).get, cellVal)
      }
      newRowIdx += 1
    }
    return (prunedMatrix, oldToNewRows.toMap, oldToNewCols.toMap)
  }

  /**
   * This returns a training matrix, a development matrix and a test matrix.
   * The three matrices build are partitioning of the non-zero cells of the original matrix.
   *
   * It is recommended to use this method on a pruned matrix, to increase connectedness/density of the matrix
   * cells. (Be aware that indices change after pruning, and adapt testRows and testCols accordingly).
   *
   * The dev and test matrices are sampled by a scheme that ensures that after their removal their are still
   * non-zero elements in the respective rows and columns.
   *
   * If there are not enough non-zero entries that satisfy this criterion the test matrix (or even the dev matrix) will
   * be smaller than specified.
   */
  def randomTestSplit(numDevNNZ: Int, numTestNNZ: Int, testRows: Option[Set[Int]] = None,
                              testCols: Option[Set[Int]] = None, random:Random = new Random(0)): (CoocMatrix, CoocMatrix, CoocMatrix) = {


    // first sort, then shuffle (wrt the seed) -- in order to reproduce exactly the same ordering, no matter what
    // the underlying ordering of the data
    val shuffledCells = random.shuffle(getNnzCells()
      .filter(cell => (testRows == None || testRows.get.contains(cell._1)))
      .filter(cell => (testCols == None || testCols.get.contains(cell._2))).sorted)

    val trainMatrix = this.copy
    // TODO: set dimensions
    val devMatrix = new CoocMatrix(numRows(), numCols())
    val testMatrix = new CoocMatrix(numRows(), numCols())

    // Consume shuffled cells and build dev test matrices.
    shuffledCells.toStream.takeWhile(_ => (devMatrix.nnz() < numDevNNZ || testMatrix.nnz() < numTestNNZ)).foreach(cell => {
      val matrixToGrow = if (devMatrix.nnz() < numDevNNZ) {
        devMatrix
      } else {
        testMatrix
      }
      val rowNr = cell._1
      val colNr = cell._2
      if (trainMatrix.rows.get(rowNr).get.size > 1 && trainMatrix.__cols.get(colNr).get.size > 1) {
        matrixToGrow.set(rowNr, colNr, get(rowNr, colNr))
        trainMatrix.set(rowNr, colNr, 0)
      }
    })
    (trainMatrix, devMatrix, testMatrix)
  }


  def getNnzCells(): Seq[(Int, Int)] = {
    for((k1, v1) <- rows.toSeq; k2 <- v1.keys) yield (k1, k2)
  }

  /*
  This writes the matrix content out so that it coresponds to the following schema:

db.rows: {
nr: <INT>
cols: [<INT>]
vals: [<DOUBLE>]
}
 */
  def collectionName: String = MongoWritable.COOC_MATRIX_PREFIX

  def writeToMongo(mongoDb: DB) {
    val collection: DBCollection = mongoDb.getCollection(collectionName)
    collection.drop()
    val builder = collection.initializeUnorderedBulkOperation();
    for (row <- getRows) {
      val rowNr = row._1
      val colsCellVals : mutable.HashMap[Int, Double] = row._2

      // TODO: here we a re using the java mongo drivers -- maybe we should go over to using the Scala Casbah drivers.
      val mongoCols = new BasicBSONList
      colsCellVals.map(_._1).zipWithIndex.foreach(c => mongoCols.put(c._2, c._1))
      val mongoVals = new BasicBSONList
      colsCellVals.map(_._2).zipWithIndex.foreach(c => mongoVals.put(c._2, c._1))

      val rowObject = new BasicDBObject
      rowObject.put(CoocMatrix.ROW_NR, rowNr)
      rowObject.put(CoocMatrix.COL_LIST, mongoCols)
      rowObject.put(CoocMatrix.CELL_VAL_LIST, mongoVals)
      //collection.insert(rowObject)
      builder.insert(rowObject);
    }
    builder.execute()
  }

  def populateFromMongo(mongoDb: DB) {
    val collection: DBCollection = mongoDb.getCollection(collectionName)
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val rowObject: DBObject = cursor.next()
        val rowNr = rowObject.get(CoocMatrix.ROW_NR).asInstanceOf[Int]
        val mongoCols: BasicDBList = rowObject.get(CoocMatrix.COL_LIST).asInstanceOf[BasicDBList]
        val mongoVals: BasicDBList = rowObject.get(CoocMatrix.CELL_VAL_LIST).asInstanceOf[BasicDBList]
        assert(mongoCols.size() == mongoVals.size())
        for (i <- 0 until mongoCols.size()) {
          val colNr = mongoCols.get(i).asInstanceOf[Int]
          val cellVal = mongoVals.get(i).asInstanceOf[Double]
          this.set(rowNr, colNr, cellVal)
        }
      }
    } finally {
      cursor.close();
    }
  }

  /*
This writes the matrix content out so that it coresponds to the following schema:

db.cells: {
row: <INT>
col: <INT>
val: <DOUBLE>
}

  def writeToMongoCellBased(mongoDb: DB, dropCollection: Boolean = true) {
    val collection: DBCollection = mongoDb.getCollection(MongoWritable.COOC_MATRIX_PREFIX)
    if (dropCollection) {
      collection.drop()
    }

    val builder = collection.initializeUnorderedBulkOperation();

    for (row <- getRows) {
      val rowNr = row._1
      val colsCellVals : mutable.HashMap[Int, Double] = row._2

      // TODO: here we a re using the java mongo drivers -- maybe we should go over to using the Scala Casbah drivers.
      colsCellVals.foreach(colAndCellVal => {
        val cellObject = new BasicDBObject
        cellObject.put(CoocMatrix.ROW_NR, rowNr)
        cellObject.put(CoocMatrix.COL_NR, colAndCellVal._1)
        cellObject.put(CoocMatrix.CELL_VAL, colAndCellVal._2)
        //collection.insert(cellObject)
        builder.insert(cellObject);
      })
    }
    builder.execute()
  }
  */
}




object CoocMatrix {
  //val CELLS_COLLECTION = "cells"
  val ROW_NR = "row_nr"
  val COL_NR = "col_nr"
  val CELL_VAL = "val"
  val COL_LIST = "cols"
  val CELL_VAL_LIST = "vals"
/*
  def fromMongoCellBased(mongoDb: DB) : CoocMatrix = {
    val collection: DBCollection = mongoDb.getCollection(CELLS_COLLECTION)
    val m = new CoocMatrix(0, 0) // grow on the fly
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val cellObject: DBObject = cursor.next()
        val rowNr = cellObject.get(ROW_NR).asInstanceOf[Int]
        val colNr = cellObject.get(COL_NR).asInstanceOf[Int]
        val cellVal = cellObject.get(CELL_VAL).asInstanceOf[Double]
        m.set(rowNr, colNr, cellVal)
      }
    } finally {
      cursor.close();
    }
    m
  }

  */

  def fromTensor2(t:Tensor2 with SparseTensor):CoocMatrix = {
    t._makeReadable()
    val m = new CoocMatrix(0, 0) // grow on the fly
    t.foreachActiveElement{ case(i, value) =>
      val col = i%t.dim1
      val row = i/t.dim2
      m.set(row, col, value)
    }
    m
  }

  def randomOneZeroMatrix(numRows: Int, numCols: Int, maxNnz:Int, random:Random = new Random(0), underlyingTopics: Int = 1, noiseRatio: Double = 0.1): CoocMatrix = {
    val m = new CoocMatrix(numRows, numCols)
    (1 to maxNnz).foreach( _ => {
      val rowNr = random.nextInt(numRows)
      // rows and columns belong to classes according to their modulo value.
      val rowClass = rowNr % underlyingTopics

      // sample the class for the column: with probability (1-noiseRatio) it is the same as for the row
      val colClass = if (random.nextDouble() > noiseRatio || underlyingTopics == 1) {
        rowClass
      } else {
        // sample from other classes
        (rowClass + 1 + random.nextInt(underlyingTopics - 1)) % underlyingTopics
      }
      // shift column indices, so that column falls into equivalence class 0, and pick random index
      val randomMultiple = random.nextInt((numCols - colClass - 1) / underlyingTopics + 1)
      val colNr = randomMultiple * underlyingTopics + colClass
      assert(colNr < numCols)
      m.set(rowNr, colNr, 1.0)
    })
    m
  }
}

