package cc.factorie.epistemodb

import scala.collection.mutable
import com.mongodb._
import org.bson.types.BasicBSONList
import scala.util.Random


/**
 * Created by beroth on 1/30/15.
 */
/**
 * Holds a generic matrix that can be written to MongoDB.
 */
class CoocMatrix {
  protected def _initialRowMap = new mutable.HashMap[Int, mutable.HashMap[Int, Double]]
  // Backpointers for efficiency
  protected def _initialColMap = new mutable.HashMap[Int, mutable.Set[Int]]

  private var __rows = _initialRowMap
  private var __cols = _initialColMap
  // number of non-zero cells
  private var __nnz = 0

  protected var _numRows = 0
  protected var _numCols = 0

  def copy(): CoocMatrix = {
    val m = new CoocMatrix
    // set dimensionality
    if (numRows() > 0 && numCols() > 0) {
      m.set(numRows()-1, numCols()-1, 0)
    }
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
      val row = __rows.get(rowNr).get
      row.remove(colNr)
      if (row.isEmpty) {
        __rows.remove(rowNr)
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
      val row = __rows.getOrElseUpdate(rowNr, new mutable.HashMap[Int, Double]())
      row.update(colNr,cellValue)

      val col = __cols.getOrElseUpdate(colNr, new mutable.HashSet[Int]())
      col+=rowNr
    }

    // In any case grow the dimensions if necessary.
    if (rowNr + 1 > _numRows) _numRows = rowNr + 1
    if (colNr + 1 > _numCols) _numCols = colNr + 1
  }

  def nnz() = __nnz

  /**
   * This is similar to an '==' method, however, we don't override it, since it relies on mutable fields (which may pose
   * problems e.g. in hash maps)
   *
   *   @param m2
   */
  def hasSameContent(m2: CoocMatrix ): Boolean = {
    return m2.numRows() == _numRows && m2.numCols() == _numCols && m2.getRows() == __rows && m2.getCols() == __cols
  }

  def getRow(rowNr: Int): mutable.HashMap[Int, Double] = {
    __rows.getOrElse(rowNr, new mutable.HashMap[Int, Double])
  }

  def get(rowNr: Int, colNr: Int) = {
    getRow(rowNr).getOrElse(colNr, 0.0)
  }

  def numRows() = _numRows
  def numCols() = _numCols

  protected def getRows() = __rows
  protected def getCols() = __cols

  /**
   * Filters the matrix depending on a column threshold t.
   *
   * The filtering steps are:
   *  1. remove rows with number of cells < tRow and columns with number of cells <= tCol
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
          if (__rows.contains(v) ||
              __cols.contains(v - _numRows) ){
            var offset = 0
            // Current row/column has outgoing pointers.
            val adj: Iterable[Int] = if (v < _numRows) {
              // get the columns; use offset
              offset = _numRows
              __rows.get(v).get.keys.filter(c => __cols.get(c).get.size > tCol)
            } else {
              __cols.get(v - _numRows).get.filter(r => __rows.get(r).get.size > tRow)
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
    val prunedMatrix = new CoocMatrix()

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
  def randomTrainDevTestSplit(numDevNNZ: Int, numTestNNZ: Int, testRows: Option[Set[Int]] = None,
                              testCols: Option[Set[Int]] = None, seed:Long = 0): (CoocMatrix, CoocMatrix, CoocMatrix) = {

    val random = new Random(seed)
    // first sort, then shuffle (wrt the seed) -- in order to reproduce exactly the same ordering, no matter what
    // the underlying ordering of the data
    val shuffledCells = random.shuffle(getNnzCells()
      .filter(cell => (testRows == None || testRows.get.contains(cell._1)))
      .filter(cell => (testCols == None || testCols.get.contains(cell._2))).sorted)

    val trainMatrix = this.copy
    // TODO: set dimensions
    val devMatrix = new CoocMatrix
    val testMatrix = new CoocMatrix

    // Consume shuffled cells and build dev test matrices.
    shuffledCells.takeWhile(_ => (devMatrix.nnz() < numDevNNZ || testMatrix.nnz() < numTestNNZ)).foreach(cell => {
      val matrixToGrow = if (devMatrix.nnz() < numDevNNZ) {
        devMatrix
      } else {
        testMatrix
      }
      val rowNr = cell._1
      val colNr = cell._2
      if (trainMatrix.__rows.get(rowNr).get.size > 1 && trainMatrix.__cols.get(colNr).get.size > 1) {
        matrixToGrow.set(rowNr, colNr, get(rowNr, colNr))
        trainMatrix.set(rowNr, colNr, 0)
      }
    })
    (trainMatrix, devMatrix, testMatrix)
  }


  def getNnzCells(): Seq[(Int, Int)] = {
    for((k1, v1) <- __rows.toSeq; k2 <- v1.keys) yield (k1, k2)
  }

  /*
  This writes the matrix content out so that it coresponds to the following schema:

db.rows: {
nr: <INT>
cols: [<INT>]
vals: [<DOUBLE>]
}
 */
  def writeToMongo(mongoDb: DB, dropCollection: Boolean = true) {
    val collection: DBCollection = mongoDb.getCollection(CoocMatrix.ROWS_COLLECTION)
    if (dropCollection) {
      collection.drop()
    }

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

  /*
This writes the matrix content out so that it coresponds to the following schema:

db.cells: {
row: <INT>
col: <INT>
val: <DOUBLE>
}
*/
  def writeToMongoCellBased(mongoDb: DB, dropCollection: Boolean = true) {
    val collection: DBCollection = mongoDb.getCollection(CoocMatrix.CELLS_COLLECTION)
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
}




object CoocMatrix {
  val CELLS_COLLECTION = "cells"
  val ROWS_COLLECTION = "rows"
  val ROW_NR = "row_nr"
  val COL_NR = "col_nr"
  val CELL_VAL = "val"
  val COL_LIST = "cols"
  val CELL_VAL_LIST = "vals"

  
  def fromMongo(mongoDb: DB) : CoocMatrix = {
    val collection: DBCollection = mongoDb.getCollection(ROWS_COLLECTION)
    val m = new CoocMatrix()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val rowObject: DBObject = cursor.next()
        val rowNr = rowObject.get(ROW_NR).asInstanceOf[Int]
        val mongoCols: BasicDBList = rowObject.get(COL_LIST).asInstanceOf[BasicDBList]
        val mongoVals: BasicDBList = rowObject.get(CELL_VAL_LIST).asInstanceOf[BasicDBList]
        assert(mongoCols.size() == mongoVals.size())
        for (i <- 0 until mongoCols.size()) {
          val colNr = mongoCols.get(i).asInstanceOf[Int]
          val cellVal = mongoVals.get(i).asInstanceOf[Double]
          m.set(rowNr, colNr, cellVal)
        }
      }
    } finally {
      cursor.close();
    }
    m
  }


  def fromMongoCellBased(mongoDb: DB) : CoocMatrix = {
    val collection: DBCollection = mongoDb.getCollection(CELLS_COLLECTION)
    val m = new CoocMatrix()
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
}
