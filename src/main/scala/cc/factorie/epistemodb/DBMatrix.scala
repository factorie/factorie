package cc.factorie.epistemodb

import scala.collection.mutable
import scala.collection.immutable

/**
 * Created by beroth on 1/30/15.
 */
class DBMatrix {
  protected def _initialRowMap = new mutable.HashMap[Int, mutable.HashMap[Int, Double]]
  // Backpointers for efficiency
  protected def _initialColMap = new mutable.HashMap[Int, mutable.Set[Int]]

  private var __rows = _initialRowMap
  private var __cols = _initialColMap

  protected var _numRows = 0
  protected var _numCols = 0

  def set(rowNr: Int, colNr: Int, cellValue: Double) {
    if (rowNr + 1 > _numRows) _numRows = rowNr + 1
    if (colNr + 1 > _numCols) _numCols = colNr + 1
    val row = __rows.getOrElseUpdate(rowNr, new mutable.HashMap[Int, Double]())
    row.update(colNr,cellValue)

    val col = __cols.getOrElseUpdate(colNr, new mutable.HashSet[Int]())
    col+=rowNr
  }

  def getRow(rowNr: Int): mutable.HashMap[Int, Double] = {
    __rows.getOrElse(rowNr, new mutable.HashMap[Int, Double])
  }

  def get(rowNr: Int, colNr: Int) = {
    getRow(rowNr).getOrElse(colNr, 0.0)
  }

  def numRows() = _numRows
  def numCols() = _numCols

  /**
   * Filters the matrix depending on a column threshold t.
   *
   * The filtering steps are:
   *  1. remove columns with row frequency <= t
   *  2. find biggest connected component
   *  3. remove columns not in biggest connected component
   *  4. remove rows without (valid) column entrys
   *  5. re-assign column and row numbering
   *
   *  It returns the pruned matrix, as well as mappings from old to new row and column indices.
   *
   * @param t
   */
  def prune(t: Int = 2): (DBMatrix, immutable.Map[Int, Int], immutable.Map[Int, Int]) = {

    val marks = Array.fill[Int](_numRows + _numCols)(0)
    var components = 0

    val compToSize = mutable.Map[Int,Int]()

    for (r <- 0 until _numRows) {
      if (marks(r) == 0) {
        // Found a row r that has not been assigned to a component yet.
        // Start a new component and do a breadth-first search to mark all rows and columns connected to it.
        components += 1
        compToSize(components) = 1

        // Queue to hols all rows/columns to be marked.
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
              __rows.get(v).get.keys.filter(c => __cols.get(c).get.size > t)
            } else {
              __cols.get(v - _numRows).get
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

    println(marks.mkString(" "))
    println(maxComp)

    // Build new, smaller matrix
    val prunedMatrix = new DBMatrix()

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
}
