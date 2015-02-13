package cc.factorie.epistemodb

import scala.collection.mutable
import com.mongodb._
import org.bson.types.BasicBSONList


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
   *  1. remove columns with row frequency <= t
   *  2. find biggest connected component
   *  3. remove columns not in biggest connected component
   *  4. remove rows without (valid) column entries
   *  5. re-assign column and row numbering
   *
   *  It returns the pruned matrix, as well as mappings from old to new row and column indices.
   *
   * @param t
   */
  def prune(t: Int = 2): (CoocMatrix, Map[Int, Int], Map[Int, Int]) = {

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

  /*
  This writes the matrix content out so that it coresponds to the following schema:

db.rows: {
nr: <INT>
cols: [<INT>]
vals: [<DOUBLE>]
}
 */
  def writeToMongo(mongoDb: DB, dropCollection: Boolean = true) {
    val collection: DBCollection = mongoDb.getCollection("rows")
    if (dropCollection) {
      collection.drop()
    }

    for (row <- getRows) {
      val rowNr = row._1
      val colsCellVals : mutable.HashMap[Int, Double] = row._2

      // TODO: here we a re using the java mongo drivers -- maybe we should go over to using the Scala Casbah drivers.
      val mongoCols = new BasicBSONList
      colsCellVals.map(_._1).zipWithIndex.foreach(c => mongoCols.put(c._2, c._1))
      val mongoVals = new BasicBSONList
      colsCellVals.map(_._2).zipWithIndex.foreach(c => mongoVals.put(c._2, c._1))

      val rowObject = new BasicDBObject
      rowObject.put("nr", rowNr)
      rowObject.put("cols", mongoCols)
      rowObject.put("vals", mongoVals)
      collection.insert(rowObject)
    }
  }
}


object CoocMatrix {
  def fromMongo(mongoDb: DB) : CoocMatrix = {
    val collection: DBCollection = mongoDb.getCollection("rows")
    val m = new CoocMatrix()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val rowObject: DBObject = cursor.next()
        val rowNr = rowObject.get("nr").asInstanceOf[Int]
        val mongoCols: BasicDBList = rowObject.get("cols").asInstanceOf[BasicDBList]
        val mongoVals: BasicDBList = rowObject.get("vals").asInstanceOf[BasicDBList]
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
}
