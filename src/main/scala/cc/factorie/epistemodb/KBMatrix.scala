package cc.factorie.epistemodb

import scala.collection.mutable
import com.google.common.collect._
import scala.collection.JavaConverters._
import com.mongodb._
import org.bson.types.BasicBSONList
import scala.Some
import scala.Some

/**
 * Created by beroth on 2/6/15.
 */
/**
 * Holds a knowledge-base with an underlying matrix.
 * I.e. additionally to matrix information, it also stores information about entities, relations etc.
 */
class KBMatrix(__matrix:DBMatrix = new DBMatrix,
               __entityMap:HashBiMap[String, Int] = HashBiMap.create[String, Int](),
               __rowMap:HashBiMap[(Int, Int), Int] = HashBiMap.create[(Int, Int), Int](),
               __colMap:HashBiMap[String, Int] = HashBiMap.create[String, Int]() ) {

  // entity pair -> row nr
  //protected def _initialRowMap = HashBiMap.create[(Int, Int), Int]()
  //val __rowMap = _initialRowMap

  // relation -> column number.
  // protected def _initialColMap = HashBiMap.create[String, Int]()
  //  val __colMap = _initialColMap

  // entity -> entity number
  //protected def _initialEntityMap = HashBiMap.create[String, Int]()
  //   val __entityMap = _initialEntityMap

  // kb relations (belonging to a preset schema)
  protected def _initialKbRels = mutable.HashSet[Int]()

  private def getEid(eStr: String): Option[Int] = if (__entityMap.containsKey(eStr)) {
    Some(__entityMap.get(eStr))
  } else {
    None
  }

  private def getRowNr(entities: (Int, Int)): Option[Int] = if (__rowMap.containsKey(entities)) {
    Some(__rowMap.get(entities))
  } else {
    None
  }

  private def getColNr(relStr: String): Option[Int] = if (__colMap.containsKey(relStr)) {
    Some(__colMap.get(relStr))
  } else {
    None
  }

  private def getEidOrCreate(eStr: String): Int = if (__entityMap.containsKey(eStr)) {
    __entityMap.get(eStr)
  } else {
    val eId = __entityMap.size()
    __entityMap.forcePut(eStr, eId)
    eId
  }

  private def getRowNrOrCreate(entities: (Int, Int)): Int = if (__rowMap.containsKey(entities)) {
    __rowMap.get(entities)
  } else {
    val rowNr = __rowMap.size()
    __rowMap.forcePut(entities, rowNr)
    rowNr
  }

  private def getColNrOrCreate(relStr: String): Int = if (__colMap.containsKey(relStr)) {
    __colMap.get(relStr)
  } else {
    val colNr = __colMap.size()
    __colMap.forcePut(relStr, colNr)
    colNr
  }

  def getRows() : Iterator[(String, String)] = {
    __rowMap.keySet().iterator().asScala.map(idPair =>
      (__entityMap.inverse().get(idPair._1), __entityMap.inverse().get(idPair._2)))
  }

  private def getColsForRow(entityPair: (String, String)): Iterable[String] = {
    val eids = (getEid(entityPair._1), getEid(entityPair._2))

    eids match {
      case (Some(e1Id), Some(e2Id)) => {
        val rowNr = __rowMap.get((e1Id, e2Id))
        __matrix.getRow(rowNr).map(cell => __colMap.inverse().get(cell._1))
      }
      case _ => List()
    }
  }

  def set(e1: String, e2: String, rel: String, cellVal: Double) {
    val e1Id = getEidOrCreate(e1)
    val e2Id = getEidOrCreate(e2)
    val rowNr = getRowNrOrCreate((e1Id, e2Id))
    val colNr = getColNrOrCreate(rel)
    __matrix.set(rowNr, colNr, cellVal)
  }

  def get(e1: String, e2: String, rel: String): Double = {
    val e1IdOption = getEid(e1)
    val e2IdOption = getEid(e2)
    (e1IdOption, e2IdOption) match {
      case (Some(e1Id), Some(e2Id)) => {
        val rowIdOption = getRowNr((e1Id, e2Id))
        rowIdOption match {
          case Some(rowId) => {

            val colIdOption = getColNr(rel)
            colIdOption match {
              case Some(colId) => {
                __matrix.get(rowId, colId)
              }
              case None => 0.0
            }
          }
          case None => 0.0
        }
      }
      case _ => 0.0
    }
  }

  def numRows(): Int = __matrix.numRows()
  def numCols(): Int = __matrix.numCols()

  def hasSameContent(m2: KBMatrix ): Boolean = {
    m2.numRows() == numRows() &&
      m2.numCols() == numCols() &&
      getRows().forall(ep => {
        getColsForRow(ep).forall(rel => get(ep._1, ep._2, rel) == m2.get(ep._1, ep._2, rel))
      })
  }


  private def writeEntityMap(mongoDb: DB) {
    // write entity map
    val entityCollection = mongoDb.getCollection(KBMatrix.ENTITY_COLLECTION)
    for((e,id) <- __entityMap.asScala) {
      //val id = __entityMap.get(e)
      val entityObject = new BasicDBObject
      entityObject.put(KBMatrix.ENTITY_ID, id)
      entityObject.put(KBMatrix.ENTITY_SURFACE, e)
      entityCollection.insert(entityObject)
    }
  }

  private def writeRowMap(mongoDb: DB) {
    val rowMapCollection = mongoDb.getCollection(KBMatrix.ROWMAP_COLLECTION)
    // write row map
    for((ep, id) <- __rowMap.asScala) {
      //val id = __rowMap.get(ep)
      val rowObject = new BasicDBObject
      rowObject.put(KBMatrix.ENTITY1, ep._1)
      rowObject.put(KBMatrix.ENTITY2, ep._2)

      rowObject.put(KBMatrix.ROW_ID, id)
      rowMapCollection.insert(rowObject)
    }
  }

  private def writeColumnMap(mongoDb: DB) {
    val colMapCollection = mongoDb.getCollection(KBMatrix.COLMAP_COLLECTION)
    // write column map
    for ((rel, id) <- __colMap.asScala) {
      //val id = __colMap.get(rel)
      val colObj = new BasicDBObject
      colObj.put(KBMatrix.COL_ID, id)
      colObj.put(KBMatrix.RELATION, rel)
      colMapCollection.insert(colObj)
    }
  }

  def writeToMongo(mongoDb: DB) {
    __matrix.writeToMongo(mongoDb)
    writeEntityMap(mongoDb)
    writeRowMap(mongoDb)
    writeColumnMap(mongoDb)
  }
}

object KBMatrix {

  val ENTITY_COLLECTION = "entities"
  val ENTITY_SURFACE = "surface"
  val ENTITY_ID = "_id"
  val ROWMAP_COLLECTION = "rowmap"
  val ENTITY1 = "e1"
  val ENTITY2 = "e2"
  val ROW_ID = "_id"
  val COLMAP_COLLECTION = "colmap"
  val COL_ID = "_id"
  val RELATION = "rel"

  private def readRowMap(mongoDb: DB): HashBiMap[(Int, Int), Int] = {
    val collection = mongoDb.getCollection(KBMatrix.ROWMAP_COLLECTION)
    val rowMap = HashBiMap.create[(Int, Int), Int]()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val rowObject: DBObject = cursor.next()
        val rowNr = rowObject.get(KBMatrix.ROW_ID).asInstanceOf[Int]
        val e1: Int = rowObject.get(KBMatrix.ENTITY1).asInstanceOf[Int]
        val e2: Int = rowObject.get(KBMatrix.ENTITY2).asInstanceOf[Int]
        rowMap.put((e1, e2), rowNr)
      }
    } finally {
      cursor.close();
    }
    rowMap
  }

  private def readEntityMap(mongoDb: DB): HashBiMap[String, Int] = {
    val collection = mongoDb.getCollection(KBMatrix.ENTITY_COLLECTION)
    val entityMap = HashBiMap.create[String, Int]()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val entityObject: DBObject = cursor.next()
        val id: Int = entityObject.get(KBMatrix.ENTITY_ID).asInstanceOf[Int]
        val surface: String = entityObject.get(KBMatrix.ENTITY_SURFACE).asInstanceOf[String]
        entityMap.put(surface, id)
      }
    } finally {
      cursor.close();
    }
    entityMap
  }

  private def readColMap(mongoDb: DB): HashBiMap[String, Int] =  {
    val collection = mongoDb.getCollection(KBMatrix.COLMAP_COLLECTION)
    val colMap = HashBiMap.create[String, Int]()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val colObject: DBObject = cursor.next()
        val id: Int = colObject.get(KBMatrix.COL_ID).asInstanceOf[Int]
        val rel: String = colObject.get(KBMatrix.RELATION).asInstanceOf[String]
        colMap.put(rel, id)
      }
    } finally {
      cursor.close();
    }
    colMap
  }

  def fromMongo(mongoDb: DB) : KBMatrix = {
    val dbMatrix = DBMatrix.fromMongo(mongoDb)
    val entityMap = readEntityMap(mongoDb)
    val rowMap = readRowMap(mongoDb)
    val colMap = readColMap(mongoDb)
    val m = new KBMatrix(dbMatrix, entityMap, rowMap, colMap)
    m
  }
}
