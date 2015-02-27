package cc.factorie.epistemodb

import scala.collection.mutable
import com.google.common.collect._
import scala.collection.JavaConverters._
import com.mongodb._
import scala.Some
import scala.util.Random
import cc.factorie.app.nlp.DocumentAnnotator
import cc.factorie.app.nlp.relation.RelationMentionList
import cc.factorie.app.nlp.{Document=>FactorieDocument}

/**
 * Created by beroth on 2/6/15.
 */
/**
 * Holds a knowledge-base with an underlying matrix.
 * I.e. additionally to matrix information, it also stores information about entities, relations etc.
 */

// TODO: get trait and implementations for memory- and mongo-backed entity map
//trait EntityMap extends ... {...}

trait XDocCorefSystem {
  // TODO
}

trait KbDocuments {
  type EntityMap = HashBiMap[String, Int]
  // - build the doc table
  // - build xDocMentions table
  def addDoc(doc: FactorieDocument)

  def documents: Iterator[FactorieDocument]

  // - use xDocMentions table (and maybe doc table)
  // - populates __entityMap
  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem)
}

class MongoDocuments extends KbDocuments {
  def addDoc(doc: FactorieDocument) {
    throw new UnsupportedOperationException
  }

  def documents: Iterator[FactorieDocument] = {
    throw new UnsupportedOperationException
  }

  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem) {
    throw new UnsupportedOperationException
  }
}

class MemoryDocuments extends KbDocuments {
  def addDoc(doc: FactorieDocument) {
    throw new UnsupportedOperationException
  }

  def documents: Iterator[FactorieDocument] = {
    throw new UnsupportedOperationException
  }

  def doXDocCoref(em: EntityMap, xcs: XDocCorefSystem) {
    throw new UnsupportedOperationException
  }
}


trait RelationDocumentAnnotator extends DocumentAnnotator {
  def postAttrs = Seq(classOf[RelationMentionList])
//  def process(doc:Document)
}

/*
trait relationMentionExtractor {
  getRelationMentions(doc): List[RelationMention]
}*/


class KBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
               __entityMap: HashBiMap[String, Int] = HashBiMap.create[String, Int](),
               __rowMap:HashBiMap[(Int, Int), Int] = HashBiMap.create[(Int, Int), Int](),
               __colMap:HashBiMap[String, Int] = HashBiMap.create[String, Int](),
               __docs: KbDocuments = new MemoryDocuments) {
  // TODO: make maps either memory-backed or mongo backed.

  def populateRelationMentions(relAnnotator: RelationDocumentAnnotator) {
    new UnsupportedOperationException
  }

  def doXDocCoref(xcs: XDocCorefSystem) {
    __docs.doXDocCoref(__entityMap, xcs)
  }

  // entity pair -> row nr
  //protected def _initialRowMap = HashBiMap.create[(Int, Int), Int]()
  //val __rowMap = _initialRowMap

  // relation -> column number.
  // protected def _initialColMap = HashBiMap.create[String, Int]()
  //  val __colMap = _initialColMap

  // entity -> entity number
  //protected def _initialEntityMap = HashBiMap.create[String, Int]()
  //   val __entityMap = _initialEntityMap

  def addDocs(doc: FactorieDocument) {
    __docs.addDoc(doc)
  }

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
        matrix.getRow(rowNr).map(cell => __colMap.inverse().get(cell._1))
      }
      case _ => List()
    }
  }

  def set(e1: String, e2: String, rel: String, cellVal: Double) {
    val e1Id = getEidOrCreate(e1)
    val e2Id = getEidOrCreate(e2)
    val rowNr = getRowNrOrCreate((e1Id, e2Id))
    val colNr = getColNrOrCreate(rel)
    matrix.set(rowNr, colNr, cellVal)
  }

  def nnz() = matrix.nnz()

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
                matrix.get(rowId, colId)
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

  def numRows(): Int = matrix.numRows()
  def numCols(): Int = matrix.numCols()

  def hasSameContent(m2: KBMatrix ): Boolean = {
    m2.numRows() == numRows() &&
      m2.numCols() == numCols() &&
      getRows().forall(ep => {
        getColsForRow(ep).forall(rel => get(ep._1, ep._2, rel) == m2.get(ep._1, ep._2, rel))
      })
  }


  private def writeEntityMap(mongoDb: DB, dropCollection: Boolean = true) {
    // write entity map
    val entityCollection = mongoDb.getCollection(KBMatrix.ENTITY_COLLECTION)

    if (dropCollection) {
      entityCollection.drop()
    }

    //val builder = entityCollection.initializeUnorderedBulkOperation()

    for((e,id) <- __entityMap.asScala) {
      //val id = __entityMap.get(e)
      val entityObject = new BasicDBObject
      entityObject.put(KBMatrix.ENTITY_ID, id)
      entityObject.put(KBMatrix.ENTITY_SURFACE, e)
      entityCollection.insert(entityObject)
      //builder.insert(entityObject)
    }
    //builder.execute()
  }

  private def writeRowMap(mongoDb: DB, dropCollection: Boolean = true) {
    val rowMapCollection = mongoDb.getCollection(KBMatrix.ROWMAP_COLLECTION)

    if (dropCollection) {
      rowMapCollection.drop()
    }

    //val builder = rowMapCollection.initializeUnorderedBulkOperation()

    // write row map
    for((ep, id) <- __rowMap.asScala) {
      //val id = __rowMap.get(ep)
      val rowObject = new BasicDBObject
      rowObject.put(KBMatrix.ENTITY1, ep._1)
      rowObject.put(KBMatrix.ENTITY2, ep._2)

      rowObject.put(KBMatrix.ROW_ID, id)
      rowMapCollection.insert(rowObject)
      //builder.insert(rowObject)
    }
    //builder.execute()
  }

  private def writeColumnMap(mongoDb: DB, dropCollection: Boolean = true) {
    val colMapCollection = mongoDb.getCollection(KBMatrix.COLMAP_COLLECTION)

    if (dropCollection) {
      colMapCollection.drop()
    }

    //val builder = colMapCollection.initializeUnorderedBulkOperation()

    // write column map
    for ((rel, id) <- __colMap.asScala) {
      //val id = __colMap.get(rel)
      val colObj = new BasicDBObject
      colObj.put(KBMatrix.COL_ID, id)
      colObj.put(KBMatrix.RELATION, rel)
      colMapCollection.insert(colObj)
      //builder.insert(colObj)
    }
    //builder.execute()
  }

  def writeToMongo(mongoDb: DB, dropCollections: Boolean = true) {
    // TODO: undo bulk writing
    matrix.writeToMongo(mongoDb, dropCollections)
    writeEntityMap(mongoDb, dropCollections)
    writeRowMap(mongoDb, dropCollections)
    writeColumnMap(mongoDb, dropCollections)
  }

  def writeToMongoCellBased(mongoDb: DB, dropCollections: Boolean = true) {
    // TODO: undo bulk writing
    matrix.writeToMongoCellBased(mongoDb, dropCollections)
    writeEntityMap(mongoDb, dropCollections)
    writeRowMap(mongoDb, dropCollections)
    writeColumnMap(mongoDb, dropCollections)
  }

  /* use prune(0,0) for no pruning
   * use prune(2,1) for moderate pruning on kb matrices
   */
  def prune(tRow: Int = 2, tCol: Int = 2): KBMatrix = {
    val (prunedMatrix, oldToNewRow, oldToNewCol) = matrix.prune(tRow, tCol)
    val newKb = new KBMatrix()

    val newToOldCol = oldToNewCol.map(_ swap)
    val newToOldRow = oldToNewRow.map(_ swap)

    for (rowNr <- 0 until prunedMatrix.numRows()) {
      for((colNr, cellVal) <- prunedMatrix.getRow(rowNr)) {
        // Get the row from this matrix that corresponds to the new (filtered) row we are looking at.
        val eIdxs = __rowMap.inverse().get(newToOldRow.get(rowNr).get)
        // Get the corresponding entity strings.
        val e1Str = __entityMap.inverse().get(eIdxs._1)
        val e2Str = __entityMap.inverse().get(eIdxs._2)
        // The the column (relation) that corresponds to the filtered cell we are looking at.
        val relStr = __colMap.inverse().get(newToOldCol.get(colNr).get)
        newKb.set(e1Str, e2Str, relStr, cellVal)
      }
    }
    newKb
  }


  def randomTestSplit(numDevNNZ: Int, numTestNNZ: Int, testRows: Option[Set[(String, String)]] = None,
                      testCols: Option[Set[String]] = None, random:Random = new Random(0)): (KBMatrix, KBMatrix, KBMatrix) = {

    val testRowIndices = testRows match {
      case Some(entityNamePairs) => Some( entityNamePairs.map(pairStr =>
        (__entityMap.get(pairStr._1), __entityMap.get(pairStr._2))).map(pairInt => __rowMap.get(pairInt)).toSet )
      case None => None
    }

    val testColIndices = testCols match {
      case Some(colNames) => Some( colNames.map(name => __colMap.get(name)).toSet )
      case None => None
    }

    val (trainCooc, devCooc, testCooc) =
      matrix.randomTestSplit(numDevNNZ, numTestNNZ, testRowIndices, testColIndices, random)

    val trainKB = new KBMatrix(trainCooc, __entityMap, __rowMap, __colMap)
    val devKB = new KBMatrix(devCooc, __entityMap, __rowMap, __colMap)
    val testKB = new KBMatrix(testCooc, __entityMap, __rowMap, __colMap)

    (trainKB, devKB, testKB)
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

  private def readColMap(mongoDb: DB): HashBiMap[String, Int] = {
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
    val dbMatrix = CoocMatrix.fromMongo(mongoDb)
    val entityMap = readEntityMap(mongoDb)
    val rowMap = readRowMap(mongoDb)
    val colMap = readColMap(mongoDb)
    val m = new KBMatrix(dbMatrix, entityMap, rowMap, colMap)
    m
  }

  def fromMongoCellBased(mongoDb: DB) : KBMatrix = {
    val dbMatrix = CoocMatrix.fromMongoCellBased(mongoDb)
    val entityMap = readEntityMap(mongoDb)
    val rowMap = readRowMap(mongoDb)
    val colMap = readColMap(mongoDb)
    val m = new KBMatrix(dbMatrix, entityMap, rowMap, colMap)
    m
  }

  private def entitiesAndRelFromLine(line: String, colsPerEnt:Int): (String, String, String, Double) = {
    val parts = line.split("\t")
    val e1 : String = parts.slice(0, colsPerEnt).mkString("\t")
    val e2 : String = parts.slice(colsPerEnt, 2 * colsPerEnt).mkString("\t")
    val rel : String = parts.slice(2 * colsPerEnt, parts.length - 1).mkString("\t")
    val cellVal : Double = parts(parts.length - 1).toDouble
    (e1, e2, rel, cellVal)
  }
  // Loads a matrix from a tab-separated file
  def fromTsv(filename:String, colsPerEnt:Int = 2) : KBMatrix = {
    val kb = new KBMatrix()
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (e1, e2, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(e1, e2, rel, cellVal)
    })
    kb
  }
}
