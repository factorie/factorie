package cc.factorie.app.uschema

import com.mongodb._
import scala.Some

/**
 * Created by beroth on 2/6/15.
 */
/**
 * Holds a knowledge-base with an underlying matrix.
 * I.e. additionally to matrix information, it also stores information about entities, relations etc.
 */

class EntityRelationKBMatrix(val matrix:CoocMatrix = new CoocMatrix(0,0),
               val __rowMap: EntityPairMemoryMap = new EntityPairMemoryMap(collectionPrefix = MongoWritable.ENTITY_ROW_MAP_PREFIX),
               val __colMap: StringMemoryIndexMap = new StringMemoryIndexMap(collectionPrefix = MongoWritable.ENTITY_COL_MAP_PREFIX)
                              ) extends KBMatrix[EntityRelationKBMatrix, EntityPair, String] with MongoWritable {

  def cloneWithNewCells(cells: CoocMatrix): EntityRelationKBMatrix = {
    new EntityRelationKBMatrix(matrix = cells, __rowMap = this.__rowMap, __colMap = this.__colMap)
  }

  def createEmptyMatrix(): EntityRelationKBMatrix = {
    new EntityRelationKBMatrix()
  }

  def writeToMongo(mongoDb: DB) {
    matrix.writeToMongo(mongoDb)
    __rowMap.writeToMongo(mongoDb)
    __colMap.writeToMongo(mongoDb)
  }

  def populateFromMongo(mongoDb: DB) {
    matrix.populateFromMongo(mongoDb)
    __rowMap.populateFromMongo(mongoDb)
    __colMap.populateFromMongo(mongoDb)
  }
}


object EntityRelationKBMatrix {

  private def entitiesAndRelFromLine(line: String, colsPerEnt:Int): (EntityPair, String, Double) = {
    val parts = line.split("\t")
    val e1 : String = parts.slice(0, colsPerEnt).mkString("\t")
    val e2 : String = parts.slice(colsPerEnt, 2 * colsPerEnt).mkString("\t")
    val rel : String = parts.slice(2 * colsPerEnt, parts.length - 1).mkString("\t")
    val cellVal : Double = parts(parts.length - 1).toDouble
    (EntityPair(e1, e2), rel, cellVal)
  }
  // Loads a matrix from a tab-separated file
  def fromTsv(filename:String, colsPerEnt:Int = 2) : EntityRelationKBMatrix = {
    val kb = new EntityRelationKBMatrix()
    scala.io.Source.fromFile(filename).getLines.foreach(line => {
      val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
      kb.set(ep, rel, cellVal)
    })
    kb
  }
}
