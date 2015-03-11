package cc.factorie.epistemodb

import com.mongodb.DB

/**
 * Created by beroth on 3/9/15.
 */
trait MongoWritable {
  def collectionPrefix: Option[String] = None
  def writeToMongo(db: Option[DB] = None, collectionPrefix: Option[String] = None)
}

object MongoWritable {
  // Some constants that should be used for specifying the collection names MongoWritables are written to.
  // Prefixes are passed via the collectionPrefix field.
  // The writeToMongo method may choose to write to more collections, concatenating the prefix with some of the suffixes.
  // TOD: ?? A suffix can be either passed as an argument to writeToMongo, or set programatically.

  // Prefixes must not contain the underscore "_" symbol.
  val ENTITY_ROW_MAP_PREFIX = "entityRowMap"
  val ENTITY_COL_MAP_PREFIX = "entityColMap"
  val COOC_MATRIX_PREFIX = "coocMatrix"

}