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

import com.google.common.collect.HashBiMap
import scala.collection.JavaConverters._
import com.mongodb._

/**
 * Created by beroth on 3/9/15.
 */
// TODO: this assumes that the key contains all the information that is necessary. Maybe distinguish between key
// information and additional information.
// TODO: get trait and implementations for memory- and mongo-backed entity map
trait MatrixIndexMap[T] {
  def keyToIndex(key: T): Int
  def indexToKey(index: Int): T
  def keyIterator: Iterator[T]
  def size: Int
  def containsKey(key: T): Boolean
  def put(key: T, value: Int)
  def add(key: T): Int
}

class MemoryIndexMap[T] extends  MatrixIndexMap[T] {
  private val _bimap: HashBiMap[T, Int] = HashBiMap.create[T, Int]()
  def _size: Int = _bimap.size()

  def keyToIndex(key: T): Int = _bimap.get(key)

  def indexToKey(index: Int): T = _bimap.inverse.get(index)

  def keyIterator: Iterator[T] = {
    _bimap.keySet().iterator().asScala
  }

  def size: Int = _size

  def containsKey(key: T): Boolean = _bimap.containsKey(key)

  def put(key: T, value: Int) = {
    _bimap.forcePut(key, value)
  }

  def add(key: T): Int = {
    if (containsKey(key)) {
      keyToIndex(key)
    } else {
      val assigned = _size
      put(key, _size)
      assigned
    }
  }
}


// TODO:
// have collectionPrefix be a field
// Either:
// (*) pass as constructor argument
// OR:
// implement/set it as val
// OR:
// set it in different traits for rows vs columns
class StringMemoryIndexMap(val collectionPrefix: String) extends MemoryIndexMap[String] with MongoWritable {

  // TODO:
  // - not have it as an option
  // - for already mongo-backed:
  // => if argument mongoDb is different that current: rewrite to mongoDb
  // => (have mongoDb default to current for mongo-backed)
  def writeToMongo(mongoDb: DB) {
    val colMapCollection = mongoDb.getCollection(collectionPrefix)
    // TODO: drop collections by default?
    colMapCollection.drop()
    val builder = colMapCollection.initializeUnorderedBulkOperation()
    // write column map
    for (rel <- this.keyIterator) {
      val id = this.keyToIndex(rel)
      //val id = __colMap.get(rel)
      val colObj = new BasicDBObject
      colObj.put(StringMemoryIndexMap.COL_ID, id)
      colObj.put(StringMemoryIndexMap.RELATION, rel)
      //colMapCollection.insert(colObj)
      builder.insert(colObj)
    }
    builder.execute()
  }

  def populateFromMongo(mongoDb: DB) {
    val collection = mongoDb.getCollection(collectionPrefix)
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val colObject: DBObject = cursor.next()
        val id: Int = colObject.get(StringMemoryIndexMap.COL_ID).asInstanceOf[Int]
        val rel: String = colObject.get(StringMemoryIndexMap.RELATION).asInstanceOf[String]
        this.put(rel, id)
      }
    } finally {
      cursor.close();
    }
  }
}

object StringMemoryIndexMap {
  val COL_ID = "colid"
  val RELATION = "rel"
}

case class EntityPair(val e1: String, val e2: String)

class EntityPairMemoryMap(private var _bimap: HashBiMap[(Int, Int), Int] = HashBiMap.create[(Int, Int), Int](),
                          var _entityMap: HashBiMap[String, Int] = HashBiMap.create[String, Int](),
                          val collectionPrefix: String) extends  MatrixIndexMap[EntityPair] with MongoWritable {

  def _size = _bimap.size()

  private def getEidOrCreate(eStr: String): Int = if (_entityMap.containsKey(eStr)) {
    _entityMap.get(eStr)
  } else {
    val eId = _entityMap.size()
    _entityMap.forcePut(eStr, eId)
    eId
  }

  private def getIntPair(ep: EntityPair): (Int, Int) = {
    if (_entityMap.containsKey(ep.e1) && _entityMap.containsKey(ep.e2)) {
      (_entityMap.get(ep.e1), _entityMap.get(ep.e2))
    } else {
      throw new IllegalArgumentException("Entity pair not contained: " + ep)
    }
  }

  private def getIntPairOrCreate(ep: EntityPair): (Int, Int) = {
    (getEidOrCreate(ep.e1), getEidOrCreate(ep.e2))
  }

  def keyToIndex(ep: EntityPair): Int = {
    _bimap.get(getIntPair(ep))
  }

  def indexToKey(index: Int): EntityPair = {
    val intPair = _bimap.inverse.get(index)
    val int2ent = _entityMap.inverse
    EntityPair(int2ent.get(intPair._1), int2ent.get(intPair._2))
  }

  def keyIterator: Iterator[EntityPair] = {
    val int2ent = _entityMap.inverse
    _bimap.keySet().iterator().asScala.map(intPair => EntityPair(int2ent.get(intPair._1), int2ent.get(intPair._2)))
  }

  def size: Int = _size

  def containsKey(key: EntityPair): Boolean = {
    _entityMap.containsKey(key.e1) && _entityMap.containsKey(key.e2) && _bimap.containsKey(getIntPair(key))
  }

  def put(key: EntityPair, value: Int) {
    val ip = getIntPairOrCreate(key)
    _bimap.forcePut(ip, value)
  }

  def add(key: EntityPair): Int = {
    if (containsKey(key)) {
      keyToIndex(key)
    } else {
      val assigned = _size
      put(key, _size)
      assigned
    }
  }

  def writeToMongo(mongoDb: DB) {
    //println("writing to: " + collectionPrefix + "_" + EntityRelationKBMatrix.ROWMAP_COLLECTION)
    writeRowMap(mongoDb.getCollection(collectionPrefix + "_" + EntityPairMemoryMap.ROWMAP_COLLECTION))
    writeEntityMap(mongoDb.getCollection(collectionPrefix + "_" + EntityPairMemoryMap.ENTITY_COLLECTION))
  }

  def populateFromMongo(mongoDb: DB) {
    this._bimap = EntityPairMemoryMap.readRowMap(mongoDb, collectionPrefix + "_" + EntityPairMemoryMap.ROWMAP_COLLECTION)
    this._entityMap = EntityPairMemoryMap.readEntityMap(mongoDb, collectionPrefix + "_" + EntityPairMemoryMap.ENTITY_COLLECTION)
  }

  private def writeEntityMap(entityCollection: DBCollection) {
    entityCollection.drop()

    val builder = entityCollection.initializeUnorderedBulkOperation()


    for((e,id) <- this._entityMap.asScala) {
      //val id = __entityMap.get(e)
      val entityObject = new BasicDBObject
      entityObject.put(EntityPairMemoryMap.ENTITY_ID, id)
      entityObject.put(EntityPairMemoryMap.ENTITY_SURFACE, e)
      //entityCollection.insert(entityObject)
      builder.insert(entityObject)
    }
    builder.execute()
  }

  private def writeRowMap(rowMapCollection: DBCollection) {
    rowMapCollection.drop()
    val builder = rowMapCollection.initializeUnorderedBulkOperation()
    // write row map
    for(ep <- this.keyIterator) {
      val id = this.keyToIndex(ep)
      //val id = __rowMap.get(ep)
      val rowObject = new BasicDBObject
      val eid1 = _entityMap.get(ep.e1)
      val eid2 = _entityMap.get(ep.e2)
      rowObject.put(EntityPairMemoryMap.ENTITY1, eid1)
      rowObject.put(EntityPairMemoryMap.ENTITY2, eid2)
      rowObject.put(EntityPairMemoryMap.ROW_ID, id)

      //println("inserted " + (eid1, eid2) + " -> " + id)

      //rowMapCollection.insert(rowObject)
      builder.insert(rowObject)
    }
    builder.execute()
  }
}

object EntityPairMemoryMap {
  val ENTITY1 = "e1"
  val ENTITY2 = "e2"
  val ROW_ID = "rowid"
  val ROWMAP_COLLECTION = "rows"
  val ENTITY_COLLECTION = "entities"
  val ENTITY_ID = "entid"
  val ENTITY_SURFACE = "entsurface"

  private def readRowMap(mongoDb: DB, collectionName: String): HashBiMap[(Int, Int), Int] = {
    //println("reading from: " + collectionName)
    val collection = mongoDb.getCollection(collectionName)
    val rowMap = HashBiMap.create[(Int, Int), Int]()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val rowObject: DBObject = cursor.next()
        val rowNr = rowObject.get(EntityPairMemoryMap.ROW_ID).asInstanceOf[Int]
        val e1: Int = rowObject.get(EntityPairMemoryMap.ENTITY1).asInstanceOf[Int]
        val e2: Int = rowObject.get(EntityPairMemoryMap.ENTITY2).asInstanceOf[Int]
        rowMap.put((e1, e2), rowNr)
      }
    } finally {
      cursor.close();
    }
    rowMap
  }

  private def readEntityMap(mongoDb: DB, collectionName: String): HashBiMap[String, Int] = {
    val collection = mongoDb.getCollection(collectionName)
    val entityMap = HashBiMap.create[String, Int]()
    val cursor: DBCursor = collection.find();
    try {
      while(cursor.hasNext()) {
        val entityObject: DBObject = cursor.next()
        val id: Int = entityObject.get(EntityPairMemoryMap.ENTITY_ID).asInstanceOf[Int]
        val surface: String = entityObject.get(EntityPairMemoryMap.ENTITY_SURFACE).asInstanceOf[String]
        entityMap.put(surface, id)
      }
    } finally {
      cursor.close();
    }
    entityMap
  }
}
