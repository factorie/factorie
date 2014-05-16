/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.db.mongo

import org.bson.BSONObject
import scala.collection.JavaConversions._
import com.mongodb._
import org.bson.types.BasicBSONList
import cc.factorie.util.Cubbie
import collection.{Map => GenericMap, mutable, JavaConversions}
import scala.collection.mutable.ArrayBuffer
import scala._
import scala.Predef._
import scala.Some
import scala.language.implicitConversions


/**
 * Covariant interface to a cubbie collection.
 *
 * @tparam C Cubbie type of collection.
 */
trait AbstractCubbieCollection[+C <: Cubbie] extends Iterable[C] {

  /**
   * Find all cubbies for which the id is within the given set of ids.
   * @param ids the ids to check against.
   * @return all cubbies in this collection which have one of the provided ids.
   */
  def findByIds(ids: Seq[Any]): Iterator[C]

  /**
   * Find all cubbies for which the id is within the given set of ids.
   * @param id the id of the object to find.
   * @return all cubbies in this collection which have one of the provided ids.
   */
  def findById(id:Any):Iterator[C]

  /**
   * Find all cubbies for which the attribute/slot with the given name has a value
   * within the provided sequence of values.
   * @param name the name of the attribute.
   * @param values the sequence of values the attribute can be in.
   * @tparam T the type of the values the attribute can have.
   * @return all cubbies that have a slot with the given name for which the value is in the
   *         specified sequence of values.
   */
  def findByAttribute[T](name: String, values: Seq[T]): Iterator[C]

  //  def findByAttribute[T](slot:C=>C#Slot, values:Seq[T]):Iterator[C]

  /**
   * Find all cubbies for which the provided field/slot has one of the provided values.
   * @param field a function from cubbie to the slot to test.
   * @param values the values the slot can have to be included in the result.
   * @tparam T the type of the slot values.
   * @return all cubbies in this collection for which the given slot has one of the given
   *         values.
   */
  def findBySlot[T](field: C => Cubbie#Slot[T], values: Seq[T]): Iterator[C]

  /**
   * A prototype cubbie of the type this collection contains.
   * @return a prototype cubbie of the same type this collection holds.
   */
  def prototype: C
}

/**
 * A cubbie collection that can be modified (hence not covariant anymore).
 * @tparam C Cubbie type of collection.
 */
trait MutableCubbieCollection[C <: Cubbie] extends AbstractCubbieCollection[C] {
  /**
   * Assuming that the collection contains the old-cubbie, this operation changes
   * the old-cubbie to be in the state of the new cubbie. This assumes that
   * both cubbies have the same id.
   * @param oldCubbie the old state of the cubbie.
   * @param newCubbie the new state of the cubbie.
   */
  def updateDelta(oldCubbie: C, newCubbie: C): WriteResult

  /**
   * Removes all cubbies that match the given query.
   * @param query a function that takes a cubbie and returns a query cubbie.
   */
  def remove(query: C => C)

  /**
   * Inserts the given cubbie into the collection.
   * @param c the cubbie to add.
   */
  def +=(c: C)

  /**
   * Batch insert of several cubbies. Can often be more efficient because underlying
   * persistence layers can avoid network overhead.
   * @param c the cubbies to add.
   */
  def ++=(c: TraversableOnce[C])

  /**
   * Deletes the complete collection.
   */
  def drop()
}

/**
 * A MongoCubbieCollection stores cubbies in a Mongo collection.
 *
 * @param coll the mongo collection that will be used to store the cubbies.
 * @param constructor the constructor to use when creating cubbies based on mongo objects.
 * @param indices A sequence of sequences of slots. Each slot sequence represents one
 *                multi-field index.
 * @tparam C the type of the cubbies this collection stores.
 * @author sriedel
 */
class MongoCubbieCollection[C <: Cubbie](val coll: DBCollection,
                                         val constructor: () => C,
                                         val indices: C => Seq[Seq[C#AbstractSlot[Any]]] = (c: C) => Seq.empty[Seq[C#AbstractSlot[Any]]])
  extends MutableCubbieCollection[C] with MongoCubbieConverter[C] {

  import MongoCubbieConverter._

  /**
   * Makes sure that the underlying collection has the specified indices.
   */
  private def ensureIndices() {
    val c = constructor()
    val indices = this.indices(c)
    for (index <- indices) {
      val dbo = new BasicDBObject()
      for (key <- index) dbo.put(key.name, 1)
      coll.ensureIndex(dbo)
    }
  }

  ensureIndices()

  /**
   * Returns a iterator over the cubbies stored in this collection.
   * @return cubbie iterator.
   */
  def iterator: CursorIterator = {
    new CursorIterator(coll.find())
  }

  /**
   * The size of the collection.
   * @return number of cubbies in the collection.
   */
  override def size = coll.count.toInt

  /**
   * Drops the underlying database collection.
   */
  def drop() {
    coll.drop()
  }

  lazy val prototype = constructor()

  def findByAttribute[T](name: String, values: Seq[T]): CursorIterator = {
    new CursorIterator(coll.find(new BasicDBObject(name, new BasicDBObject("$in", toMongo(values)))))
  }

  //todo: should override other default implementations---how much is this like the casbah MongoCollection?

  /**
   * Inserts a cubbie into the collection.
   * @param c the cubbie to add.
   */
  def +=(c: C) {
    coll.insert(eagerDBO(c))
  }

  /**
   * Batch insert of a collection of cubbies.
   * @param c collection to insert.
   */
  def ++=(c: TraversableOnce[C]) {
    coll.insert(JavaConversions.seqAsJavaList(c.map(eagerDBO(_)).toSeq))
  }
  /**
   * Batch insert of a collection of cubbies.
   * @param c collection to insert.
   * @param w writeConcern.
   */
  def ++=(c: TraversableOnce[C], w: WriteConcern) {
    coll.insert(JavaConversions.seqAsJavaList(c.map(eagerDBO(_)).toSeq), w)
  }
  case class Modification(op: String, value: Any)

  /**
   * Returns the modification operation and value needed to store the change represented by the old and new value.
   * @param oldOpt either None (if no value existed) or Some(x) where x is the old value
   * @param newOpt either None (if no new value exists) or Some(x) where x is the new value
   * @return A modification object that shows which mongo modifier is required, and what its value should be.
   */
  private def modification(oldOpt: Option[Any], newOpt: Option[Any]): Modification = {
    oldOpt -> newOpt match {
      case (Some(s1: Seq[_]), Some(s2: Seq[_])) if s2.startsWith(s1) =>
        Modification("$pushAll", toMongo(s2.drop(s1.size)))
      case (Some(oldValue), Some(newValue)) =>
        Modification("$set", toMongo(newValue))
      case (Some(oldValue), None) =>
        Modification("$unset", 1)
      case (None, Some(newValue)) =>
        Modification("$set", toMongo(newValue))
      case _ => sys.error("One argument should be Some(x)")
    }
  }

  /**
   * Efficiently stores the changes made to a given cubbie.
   * @param oldCubbie an old version of the cubbie to store
   * @param newCubbie a new version of the cubbie to store
   * @return a mongodb WriteResult.
   */
  def updateDelta(oldCubbie: C, newCubbie: C): WriteResult = {
    require(oldCubbie.id == newCubbie.id)
    val keys = oldCubbie._map.keySet ++ newCubbie._map.keySet
    val insertDBO = new BasicDBObject()
    var foundDiff = false
    for (key <- keys; if key != "_id") {
      if(oldCubbie._map.get(key) != newCubbie._map.get(key)){
        foundDiff=true
        val mod = modification(oldCubbie._map.get(key), newCubbie._map.get(key))
        val bag = insertDBO.getOrElseUpdate(mod.op, new BasicDBObject()).asInstanceOf[DBObject]
        bag.put(key, mod.value)
      }
    }
    if(foundDiff){
      val queryDBO = new BasicDBObject("_id", oldCubbie.id)
      coll.update(queryDBO, insertDBO)
    } else {
      null
    }
  }

  private def safeDbo(f: C => C) = if (f == null) null else eagerDBO(f(constructor()))

  /**
   * Finds all cubbies that match the given queries and instantiates the selected slots of these cubbies.
   * @param query a function that maps a cubbie to a cubbie that should be matched.
   * @param select a function that maps a cubbie to a cubbie that shows which slots to instantiate
   *               (using the select method). If null all slots are selected
   * @return an iterator over cubbies as defined above.
   */
  def query(query: C => C = null.asInstanceOf[C => C], select: C => C = null.asInstanceOf[C => C]) = {
    val queryDBO = safeDbo(query)
    val selectDBO = safeDbo(select)
    new CursorIterator(coll.find(queryDBO, selectDBO))
  }

  def findByIds(ids: Seq[Any]) = {
    query(new MongoCubbie(_).idsIn(ids))
  }

  def findById(id: Any) = {
    query(new MongoCubbie(_).idIs(id))
  }

  def findBySlot[T](field: (C) => Cubbie#Slot[T], values: Seq[T]) = {
    query(c => new MongoSlot[Cubbie, T](field(c)).valuesIn(values).asInstanceOf[C])
  }


  /**
   * Removes all items in the collection that match the given query.
   * @param query a function that maps a cubbie to a cubbie that should be matched.
   * @return unit
   */
  def remove(query: C => C) {
    val queryDBO = safeDbo(query)
    coll.remove(queryDBO)
  }

  /**
   * Updates the collection as specified.
   * @param query a function that returns a cubbie to match
   * @param modification a function that returns the modification cubbie.
   * @param upsert should an object be created if no match can be found.
   * @param multi should we do updates to all matching cubbies.
   * @return a mongo write result.
   */
  def update(query: C => C = null.asInstanceOf[C => C],
             modification: C => C = null.asInstanceOf[C => C],
             upsert: Boolean = false,
             multi: Boolean = false) = {
    val queryDBO = safeDbo(query)
    val modDBO = safeDbo(modification)
    coll.update(queryDBO, modDBO, upsert, multi)
  }

  /**
   * By default the collection converts mongo documents eagerly to scala map
   * objects which then become the maps underlying the cubbies.
   * @param dbo a mongo document object.
   * @param constructor the constructor to use to create the cubbies.
   * @return a cubbie based on the provided dbo.
   */
  def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.eagerCubbie(dbo, constructor)
  }

  /**
   * A wrapper around a mongodb database iterator. On each
   * call of next it converts the given document to a cubbie.
   * @param underlying the underyling raw database iterator.
   */
  class CursorIterator(val underlying: DBCursor) extends Iterator[C] {
    def skip(amount: Int) = new CursorIterator(underlying.skip(amount))

    def limit(amount: Int) = new CursorIterator(underlying.limit(amount))

    def next() = mongo2Cubbie(underlying.next(), constructor)

    def hasNext = underlying.hasNext

    def headOption = if (hasNext) Some(next()) else None

    def setNoTimeOut() {
      underlying.setOptions(16)
    }

    def sort(fields: C => C) = {
      val fieldsDBo = safeDbo(fields)
      new CursorIterator(underlying.sort(fieldsDBo))
    }

    def close() {
      underlying.close()
    }
  }

}

/**
 * A trait to mix-in into mongo cubbie collections that controls how the raw
 * mongo documents are converted to cubbies.
 * @tparam C the type of cubbies to convert to.
 */
trait MongoCubbieConverter[C <: Cubbie] {
  /**
   * Converts a mongodb document to a cubbie
   * @param dbo the mongodb doc.
   * @param constructor the constructor to use for creating the cubbie.
   * @return a cubbie based on the content of the given mongo document.
   */
  def mongo2Cubbie(dbo: DBObject, constructor: () => C): C
}

/**
 * A converter that eagerly, and recursively, creates a mutable.Map based on the
 * given mongodb document. This eager conversion translates DBObjects to mutable.Maps,
 * and lists to scala Seq objects. All other types are kept as is.
 * @tparam C the type of cubbies to convert to.
 */
trait EagerCubbieConverter[C <: Cubbie] extends MongoCubbieConverter[C] {
  abstract override def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.eagerCubbie[C](dbo, constructor)
  }
}

/**
 * This converter creates cubbies which do conversion from mongodb objects to
 * scala maps and seqs on-the-fly. That is, no conversion is performed at creation
 * time (or call time of mongo2Cubbie). Instead, the underyling map of the cubbie
 * keeps the the original mongodb object around, and when its get/set methods are called
 * the map converts the mongodb fields into corresponding scala values.
 *
 * @tparam C the type of cubbie the converter creates.
 */
trait LazyCubbieConverter[C <: Cubbie] extends MongoCubbieConverter[C] {
  abstract override def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.lazyCubbie(dbo, constructor)
  }
}


/**
 * Helper methods to convert cubbies into mongo objects and vice versa.
 */
object MongoCubbieConverter {

  def eagerDBO(cubbie: Cubbie): DBObject = {
    toMongo(cubbie._map).asInstanceOf[DBObject]
  }

  def eagerCubbie[C <: Cubbie](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c._map = toCubbie(dbo).asInstanceOf[mutable.Map[String, Any]]
    c
  }

  def lazyCubbie[C <: Cubbie](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c._map = toLazyCubbie(dbo).asInstanceOf[mutable.Map[String, Any]]
    c
  }


  def toMongo(any: Any): Any = {
    any match {
      case smap: scala.collection.Map[_, _] =>
        val dbMap = new BasicDBObject(smap.size)
        for ((key, value) <- smap) {
          val mongoValue = toMongo(value)
          dbMap.put(key.asInstanceOf[String], mongoValue)
        }
        dbMap
      case l: Seq[_] =>
        val dbList = new BasicDBList
        for (element <- l) dbList.add(toMongo(element).asInstanceOf[AnyRef])
        dbList
      case _ => any
    }
  }

  def toCubbie(any: Any): Any = {
    any match {
      case dbList: BasicBSONList =>
        val list = new ArrayBuffer[Any]
        for (element <- dbList) list += toCubbie(element)
        list
      case dbo: DBObject =>
        val map = new mutable.HashMap[String, Any]
        for (key <- dbo.keySet()) map(key) = toCubbie(dbo.get(key))
        map
      case _ => any
    }
  }

  def toLazyCubbie(any: Any): Any = {
    any match {
      case dbList: BasicBSONList => new BasicBSONBSeq(dbList)
      case dbo: BSONObject => new BSONMap(dbo)
      case _ => any
    }
  }


}

/**
 * A wrapper around a cubbie that can be used to specify mongo queries using the cubbie map.
 * @param cubbie the cubbie to wrap around
 * @tparam C the type of cubbie.
 */
class MongoCubbie[C <: Cubbie](val cubbie: C) {
  /**
   * Create a query that matches cubbies with the given id.
   * @param id the id for matching cubbies to have.
   * @return the encompassing query cubbie itself.
   */
  def idIs(id: Any): C = {
    cubbie._map("_id") = id
    cubbie
  }

  /**
   * Create a query that matches cubbies that have one of the provided ids.
   * @param ids the ids for matching cubbies to have.
   * @return the encompassing query cubbie itself.
   */
  def idsIn(ids: Seq[Any]): C = {
    cubbie._map("_id") = Map("$in" -> ids)
    cubbie
  }
}

/**
 * A slot that can do mongo specific operations
 * @param slot the original slot in the cubbie
 * @tparam C the cubbie type.
 * @tparam V the value type of the slot.
 */
class MongoSlot[C <: Cubbie, V](val slot: C#Slot[V]) {

  /**
   * Changes the query cubbie to select/project the given slot.
   * @return the cubbie container of this slot.
   */
  def select: C = {
    slot.rawPut(1)
    slot.cubbie
  }


  /**
   * Creates a query cubbie that will update the given slot for matched cubbies
   * to have the given values.
   * @param value the new slot value for matching cubbies to be set to.
   * @return the encompassing query cubbie.
   */
  def update(value: V): C = {
    //todo: need to fix cubbies to avoid try-catch
    val nestedMap = try {
      val oldMap = slot.cubbie._map("$set").asInstanceOf[scala.collection.mutable.Map[String, Any]]
      if (oldMap == null) {
        val map = new mutable.HashMap[String, Any]
        slot.cubbie._map.update("$set", map)
        map
      } else oldMap
    } catch {
      case _: Throwable => {
        val map = new mutable.HashMap[String, Any]
        slot.cubbie._map.update("$set", map)
        map
      }
    }
    nestedMap(slot.name) = value
    slot.cubbie
  }

  /**
   * Modifies the query to match if the given slot has values in the given
   * set of values.
   * @param values the values to match against.
   * @return the encompassing query cubbie.
   */
  def valuesIn(values: Seq[V]): C = {
    slot.cubbie._map(slot.name) = Map("$in" -> values)
    slot.cubbie
  }

  /**
   * Modifies the query to test whether the cubbie has the given field or not.
   * @param yes if true the query requires the cubbie to have the given slot, else it
   *            requires the cubbie to not have the given slot.
   * @return the encompassing query cubbie.
   */
  def exists(yes: Boolean): C = {
    slot.cubbie._map(slot.name) = Map("$exists" -> yes)
    slot.cubbie
  }

}


class MongoRefSlot[C <: Cubbie, A <: Cubbie](val slot: C#AbstractRefSlot[A]) {
  def in(coll: MongoCubbieCollection[A]): GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(slot, coll)

}

/**
 * Support for mongo queries specific to list attributes
 * @param slot a list slot
 * @tparam C the type of cubbie the attribute is part of
 * @tparam A the type objects in the list.
 */
class MongoPrimitiveListSlot[C <: Cubbie, A](val slot: C#PrimitiveListSlot[A]) {
  /**
   * Returns a cubbie that mongo can use to match documents that have the given value as member in the list.
   * @param a the element that needs to be in the list.
   * @return the cubbie itself.
   */
  def contains(a:A): C = {
    slot.rawPut(a)
    slot.cubbie
  }

}


class MongoInvSlot[C <: Cubbie, A <: Cubbie](val slot: C#AbstractInverseSlot[A]) {
  def of(coll: MongoCubbieCollection[A]): GraphLoader.InvSlotInCollection[A] = GraphLoader.InvSlotInCollection(slot, coll)
}


/**
 * Lazy scala seq wrapper around bson sequence.
 * @param bson the bson list to wrap around.
 */
class BasicBSONBSeq(val bson: BasicBSONList) extends Seq[Any] {

  import MongoCubbieConverter._

  def length = bson.size()

  def apply(idx: Int) = toLazyCubbie(bson.get(idx))

  def iterator = bson.iterator().map(toLazyCubbie(_))
}

/**
 * Wrapper around a bson map to provide scala-access to the map and its content. It recursively
 * converts values in the map into their scala equivalent (in case they are bson-based).
 * @param bson the underlying BSON map.
 */
class BSONMap(val bson: BSONObject) extends collection.mutable.Map[String, Any] {

  import MongoCubbieConverter._

  def get(key: String) = if (bson.containsField(key)) Some(toLazyCubbie(bson.get(key))) else None

  def iterator = bson.keySet().iterator().map(key => key -> toLazyCubbie(bson.get(key)))

  def +=(kv: (String, Any)): this.type = {
    bson.put(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = {
    bson.removeField(key)
    this
  }
}

/**
 * Implicits to import when you want to create cubbie mongo queries.
 */
object MongoCubbieImplicits {

  implicit def toMongoSlot[C <: Cubbie, V](slot: C#Slot[V]) = new MongoSlot(slot)

  implicit def toMongoRefSlot[C <: Cubbie, A <: Cubbie](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot)

  implicit def toMongoInvSlot[C <: Cubbie, A <: Cubbie](slot: C#AbstractInverseSlot[A]) = new MongoInvSlot(slot)

  implicit def toMongoPrimitiveListSlot[C <: Cubbie, A](slot: C#PrimitiveListSlot[A]) = new MongoPrimitiveListSlot(slot)

  implicit def toMongoCubbie[C <: Cubbie](cubbie: C) = new MongoCubbie(cubbie)

}

object DerefImplicits {
  implicit def toMongoRefSlot[C <: Cubbie, A <: Cubbie](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot) {
    def -->(implicit cache: GenericMap[Any, Cubbie]): A = slot.deref(cache)
  }

  implicit def toMongoInvSlot[C <: Cubbie, A <: Cubbie](slot: C#InverseSlot[A]) = new MongoInvSlot(slot) {
    //    def -->(implicit cache: GenericMap[Any, Cubbie]): A = slot.deref(cache)
  }

}



class CachedFunction[F, T](val delegate: F => T) extends Map[F, T] {
  val cache = new collection.mutable.HashMap[F, T]

  def get(key: F) = Some(cache.getOrElseUpdate(key, delegate(key)))

  def iterator = cache.iterator

  def -(key: F) = {
    val result = new CachedFunction(delegate)
    result.cache ++= cache
    result.cache -= key
    result
  }

  def +[B1 >: T](kv: (F, B1)) = {
    val result = new CachedFunction(delegate)
    result.cache ++= cache
    result.cache += kv.asInstanceOf[(F, T)]
    result
  }
}

class LazyInverter(val cubbies: PartialFunction[Manifest[Cubbie], Iterable[Cubbie]])
  extends (Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]) {
  def apply(slot: Cubbie#InverseSlot[Cubbie]) = {
    val typed = slot.asInstanceOf[Cubbie#InverseSlot[Cubbie]]
    val result = cubbies.lift(slot.manifest).getOrElse(Nil).filter(c => typed.slot(c).opt == Some(typed.cubbie.id))
    result
  }
}

class IndexedLazyInverter(val cubbies: PartialFunction[Manifest[Cubbie], Iterable[Cubbie]])
  extends (Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]) {


  val index = new mutable.HashMap[(Cubbie#AbstractRefSlot[Cubbie], Any), Seq[Cubbie]]
  val indexed = new mutable.HashSet[Cubbie#AbstractRefSlot[Cubbie]]
  val prototypes = new mutable.HashMap[Manifest[Cubbie], Option[Cubbie]] //cubbies.map(p => p._1 -> p._2.headOption)

  def findCubbiesWhereRefSlotIs(refSlotFunction: Cubbie => Cubbie#AbstractRefSlot[Cubbie],
                                id: Any,
                                inWhere: Iterable[Cubbie],
                                ofType: Manifest[Cubbie]) = {
    {
      for (prototype <- prototypes.getOrElseUpdate(ofType, cubbies(ofType).headOption);
           refSlot = refSlotFunction(prototype)) yield {
        if (!indexed(refSlot)) {
          for (c <- cubbies.lift(ofType).getOrElse(Nil); if refSlotFunction(c).opt == Some(id)) {
            index(refSlot -> id) = index.getOrElse(refSlot -> id, Nil) :+ c
          }
          indexed += refSlot
        }
        index.getOrElse(refSlot -> id, Nil)
      }
    }.getOrElse(Nil)
  }

  def apply(slot: Cubbie#InverseSlot[Cubbie]) = {
    val typed = slot.asInstanceOf[Cubbie#InverseSlot[Cubbie]]
    findCubbiesWhereRefSlotIs(typed.slot, typed.cubbie.id, cubbies.lift(typed.manifest).getOrElse(Nil), typed.manifest)
  }
}


class LazyMongoInverter(val cubbies: PartialFunction[Manifest[Cubbie], AbstractCubbieCollection[Cubbie]],
                        val cache: GenericMap[Any, Cubbie] = Map.empty)
  extends (Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]) {
  def apply(slot: Cubbie#InverseSlot[Cubbie]) = {
    val typed = slot.asInstanceOf[Cubbie#InverseSlot[Cubbie]]
    val found = for (coll <- cubbies.lift(slot.manifest)) yield {
      val raw = coll.findBySlot(c => slot.slot(c).asInstanceOf[Cubbie#RefSlot[Cubbie]], Seq(typed.cubbie.id))
      raw.map(c => cache.getOrElse(c.id, c)).toSeq
    }
    found.getOrElse(Nil)
  }
}

class Indexer(val indices: Cubbie => Seq[Cubbie#AbstractSlot[Any]]) extends ((Cubbie#AbstractSlot[Any], Any) => Unit) {

  case class SlotKey(cubbieClass: Class[Cubbie], name: String, value: Any) {
  }

  def slotKey(slot: Cubbie#AbstractSlot[Any], value: Any) = {
    SlotKey(slot.cubbie.getClass.asInstanceOf[Class[Cubbie]], slot.name, value)
  }

  val index = new mutable.HashMap[SlotKey, List[Cubbie]]

  def apply(slot: Cubbie#AbstractSlot[Any], value: Any) {
    val typed = slot.cubbie
    if (indices(typed).contains(slot)) {
      val key = slotKey(slot, value)
      index(key) = index.getOrElse(key, Nil) :+ typed
    }
  }
}


