package cc.factorie.db.mongo

import cc.factorie.Cubbie
import java.util.Map
import org.bson.BSONObject
import collection.JavaConversions._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import org.bson.types.BasicBSONList
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * A MongoCubbieCollection stores cubbies in a Mongo collection.
 *
 * @param coll the mongo collection that will be used to store the cubbies
 * @param constructor the constructor to use when creating cubbies based on mongo objects
 * @param indices A sequence of sequences of slots. Each slot sequence represents one
 * multifield index
 * @author sriedel
 */
class MongoCubbieCollection[C <: Cubbie](val coll: DBCollection,
                                         val constructor: () => C,
                                         val indices: C => Seq[Seq[C#AbstractSlot[Any]]] = (c: C) => Seq.empty[Seq[C#AbstractSlot[Any]]]) extends Iterable[C] {

  import MongoCubbieConverter._

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

  override def size = coll.count.toInt

  /**
   * Drops the underlying database collection.
   */
  def drop() {
    coll.drop()
  }

  //todo: should override other default implementations---how much is this like the casbah MongoCollection?

  /**
   * Inserts a cubbie into the collection.
   * @param c the cubbie to add.
   */
  def +=(c: C) {
    coll.insert(eagerDBO(c))
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
      case (Some(s1: Seq[_]), Some(s2: Seq[_])) if (s2.startsWith(s1)) =>
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
  def updateDelta(oldCubbie: C, newCubbie: C) = {
    require(oldCubbie.id == newCubbie.id)
    val keys = oldCubbie._map.keySet ++ newCubbie._map.keySet
    val insertDBO = new BasicDBObject()
    for (key <- keys; if (key != "_id")) {
      val mod = modification(oldCubbie._map.get(key), newCubbie._map.get(key))
      val bag = insertDBO.getOrElseUpdate(mod.op, new BasicDBObject()).asInstanceOf[DBObject]
      bag.put(key, mod.value)
    }
    val queryDBO = new BasicDBObject("_id", oldCubbie.id)
    coll.update(queryDBO, insertDBO)
  }

  def safeDbo(f: C => C) = if (f == null) null else eagerDBO(f(constructor()))

  /**
   * Finds all cubbies that match the given queries and instantiates the selected slots of these cubbies.
   * @param query a function that maps a cubbie to a cubbie that should be matched.
   * @param select a function that maps a cubbie to a cubbie that shows which slots to instantiate
   * (using the select method). If null all slots are selected
   * @return an iterator over cubbies as defined above.
   */
  def query(query: C => C = null.asInstanceOf[C => C], select: C => C = null.asInstanceOf[C => C]) = {
    val queryDBO = safeDbo(query)
    val selectDBO = safeDbo(select)
    new CursorIterator(coll.find(queryDBO, selectDBO))
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

  class CursorIterator(val underlying: DBCursor) extends Iterator[C] {
    def skip(amount: Int) = new CursorIterator(underlying.skip(amount))

    def limit(amount: Int) = new CursorIterator(underlying.limit(amount))

    def next() = MongoCubbieConverter.eagerCubbie(underlying.next(), constructor)

    def hasNext = underlying.hasNext

    def headOption = if (hasNext) Some(next()) else None

    def setNoTimeOut() {
      underlying.setOptions(16)
    }

    def close() {
      underlying.close()
    }
  }


}

/**
 * Methods to convert cubbies into mongo objects and vice versa.
 */
object MongoCubbieConverter {

  def eagerDBO(cubbie: Cubbie): DBObject = {
    toMongo(cubbie._map).asInstanceOf[DBObject]
  }

  def eagerCubbie[C <: Cubbie](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c._map = toCubbie(dbo).asInstanceOf[HashMap[String, Any]]
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
      case dbo: DBObject =>
        val map = new HashMap[String, Any]
        for (key <- dbo.keySet()) map(key) = toCubbie(dbo.get(key))
        map
      case dbList: BasicBSONList =>
        val list = new ArrayBuffer[Any]
        for (element <- dbList) list += toCubbie(element)
        list
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
 * A slot that can do mongo specific operations
 * @param slot the original slot in the cubbie
 * @tparam C the cubbie type.
 * @tparam V the value type of the slot.
 */
class MongoSlot[C <: Cubbie, V](val slot: C#Slot[V]) {
  def select: C = {
    slot.rawPut(1)
    slot.cubbie
  }

  def update(value: V): C = {
    //todo: need to fix cubbies to avoid try-catch
    val nestedMap = try {
      val oldMap = slot.cubbie._rawGet("$set").asInstanceOf[scala.collection.mutable.Map[String, Any]]
      if (oldMap == null) {
        val map = new HashMap[String, Any];
        slot.cubbie._rawPut("$set", map);
        map
      } else oldMap
    } catch {
      case _ => {
        val map = new HashMap[String, Any];
        slot.cubbie._rawPut("$set", map);
        map
      }
    }
    nestedMap(slot.name) = value
    slot.cubbie
  }

}

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

object MongoCubbieImplicits {

  implicit def toMongoSlot[C <: Cubbie, V](slot: C#Slot[V]) = new MongoSlot(slot)
}

object CubbieMongoTest {

  import MongoCubbieImplicits._
  import MongoCubbieConverter._

  def main(args: Array[String]) {

    class Person extends Cubbie {
      _map = new HashMap[String, Any]
      val name = StringSlot("name")
      val age = IntSlot("age")
      val address = CubbieSlot("address", () => new Address)
      val hobbies = StringListSlot("hobbies")
      val spouse = RefSlot("spouse", () => new Person)
    }
    class Address extends Cubbie {
      _map = new HashMap[String, Any]
      val street = StringSlot("street")
      val zip = StringSlot("zip")
    }
    val address = new Address
    address.street := "Mass Ave."

    val james = new Person
    val laura = new Person
    james.name := "James"
    james.id = 1
    james.age := 50
    james.hobbies := Seq("photography", "soccer")
    james.address := address

    laura.name := "Laura"
    laura.id = 2
    laura.age := 20
    laura.hobbies := Seq("James")
    laura.address := address

    james.spouse ::= laura
    laura.spouse ::= james


    val mongoConn = new Mongo("localhost", 27017)
    val mongoDB = mongoConn.getDB("mongocubbie-test")
    val coll = mongoDB.getCollection("persons")
    coll.drop()
    val persons = new MongoCubbieCollection(coll, () => new Person, (p: Person) => Seq(Seq(p.name)))

    persons += james
    persons += laura

    persons.update(_.name.set("James"), _.name.update("Jamie"))

    for (p <- persons) {
      println(p._map)
    }

    val queryResult = persons.query(_.age.set(50), _.age.select.name.select)
    //    val queryResult = persons.query(_.age.set(50))
    for (p <- queryResult) {
      println(p._map)
    }

    //test a delta update, laura turns 21 and is also interested in Rick!
    val updatedLaura = new Person
    updatedLaura.id = 2
    updatedLaura.age := 21
    updatedLaura.hobbies := Seq("James", "Rick!")
    updatedLaura.address := address
    updatedLaura.spouse ::= james

    persons.updateDelta(laura, updatedLaura)

    println(persons.mkString("\n"))


  }
}

class NestedDiffMap(arg1: collection.Map[String, Any], arg2: collection.Map[String, Any]) extends collection.mutable.Map[String, Any] {

  def toDiff(arg1: Any, arg2: Any): Any = {
    null
    //
    //    arg2 match {
    //      case m:Map[_,_] => new NestedDiffMap(null,m.asInstanceOf[collection.Map[String,Any]])
    //      case s:Seq[_] => s.map(toDiff(_))
    //      case _ => value2
    //    }
  }

  def get(key: String) = {
    val value1 = arg1.get(key)
    val value2 = arg2.get(key)
    if (value1 != value2) {
      value2 match {
        case Some(m: Map[_, _]) => Some(new NestedDiffMap(null, m.asInstanceOf[collection.Map[String, Any]]))
        case _ => value2
      }
    } else None
  }


  def iterator = null

  def +=(kv: (String, Any)) = null

  def -=(key: String) = null
}
