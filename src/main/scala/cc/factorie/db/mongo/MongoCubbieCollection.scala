package cc.factorie.db.mongo

import cc.factorie.Cubbie
import java.util.Map
import org.bson.BSONObject
import collection.JavaConversions._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import org.bson.types.BasicBSONList
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * @author sriedel
 */
class MongoCubbieCollection[C <: Cubbie](val coll: DBCollection, val constructor: () => C) extends Iterable[C] {

  import MongoCubbieConverter._

  def iterator: CursorIterator = {
    new CursorIterator(coll.find())
  }

  override def size = coll.count.toInt

  def drop() {
    coll.drop()
  }

  //todo: should override other default implementations---how much is this like the casbah MongoCollection?

  def +=(c: C) {
    coll.insert(eagerDBO(c))
  }

  def safeDbo(f: C => C) = if (f == null) null else eagerDBO(f(constructor()))

  def query(query: C => C = null.asInstanceOf[C => C], select: C => C = null.asInstanceOf[C => C]) = {
    val queryDBO = safeDbo(query)
    val selectDBO = safeDbo(select)
    new CursorIterator(coll.find(queryDBO, selectDBO))
  }

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


class MongoMap(val dbo: DBObject) extends HashMap[String, Any]

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

class MongoSlot[C <: Cubbie, V](val slot: C#Slot[V]) {
  def select: C = {
    slot.rawPut(1)
    slot.cubbie
  }

  def update(value: V): C = {
    //todo: need to fix cubbies to avoid try-catch
    val nestedMap = try {
      val oldMap = slot.cubbie._rawGet("$set").asInstanceOf[scala.collection.mutable.Map[String,Any]]
      if (oldMap == null) {
        val map = new HashMap[String, Any];
        slot.cubbie._rawPut("$set",map);
        map
      } else oldMap
    } catch {
      case _ => {
        val map = new HashMap[String, Any];
        slot.cubbie._rawPut("$set",map);
        map
      }
    }
    nestedMap(slot.name) = value
    slot.cubbie
  }

}

class BasicBSONBSeq(val bson:BasicBSONList) extends Seq[Any] {
  import MongoCubbieConverter._

  def length = bson.size()

  def apply(idx: Int) = toLazyCubbie(bson.get(idx))

  def iterator = bson.iterator().map(toLazyCubbie(_))
}

class BSONMap(val bson:BSONObject) extends collection.mutable.Map[String,Any] {
  import MongoCubbieConverter._
  
  def get(key: String) = if (bson.containsField(key)) Some(toLazyCubbie(bson.get(key))) else None

  def iterator = bson.keySet().iterator().map(key => key -> toLazyCubbie(bson.get(key)))

  def +=(kv: (String, Any)):this.type = {
    bson.put(kv._1,kv._2)
    this
  }

  def -=(key: String):this.type = {
    bson.removeField(key)
    this
  }
}

object MongoCubbieImplicits {

  implicit def toMongoSlot[C <: Cubbie, V](slot: C#Slot[V]) = new MongoSlot(slot)
}

object CubbieMongoTest {

  import MongoCubbieImplicits._

  def main(args: Array[String]) {

    class Person extends Cubbie {
      val name = StringSlot("name")
      val age = IntSlot("age")
      val address = CubbieSlot("address", () => new Address)
      val hobbies = StringListSlot("hobbies")
      val spouse = RefSlot("spouse", () => new Person)
    }
    class Address extends Cubbie {
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
    val persons = new MongoCubbieCollection(coll, () => new Person)

    persons += james
    persons += laura

    persons.update(_.name.set("James"), _.name.update("Jamie"))

    for (p <- persons) {
      println(p._map)
    }

    val queryResult = persons.query(_.age.set(50), _.age.select.name.select)

    for (p <- queryResult) {
      println(p._map)
    }


  }
}
