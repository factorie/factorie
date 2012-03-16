package cc.factorie.db.mongo

import cc.factorie.Cubbie
import org.bson.BSONObject
import collection.JavaConversions._
import com.mongodb.{BasicDBList, BasicDBObject, DBCursor, DBObject, DBCollection, Mongo}
import org.bson.types.BasicBSONList
import collection.mutable.{ArrayBuffer, HashMap}
import collection.mutable.{Map => MutableMap}
import cc.factorie.util.CubbieRefs
import annotation.tailrec
import collection.{MapProxy, Map => GenericMap, JavaConversions}


/**
 * Covariant interface to cubbie collection
 * @tparam C Cubbie type of collection.
 */
trait AbstractMongoCubbieCollection[+C <: Cubbie] extends Iterable[C] {

  def findByIds(ids: Seq[Any]): Iterator[C]
  
  def findBySlot[T](field:C => Cubbie#Slot[T], values:Seq[T]): Iterator[C]
}

/**
 * A MongoCubbieCollection stores cubbies in a Mongo collection.
 *
 * @param coll the mongo collection that will be used to store the cubbies.
 * @param constructor the constructor to use when creating cubbies based on mongo objects.
 * @param indices A sequence of sequences of slots. Each slot sequence represents one
 * multi-field index.
 * @tparam C the type of the cubbies this collection stores.
 * @author sriedel
 */
class MongoCubbieCollection[C <: Cubbie](val coll: DBCollection,
                                         val constructor: () => C,
                                         val indices: C => Seq[Seq[C#AbstractSlot[Any]]] = (c: C) => Seq.empty[Seq[C#AbstractSlot[Any]]])
  extends AbstractMongoCubbieCollection[C] with MongoCubbieConverter[C] {

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
  def +=(c: C) = {
    coll.insert(eagerDBO(c))
  }

  /**
   * Batch insert of a collection of cubbies.
   * @param c collection to insert.
   */
  def ++=(c: TraversableOnce[C]) = {
    coll.insert(JavaConversions.seqAsJavaList(c.map(eagerDBO(_)).toSeq))
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

  private def safeDbo(f: C => C) = if (f == null) null else eagerDBO(f(constructor()))

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

  def findByIds(ids: Seq[Any]) = {
    query(new MongoCubbie(_).idsIn(ids))
  }

  def findBySlot[T](field: (C) => Cubbie#Slot[T], values:Seq[T]) = {
    query(c => new MongoSlot[Cubbie,T](field(c)).valuesIn(values).asInstanceOf[C])
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

  def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.eagerCubbie(dbo, constructor)
  }

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

    //    def sort() {
    //      underlying.sort()
    //    }

    def close() {
      underlying.close()
    }
  }

}

trait MongoCubbieConverter[C <: Cubbie] {
  def mongo2Cubbie(dbo: DBObject, constructor: () => C): C
}

trait EagerCubbieConverter[C <: Cubbie] extends MongoCubbieConverter[C] {
  abstract override def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.eagerCubbie(dbo, constructor)
  }
}

/**
 * Mix in this trait to a mongo cubbie collection and you get lazy loading of cubbie objects.
 * @tparam C the type of cubbie the converter creates.
 */
trait LazyCubbieConverter[C <: Cubbie] extends MongoCubbieConverter[C] {
  abstract override def mongo2Cubbie(dbo: DBObject, constructor: () => C) = {
    MongoCubbieConverter.lazyCubbie(dbo, constructor)
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
    c._map = toCubbie(dbo).asInstanceOf[MutableMap[String, Any]]
    c
  }

  def lazyCubbie[C <: Cubbie](dbo: DBObject, constructor: () => C): C = {
    val c = constructor()
    c._map = toLazyCubbie(dbo).asInstanceOf[MutableMap[String, Any]]
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
 * A wrapper around a cubbie that can be used to specify mongo queries using the cubbie map.
 * @param cubbie the cubbie to wrap around
 * @tparam C the type of cubbie.
 */
class MongoCubbie[C <: Cubbie](val cubbie: C) {
  def idIs(id: Any): cubbie.type = {
    cubbie._map("_id") = id
    cubbie
  }

  def idsIn(ids: Seq[Any]): cubbie.type = {
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

  def valuesIn(values: Seq[V]): C = {
    slot.cubbie._map(slot.name) = Map("$in" -> values)
    slot.cubbie
  }


}

class MongoRefSlot[C <: Cubbie, A <: Cubbie](val slot: C#AbstractRefSlot[A]) {
  def in(coll: MongoCubbieCollection[A]): GraphLoader.SlotInCollection[A] = GraphLoader.SlotInCollection(slot, coll)

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

object MongoCubbieImplicits {

  implicit def toMongoSlot[C <: Cubbie, V](slot: C#Slot[V]) = new MongoSlot(slot)

  implicit def toMongoRefSlot[C <: Cubbie, A <: Cubbie](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot)

  implicit def toMongoCubbie[C <: Cubbie](cubbie: C) = new MongoCubbie(cubbie)

}

object DerefImplicits {
  implicit def toMongoRefSlot[C <: Cubbie, A <: Cubbie](slot: C#AbstractRefSlot[A]) = new MongoRefSlot(slot) {
    def -->(implicit cache: GenericMap[Any, Cubbie]): A = slot.deref(cache)
  }
}

object CubbieMongoTest {

  import MongoCubbieImplicits._
  import MongoCubbieConverter._

  def main(args: Array[String]) {

    class Person extends Cubbie {
      val name = StringSlot("name")
      val age = IntSlot("age")
      val address = CubbieSlot("address", () => new Address)
      val hobbies = StringListSlot("hobbies")
      val spouse = RefSlot("spouse", () => new Person)
      val children = InverseSlot("children", (p:Person) => p.father)
      val father = RefSlot("father", () => new Person)
    }
    class Address extends Cubbie {
      val street = StringSlot("street")
      val zip = StringSlot("zip")
    }

    val address = new Address
    address.street := "Mass Ave."

    val james = new Person
    val laura = new Person
    val kid = new Person

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

    kid.name := "Kid"
    kid.id = 3

    james.spouse ::= laura
    laura.spouse ::= james
    kid.father ::= james


    val mongoConn = new Mongo("localhost", 27017)
    val mongoDB = mongoConn.getDB("mongocubbie-test")
    val coll = mongoDB.getCollection("persons")
    coll.drop()
    val persons = new MongoCubbieCollection(coll, () => new Person, (p: Person) => Seq(Seq(p.name))) with LazyCubbieConverter[Person]

    persons += james
    persons += laura
    persons += kid

    persons.update(_.name.set("James"), _.name.update("Jamie"))

    for (p <- persons) {
      println(p._map)
    }

    val queryResult = persons.query(_.age(50), _.age.select.name.select)
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

    //test batch id query
    println("****")
    println(persons.query(_.idsIn(Seq(1, 2))).mkString("\n"))
    println(persons.query(_.idIs(1)).mkString("\n"))


    implicit val refs = GraphLoader.load(Seq(james), {
      case p: Person => Seq(p.spouse in persons)
    })

    println("James' spouse")
    println(james.spouse.deref)
    println("James' spouse's spouse")
    println(james.spouse.deref.spouse.deref)

    //or with fancy deref implicits
    //    import DerefImplicits._
    //    println(james.spouse-->spouse-->name.value)

    kid.name := "Kid 2"

    implicit val inverter = new CachedFunction(new LazyInverter(Map(manifest[Person]-> Seq(james,laura,kid))))

    println(james.children.value)
    
    val mongoInverter = new CachedFunction(new LazyMongoInverter(Map(manifest[Person] -> persons)))

    println(james.children.value(mongoInverter))
    

  }
}

class CachedFunction[F,T](val delegate:F=>T) extends Map[F, T] {
  val cache = new HashMap[F,T]

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
    result.cache += kv.asInstanceOf[(F,T)]
    result
  }
}

class LazyInverter(val cubbies:PartialFunction[Manifest[Cubbie],Iterable[Cubbie]]) 
  extends (Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]) {
  def apply(slot: Cubbie#InverseSlot[Cubbie]) = {
    val typed = slot.asInstanceOf[Cubbie#InverseSlot[Cubbie]]
    val result = cubbies.lift(slot.manifest).getOrElse(Nil).filter(c => typed.slot(c).opt == Some(typed.cubbie.id))
    result
  }
}

class LazyMongoInverter(val cubbies:PartialFunction[Manifest[Cubbie],AbstractMongoCubbieCollection[Cubbie]],
                        val cache:GenericMap[Any,Cubbie] = Map.empty)
  extends (Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]) {
  def apply(slot: Cubbie#InverseSlot[Cubbie]) = {
    val typed = slot.asInstanceOf[Cubbie#InverseSlot[Cubbie]]
    val found = for (coll <- cubbies.lift(slot.manifest)) yield {
      val raw = coll.findBySlot(c => slot.slot(c).asInstanceOf[Cubbie#RefSlot[Cubbie]],Seq(typed.cubbie.id))
      raw.map(c => cache.getOrElse(c.id,c)).toSeq
    }
    found.getOrElse(Nil)
  }
}



object GraphLoader {

  case class SlotInCollection[+R <: Cubbie](slot: Cubbie#AbstractRefSlot[R], coll: AbstractMongoCubbieCollection[R])

  type Refs = GenericMap[Any, Cubbie]


  /**
   * Loads a cache from ids to cubbies based on the root objects and a neighborhood function.
   * @param roots the cubbies to start with.
   * @param neighbors the neigborhood function from cubbies to (refslot,collection) pairs.
   * @param maxDepth How far from the root are we allowed to travel. If maxDepth == 0 no ids are added to the cache, for
   * maxDepth == 1 only the roots are added to the cache, for maxDeptn == 2 the roots immediate children etc.
   * @param oldRefs an existing cache. Cubbies with ids in this cache will not be loaded/traversed.
   * @return a cache that maps ids to the cubbie objects in the graph defined by the roots, neighborhood function and
   * maximum depth.
   */
  @tailrec
  def load(roots: TraversableOnce[Cubbie],
           neighbors: PartialFunction[Cubbie, Seq[SlotInCollection[Cubbie]]],
           maxDepth: Int = Int.MaxValue,
           oldRefs: Refs = Map.empty): Refs = {

    if (maxDepth == 0) {
      oldRefs
    }
    else if (maxDepth == 1) {
      //fill-up roots into refs
      oldRefs ++ roots.map(c => c.id -> c).toMap
    }
    else {
      //fill-up roots into refs
      var refs = oldRefs ++ roots.map(c => c.id -> c).toMap

      //mapping from collections to the ids that need to be loaded
      val colls2ids = new HashMap[AbstractMongoCubbieCollection[Cubbie], List[Any]]

      //gather ids to load for each collection
      for (c <- roots) {
        for (slots <- neighbors.lift(c)) {
          for (slot <- slots) {
            for (idRef <- slot.slot.opt) {
              if (!refs.isDefinedAt(idRef)) {
                colls2ids(slot.coll) = colls2ids.getOrElse(slot.coll, Nil) :+ idRef
              }
            }
          }
        }
      }

      //now do loading
      var loaded: List[Cubbie] = Nil
      for ((coll, ids) <- colls2ids) {
        loaded = loaded ++ coll.findByIds(ids).toList
      }

      //instantiate the yield
      if (loaded.size > 0) load(loaded, neighbors, maxDepth - 1, refs) else refs
    }

  }


}