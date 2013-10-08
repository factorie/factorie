package cc.factorie.db.mongo

import cc.factorie.util.Cubbie
import com.mongodb.{MongoClient, Mongo}

/**
 * This class shows some example usage of cubbies and mongo serialization and querying.
 */
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
      val children = InverseSlot("children", (p: Person) => p.father)
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

    //reference attributes
    james.spouse ::= laura
    laura.spouse ::= james
    kid.father ::= james

    //calling apply on a slot without parameters returns the value of the slot.
    println("apply method calls")
    println(james.age())
    //calling apply with a parameter sets the slot to the given value and returns the cubbie.
    println(kid.age(10))


    val mongoConn = new MongoClient("localhost", 27017)
    val mongoDB = mongoConn.getDB("mongocubbie-test")
    val coll = mongoDB.getCollection("persons")
    coll.drop()
    val persons = new MongoCubbieCollection(coll, () => new Person, (p: Person) => Seq(Seq(p.name))) with LazyCubbieConverter[Person]

    persons += james
    persons += laura
    persons += kid

    //find all persons with name = "James" and set their name slot to "Jamie"
    persons.update(_.name.set("James"), _.name.update("Jamie"))

    //iterate over all persons in the collection
    for (p <- persons) {
      println(p._map)
    }

    //find all people with age == 50 but only return their age and name slots.
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

    //todo: this seems to not keep the name (= attribute not set in updated cubbie)
    persons.updateDelta(laura, updatedLaura)

    println(persons.mkString("\n"))

    //test batch id query
    println("****")
    println(persons.query(_.hobbies.contains("James")).mkString("\n"))
    println(persons.query(_.idsIn(Seq(1, 2))).mkString("\n"))
    println(persons.query(_.idIs(1)).mkString("\n"))

    //the graph loader loads a graph rooted around "james" by incrementally and recursively instantiating
    //the spouse of every person in the graph.
    implicit val refs = GraphLoader.load(Seq(james), {
      case p: Person => Seq(p.spouse in persons)
    })

    //the calls below use the implicit refs object returned by the graph loader to do dereferencing.
    println("James' spouse")
    println(james.spouse.deref)
    println("James' spouse's spouse")
    println(james.spouse.deref.spouse.deref)

    //same as above, but with both inverse and ref slots.
    val index = GraphLoader.load2(Seq(kid), {
      case p: Person => Seq(p.children of persons, p.father of persons)
    })

    println("Index:")
    println(index)
    println(james.children.value2(index))
    //ref slots need a Refs object (mapping from IDs to cubbies) and inv slots an inverter.
    println(james.children.value(GraphLoader.toInverter(index)))
    println(kid.father.deref(GraphLoader.toRefs(index)))

    println("Test Index 2")
    val index2 = GraphLoader.load2(Seq(james), {
      case p: Person => Seq(p.children of persons)
    })
    println(james.children.value(GraphLoader.toInverter(index2)))
    println(kid.father.deref(GraphLoader.toRefs(index2)))




    //or with fancy deref implicits
    //    import DerefImplicits._
    //    println(james.spouse-->spouse-->name.value)

    kid.name := "Kid 2"
    //more experimental stuff from here on:

    implicit val inverter = new CachedFunction(new LazyInverter(Map(manifest[Person] -> Seq(james, laura, kid))))

    println(james.children.value)

    val mongoInverter = new CachedFunction(new LazyMongoInverter(Map(manifest[Person] -> persons)))

    println(james.children.value(mongoInverter))

    val indexedInverter = new CachedFunction(new IndexedLazyInverter(Map(manifest[Person] -> Seq(james, laura, kid))))

    println(james.children.value(indexedInverter))

    //in memory caching
    implicit val indexer = new Indexer({
      case p: Person => Seq(p.name, p.age)
    })

    //these :=! calls inform the indexer of changes
    james.age :=! 51
    james.name :=! "Jamison"

    println(indexer.index)

  }
}
