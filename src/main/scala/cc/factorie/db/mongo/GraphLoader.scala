package cc.factorie.db.mongo

import cc.factorie.util.Cubbie
import annotation.tailrec
import scala.collection.{MapProxy, Map => GenericMap, JavaConversions}
import collection.mutable.HashMap


object GraphLoader {

  case class SlotInCollection[+R <: Cubbie](slot: Cubbie#AbstractRefSlot[R], coll: AbstractCubbieCollection[R])

  type Refs = GenericMap[Any, Cubbie]
  type Invs = Cubbie#InverseSlot[Cubbie] => Iterable[Cubbie]

  //Map from (cubbie class, attribute name, value) to the cubbies of that class with that attribute value
  type Graph = GenericMap[(Class[Cubbie], String, Any), Iterable[Cubbie]]

  /**
   * Loads a cache from ids to cubbies based on the root objects and a neighborhood function.
   * @param roots the cubbies to start with.
   * @param neighbors the neigborhood function from cubbies to (refslot,collection) pairs.
   * @param maxDepth How far from the root are we allowed to travel. If maxDepth == 0 no ids are added to the cache, for
   *                 maxDepth == 1 only the roots are added to the cache, for maxDeptn == 2 the roots immediate children etc.
   * @param oldRefs an existing cache. Cubbies with ids in this cache will not be loaded/traversed.
   * @return a cache that maps ids to the cubbie objects in the graph defined by the roots, neighborhood function and
   *         maximum depth.
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
      val colls2ids = new HashMap[AbstractCubbieCollection[Cubbie], List[Any]]

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

  case class InvSlotInCollection[+R <: Cubbie](invSlot: Cubbie#AbstractInverseSlot[R], coll: AbstractCubbieCollection[R])


  //
  @tailrec
  def load2(roots: TraversableOnce[Cubbie],
            neighbors: PartialFunction[Cubbie, Seq[InvSlotInCollection[Cubbie]]],
            maxDepth: Int = Int.MaxValue,
            oldGraph: Graph = Map.empty): Graph = {

    if (maxDepth == 0) {
      oldGraph
    }
    else if (maxDepth == 1) {
      //fill-up roots into refs
      //todo: does this need to fill up inverse links too?
      oldGraph ++ roots.map(c => (c.cubbieClass, c.IdSlot.name, c.id) -> Seq(c))
      //oldGraph.copy(refs = oldGraph.refs ++ roots.map(c => c.id -> c).toMap)
    }
    else {
      //fill-up roots into refs
      var graph = oldGraph ++ roots.map(c => (c.cubbieClass, c.IdSlot.name, c.id) -> Seq(c))

      //mapping from collections and attributes to the values that need to be queried for.
      val collsAttr2ids = new HashMap[(InvSlotInCollection[Cubbie], String), List[Any]]

      //gather queries to execute
      for (c <- roots) {
        for (slots <- neighbors.lift(c)) {
          for (slotAndColl <- slots) {
            val invSlot = slotAndColl.invSlot
            val foreignSlot = invSlot.foreignSlot(slotAndColl.coll.prototype)
            val foreignCubbieClass = foreignSlot.cubbie.getClass.asInstanceOf[Class[Cubbie]]
            for (target <- invSlot.target) {
              val attrName = foreignSlot.name
              if (!graph.isDefinedAt((foreignCubbieClass, attrName, target))) {
                collsAttr2ids(slotAndColl -> attrName) = collsAttr2ids.getOrElse(slotAndColl -> attrName, Nil) :+ target
              }
            }
          }
        }
      }
      //now do the loading
      var loaded: List[Cubbie] = Nil
      for (((coll, attrName), targets) <- collsAttr2ids) {
        val prototype = coll.coll.prototype
        val foreignClass = prototype.cubbieClass
        val result = coll.coll.findByAttribute(attrName, targets).toList
        //replace cubbies with already loaded cubbies with same id
        val deduped = result.map(c => {
          val existing = graph.get((foreignClass, prototype.IdSlot.name, c.id))
          existing match {
            case Some(refs) => refs.head
            case _ => loaded = loaded :+ c; c
          }
        })
        val grouped = deduped.groupBy(c => {
          val foreignSlot = coll.invSlot.foreignSlot(c)
          val foreignValue = foreignSlot.value
          (foreignClass, attrName, foreignValue)
        })
        graph = graph ++ grouped
      }
      if (loaded.size > 0) load2(loaded, neighbors, maxDepth - 1, graph) else graph
    }

  }


}
