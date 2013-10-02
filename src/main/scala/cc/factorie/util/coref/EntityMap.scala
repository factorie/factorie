package cc.factorie.util.coref

import collection.mutable.{Set => MSet, ArrayBuffer, HashSet, Map, LinkedHashMap, HashMap}
import scala.collection.Set
import io.Source
import scala.language.implicitConversions

/**
 * Class to store a clustering over a subset of mentions.
 * @author sameer
 */
class GenericEntityMap[M] {
  val entities: Map[Long, MSet[M]] = new LinkedHashMap
  // eId -> Set[mention]
  // eId -> [mentions]
  val reverseMap: Map[M, Long] = new LinkedHashMap // mention -> eId

  def getEntities: Iterable[Set[M]] = {
    entities.values
  }

  def getEntity(mId: M): Long = {
    reverseMap(mId)
  }

  def getEntityIds: Set[Long] = {
    entities.keySet
  }

  def getMentionIds: Set[M] = {
    reverseMap.keySet
  }

  def numMentions = {
    reverseMap.size
  }

  def numEntities = {
    entities.size
  }

  def getMentions(eId: Long): Set[M] = {
    entities(eId)
  }

  def contains(mId: M): Boolean = reverseMap.contains(mId)

  def addCoreferentPair(m1: M, m2: M): Unit = {
    // if they don't exist, error!
    if (!contains(m1) || !contains(m2)) throw new Error("trying to merge mentions that are absent")
    // get the entities
    val m1e = getEntity(m1)
    val m2e = getEntity(m2)
    if (m1e == m2e) {
      // no need to do anything
    } else {
      // move all mentions in m2e to m1e
      val set = entities(m1e)
      for (mid <- getMentions(m2e)) {
        reverseMap.put(mid, m1e)
        set.add(mid)
      }
      entities.remove(m2e)
      // checks
      assert(getEntity(m1) == getEntity(m2))
      assert(!entities.contains(m2e))
    }
  }

  def addMention(mId: M, eId: Long): Unit = {
    if (reverseMap.contains(mId)) {
      if (reverseMap(mId) == eId) {
        // already in the right entity
      } else {
        // remove the previous mention from its entity
        val oldEntity: MSet[M] = entities(reverseMap(mId))
        oldEntity.remove(mId)
      }
    }
    var s: MSet[M] = null
    if (entities.contains(eId)) {
      s = entities(eId) // get existing set of mentions
    } else {
      s = new HashSet[M] // create a set of mentions
      entities.put(eId, s)
    }
    s.add(mId)
    // update the reverse map
    reverseMap.put(mId, eId)
  }

  def clear(): Unit = {
    entities.clear()
    reverseMap.clear()
  }

  def checkConsistency: Boolean = {
    var pass: Boolean = true
    for (mid <- reverseMap.keySet) {
      val entity: Set[M] = entities(reverseMap(mid))
      pass = pass && entity.contains(mid)
    }
    for (eid: Long <- entities.keys) {
      for (mid <- entities(eid)) {
        pass = pass && reverseMap(mid) == eid
      }
    }
    pass
  }

  override def toString: String = {
    val sb: StringBuffer = new StringBuffer
    for (entityId: Long <- entities.keySet) {
      sb.append("Entity(" + entityId + "): { ")
      for (mid <- entities(entityId)) {
        sb.append(" " + mid + " ")
      }
      sb.append(" }\n")
    }
    sb.toString
  }

}

trait Key[K, M] {
  this: GenericEntityMap[M] =>
  val map = new HashMap[K, Long]()
  val list = new ArrayBuffer[K]()

  implicit def key(id: Long): K = list(id.toInt)

  implicit def id(key: K): Long = map.getOrElseUpdate(key, {list += key; list.length.toLong - 1l})

  def addMention(mId: M, eId: K): Unit = addMention(mId, id(eId))

  def getEntityKey(mId: M): K = key(getEntity(mId))

  def getEntityKeys = getEntityIds.map(key(_)).toSet

  def getMentions(eId: K): Set[M] = getMentions(id(eId))
}

class EntityMap extends GenericEntityMap[Long]

object EntityMap {
  def initToSingletons[M](mentions: Iterable[M]): GenericEntityMap[M] = {
    val e: GenericEntityMap[M] = new GenericEntityMap[M]
    var entityIndex: Long = 0
    for (m <- mentions) {
      e.addMention(m, entityIndex)
      entityIndex += 1
    }
    e
  }

  // Assume input text file with format "entity_id \t mention_id" for each line
  def readFromFile(filename: String, mentionAlphabet: HashMap[String, Long]): EntityMap = {
    val map: EntityMap = new EntityMap
    val entityAlphabet: HashMap[String, Long] = new HashMap
    for (line: String <- Source.fromFile(filename).getLines()) {
      if (!line.trim.equals("")) {
        val splits = line.trim.split("\\t")
        println("Adding {" + splits(1) + "} to {" + splits(0) + "}")
        val mentionId: Long = mentionAlphabet.getOrElseUpdate(splits(1).trim, mentionAlphabet.size.toLong)
        val entityId: Long = entityAlphabet.getOrElseUpdate(splits(0).trim, entityAlphabet.size.toLong)
        map.addMention(mentionId, entityId)
      }
    }
    map
  }

  // Assume input text file with format "entity_id" for each line (mention)
  def readFromFile(filename: String): EntityMap = {
    val map: EntityMap = new EntityMap
    val entityAlphabet: HashMap[String, Long] = new HashMap
    var mentionId: Long = 0
    for (line: String <- Source.fromFile(filename).getLines()) {
      //println("Adding {" + mentionId + "} to {" + line.trim + "}")
      map.addMention(mentionId, entityAlphabet.getOrElseUpdate(line.trim, entityAlphabet.size.toLong))
      mentionId += 1
    }
    map
  }

}
