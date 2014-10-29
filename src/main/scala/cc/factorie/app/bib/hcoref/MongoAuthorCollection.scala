package cc.factorie.app.bib.hcoref

import cc.factorie.app.nlp.hcoref._
import cc.factorie.util.Cubbie
import cc.factorie.variable.Var
import com.mongodb.DB
import scala.collection.mutable

/**
 * @author John Sullivan
 */
class HashMapCubbie extends Cubbie {
  def store(bag:Map[String, Double]) = {_map ++= bag;this}
  def fetch = _map.map{case (k, v) => k -> v.toString.toDouble}.toMap
}

object AuthorNodeCubbie {
  val deletions = mutable.HashSet[String]()
}

class AuthorNodeCubbie extends NodeCubbie[AuthorVars] {
  val truth = StringSlot("gt")
  val title = StringSlot("title")
  val canopy = StringSlot("canopy")
  val firstNameBag = new CubbieSlot("fnb", () => new HashMapCubbie)
  val middleNameBag = new CubbieSlot("mnb", () => new HashMapCubbie)
  val nickName = StringSlot("nn") // nickname  e.g. William Bruce Croft, nickname=Bruce; or William Freeman, nickname=Bill
  val emails = new CubbieSlot("emails", () => new HashMapCubbie)
  val topics = new CubbieSlot("topics", () => new HashMapCubbie)
  val keywords = new CubbieSlot("keywords", () => new HashMapCubbie)
  val venues = new CubbieSlot("venues", () => new HashMapCubbie)
  val coauthors = new CubbieSlot("coauthors", () => new HashMapCubbie)

  def newNodeCubbie = new AuthorNodeCubbie

  val deletionSet: mutable.HashSet[String] = AuthorNodeCubbie.deletions
}

class MongoAuthorCollection(db:DB, embeddingMap:Map[String, Array[Double]]) extends MongoNodeCollection[AuthorVars, AuthorNodeCubbie](Seq.empty, Seq.empty, db) {
  protected def newNodeCubbie = new AuthorNodeCubbie

  protected def newNodeVars[V <: Var](truth: String, vars: V*) = new AuthorVars()

  override protected def newNode(v:AuthorVars, nc:AuthorNodeCubbie) = if (nc.isMention.value) {
    new Mention[AuthorVars](AuthorVars.fromNodeCubbie(nc, embeddingMap), nc.id.toString)(null)
  } else {
    new Node[AuthorVars](AuthorVars.fromNodeCubbie(nc, embeddingMap), nc.id.toString)(null)
  }

  def getTruth(nc: AuthorNodeCubbie) = nc.truth.value
}
