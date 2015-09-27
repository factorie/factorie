package cc.factorie.app.bib.hcoref

import cc.factorie.app.nlp.hcoref._
import cc.factorie.util.namejuggler.{NonemptyString, PersonNameWithDerivations}
import cc.factorie.util.{Cubbie, DoubleSeq}
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

  def fromNode(n:Node[AuthorVars]):AuthorNodeCubbie = fromNode(new AuthorNodeCubbie, n)
  def fromNode(nc:AuthorNodeCubbie, n:Node[AuthorVars]):AuthorNodeCubbie = {
    nc.fullName.set(n.variables.fullName)
    nc.firstNameBag.set(new HashMapCubbie().store(n.variables.firstNames.value.asHashMap.toMap))
    nc.middleNameBag.set(new HashMapCubbie().store(n.variables.middleNames.value.asHashMap.toMap))
    if (n.variables.truth.size > 0) { // avoid creating empty truth bags
      nc.truth.set(new HashMapCubbie().store(n.variables.truth.value.asHashMap.toMap))
    }
    nc.coauthors.set(new HashMapCubbie().store(n.variables.firstNames.value.asHashMap.toMap))
    nc.venues.set(new HashMapCubbie().store(n.variables.firstNames.value.asHashMap.toMap))
    nc.keywords.set(new HashMapCubbie().store(n.variables.firstNames.value.asHashMap.toMap))
    nc.canopies.set(n.variables.canopies)
    nc.parentRef.set(n.getParent.map(_.uniqueId))
    nc.topicEmbedding.set(DoubleSeq(n.variables.topics.value))
    nc.title.set(n.variables.title)
    nc.isMention.set(n.isMention)
    nc.id_=(n.uniqueId)
    nc
  }

  def fromTSVStringEmbedding(s:String):AuthorNodeCubbie = {
    val arr = s.split("\t")
    val (args, embeddings) = arr.splitAt(11)
    val nc = fromTSVString(args)
    nc.topicEmbedding set DoubleSeq(embeddings.map(_.toDouble))
    nc
  }
  def fromFullTSVString(s:String):AuthorNodeCubbie = fromTSVString(s.split("\t"))

  def fromRawTSVString(s:String, keyStore:Keystore = null):AuthorNodeCubbie = {
    val id :: rawAuthor :: rawTitle :: rawVenues :: rawKeywords :: rawCoauthors = s.split("\t").toList
    val authorName = PersonNameWithDerivations(NonemptyString(rawAuthor)).inferFully
    val coAuthorCanopies = rawCoauthors.map(Canopies.fromString).filterNot(_.isEmpty).map(s => s -> 1.0).toMap
    val venues = rawVenues.split("""\s+""").map(s => s -> 1.0).toMap
    val keyWords = rawKeywords.split("""\s+""").map(s => s -> 1.0).toMap
    val nc = new AuthorNodeCubbie()
    nc.id = id
    nc.fullName set rawAuthor
    nc.firstNameBag set new HashMapCubbie().store(authorName.firstName.map(nes => nes.s -> 1.0).toMap)
    nc.middleNameBag set new HashMapCubbie().store(authorName.middleNames.map(nes => nes.s -> 1.0).toMap)
    nc.coauthors set new HashMapCubbie().store(coAuthorCanopies)
    nc.keywords set new HashMapCubbie().store(keyWords)
    nc.venues set new HashMapCubbie().store(venues)
    nc.canopies set Seq(Canopies.fromString(rawAuthor))
    nc.title set rawTitle
    nc.isMention set true
    if(keyStore != null) {
      nc.topicEmbedding set DoubleSeq(keyStore.generateVector(rawTitle.split( """\s+""")))
    }
    nc
  }

  private def splitBagString(s:String):Map[String, Double] = s.split("""\s+""").map { kv =>
    val arr = kv.split(":")
    if(arr.size == 2) {
      val Array(key, valString) = arr
      key -> valString.toDouble
    } else {
      kv -> 1.0
    }
  }.toMap

  def buildBagString(bag:Map[String, Double]):String = bag.map{case(key, value) => key + ":" + value}.mkString(" ")


  private def fromTSVString(arr:Array[String]):AuthorNodeCubbie = {
    val Array(id, fullName, firstName, middleName, coauthors, keywords, venues, canopy, title, isMentionString, parentRefString, truthString) = arr
    val nc = new AuthorNodeCubbie
    nc.id = id
    nc.fullName set fullName
    nc.firstNameBag set new HashMapCubbie().store(splitBagString(firstName))
    nc.middleNameBag set new HashMapCubbie().store(splitBagString(middleName))
    nc.coauthors set new HashMapCubbie().store(splitBagString(coauthors))
    nc.keywords set new HashMapCubbie().store(splitBagString(keywords))
    nc.venues set new HashMapCubbie().store(splitBagString(venues))
    nc.canopies set Seq(canopy)
    nc.title set title
    nc.isMention set (isMentionString.toLowerCase == "true")
    if(parentRefString.nonEmpty) {
      nc.parentRef set parentRefString
    }
    if(truthString.nonEmpty) {
      nc.truth set new HashMapCubbie().store(splitBagString(truthString))
    }
    nc
  }
}

class AuthorNodeCubbie extends NodeCubbie[AuthorVars] {
  val truth = new CubbieSlot("gtbag", () => new HashMapCubbie)
  val title = StringSlot("title")
  val fullName = StringSlot("full-name")
  val firstNameBag = new CubbieSlot("fnb", () => new HashMapCubbie)
  val middleNameBag = new CubbieSlot("mnb", () => new HashMapCubbie)
  val nickName = StringSlot("nn") // nickname  e.g. William Bruce Croft, nickname=Bruce; or William Freeman, nickname=Bill
  val emails = new CubbieSlot("emails", () => new HashMapCubbie)
  val topics = new CubbieSlot("topics", () => new HashMapCubbie)
  val topicEmbedding = new DoubleSeqSlot("topicEmb")
  val keywords = new CubbieSlot("keywords", () => new HashMapCubbie)
  val venues = new CubbieSlot("venues", () => new HashMapCubbie)
  val coauthors = new CubbieSlot("coauthors", () => new HashMapCubbie)

  def newNodeCubbie = new AuthorNodeCubbie

  val deletionSet: mutable.HashSet[String] = AuthorNodeCubbie.deletions

  import AuthorNodeCubbie._
  def toTSVString = Seq(id.toString,
                        fullName.value,
                        buildBagString(firstNameBag.value.fetch),
                        buildBagString(middleNameBag.value.fetch),
                        buildBagString(coauthors.value.fetch),
                        buildBagString(keywords.value.fetch),
                        buildBagString(venues.value.fetch),
                        canopies.value.head,
                        title.value,
                        isMention.value.toString,
                        if(parentRef.isDefined) parentRef.value else "",
                        buildBagString(truth.value.fetch)).mkString("\t")

  def titleString = title.value

  def setEmbeddings(ks:Keystore):AuthorNodeCubbie = topicEmbedding.set(DoubleSeq(ks.generateVector(title.value.split("""\s+"""))))
}

class MongoAuthorCollection(db:DB, embeddingMap:Keystore) extends MongoNodeCollection[AuthorVars, AuthorNodeCubbie](Seq("authors"), Seq.empty, db) {
  protected def newNodeCubbie = new AuthorNodeCubbie

  protected def newNodeVars[V <: Var](truth: String, vars: V*) = new AuthorVars()

  def loadMentions: Seq[Mention[AuthorVars]] = nodeCubbieColl.query({nc => nc.isMention.set(true)}).map(newNode(null, _).asInstanceOf[Mention[AuthorVars]]).toSeq

  override protected def newNode(v:AuthorVars, nc:AuthorNodeCubbie) = if (nc.isMention.value) {
    new Mention[AuthorVars](AuthorVars.fromNodeCubbie(nc, embeddingMap), nc.id.toString)(null)
  } else {
    new Node[AuthorVars](AuthorVars.fromNodeCubbie(nc, embeddingMap), nc.id.toString)(null)
  }

  override protected def cubbify(n:Node[AuthorVars]) = AuthorNodeCubbie.fromNode(n)

  /** Set the collection to only un-corefed mentions */
  def setToSingletons() {
    nodeCubbieColl.remove(c => c.isMention.set(false))
  }

  def updateCollection(ns:Iterable[Node[AuthorVars]]) {
    AuthorNodeCubbie.deletions.map{del =>
      nodeCubbieColl.remove{c => c.id = del; c}
    }
    println("Removed %s deleted records".format(AuthorNodeCubbie.deletions.size))
    ns.foreach { n =>
      val nc = AuthorNodeCubbie.fromNode(n)
      nodeCubbieColl.update({c => c.id = n.uniqueId; c}, {c => AuthorNodeCubbie.fromNode(c, n)}, true)
    }
    println("Upserted %d nodes".format(ns.size))
  }

  def getTruth(nc: AuthorNodeCubbie) = nc.truth.value.fetch.head._1
}
