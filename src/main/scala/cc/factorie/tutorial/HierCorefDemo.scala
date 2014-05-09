package cc.factorie.tutorial

import cc.factorie.app.nlp.hcoref.BagOfWordsVariable
import cc.factorie.app.nlp.xcoref._
import cc.factorie.variable.{Var, DiffList}
import scala.util.Random
import com.mongodb.{DB, MongoClient}


/**
 * @author John Sullivan
 */
object HierCorefDemo {

  /*
    This is the vars class that stores all of the variables compared in coreference sampling.
   */
  class WikiCorefVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val mentions:BagOfWordsVariable, val truth:String = null) extends NodeVariables[WikiCorefVars] with Canopy {


    override def canopies: Seq[String] = names.iterator.toList.map(_._1)

    def this(n:Map[String, Double], c:Map[String, Double], m:Map[String, Double]) = this(new BagOfWordsVariable(Nil, n), new BagOfWordsVariable(Nil, c), new BagOfWordsVariable(Nil, m))
    def this()(implicit d:DiffList) = this(new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable())

    private def newBagAdding(b1:BagOfWordsVariable, b2:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
      val n = new BagOfWordsVariable()
      b1.foreach{ case(word, value) =>
        n.add(word, value)(d)
      }
      b2.foreach{ case(word, value) =>
        n.add(word, value)(d)
      }
      n
    }

    private def newBagSubtracting(b1:BagOfWordsVariable, b2:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
      val n = new BagOfWordsVariable()
      b1.foreach{ case(word, value) =>
        n.add(word, value)(d)
      }
      b2.foreach{ case(word, value) =>
        n.remove(word,value)(d)
      }
      n
    }

    def ++(other: WikiCorefVars)(implicit d: DiffList): WikiCorefVars = new WikiCorefVars(newBagAdding(this.names, other.names), newBagAdding(this.context, other.context), newBagAdding(this.mentions, other.mentions))

    def --(other: WikiCorefVars)(implicit d: DiffList): WikiCorefVars = new WikiCorefVars(newBagSubtracting(this.names, other.names), newBagSubtracting(this.context, other.context), newBagSubtracting(this.mentions, other.mentions))

    def ++=(other: WikiCorefVars)(implicit d: DiffList) {
      this.names.add(other.names.members)(d)
      this.context.add(other.context.members)(d)
      this.mentions.add(other.mentions.members)(d)
    }

    def --=(other: WikiCorefVars)(implicit d: DiffList) {
      this.names.remove(other.names.members)(d)
      this.context.remove(other.context.members)(d)
      this.mentions.remove(other.mentions.members)(d)
    }

    def getVariables: Seq[Var] = Seq(names, context, mentions)

    def nameString: String = names.toString()
  }

  /*
    The model used to score variables between nodes.
   */
  class WikiCorefModel extends CorefModel[WikiCorefVars] {
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.names}) {this.debugOff()}
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.context}) {this.debugOff()}
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.mentions}) {this.debugOff()}
    this += new BagOfWordsEntropy(0.25, {w:WikiCorefVars => w.names}) {this.debugOff()}
  }

  /*
    Code to load mentions from Mongo
   */
  class HcorefNodeCubbie extends NodeCubbie[WikiCorefVars, Node[WikiCorefVars] with Persistence] {

    def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie()
  }

  class HcorefCubbieCollection(names: Seq[String], mongoDB: DB)
    extends MongoNodeCollection[WikiCorefVars, Node[WikiCorefVars] with Persistence, HcorefNodeCubbie](names, mongoDB) {

    protected def newBOWCubbie = new BOWCubbie()

    protected def newNodeVars[V <: Var](truth: String, vars: V*): WikiCorefVars = {
      if(truth == ""){
        new WikiCorefVars(vars(0).asInstanceOf[BagOfWordsVariable], vars(1).asInstanceOf[BagOfWordsVariable], vars(2).asInstanceOf[BagOfWordsVariable])
      } else {
        new WikiCorefVars(vars(0).asInstanceOf[BagOfWordsVariable], vars(1).asInstanceOf[BagOfWordsVariable], vars(2).asInstanceOf[BagOfWordsVariable], truth)
      }
    }

    protected def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie

    protected def newNode(v: WikiCorefVars, nc: HcorefNodeCubbie) = {
      if (nc.isMention.value) {
        new Mention[WikiCorefVars](v, nc.id.toString)(null) with Persistence {
          protected val loadedFromDb = true
        }
      }
      else {
        new Node[WikiCorefVars](v, nc.id.toString)(null) with Persistence {
          protected val loadedFromDb = true
        }
      }
    }
  }
  

  /*
    Example system to perform coreference and return a map of mention id -> entity id
   */
  def doCoreference:Iterable[(String, String)] = {

    val mongoConn = new MongoClient("localhost", 27017)
    val mongoDb = mongoConn.getDB("wikicoref")
    val corefCollection = new HcorefCubbieCollection(Seq("mentions", "cbag", "nbag", "mbag"), mongoDb)
    
    implicit val random = new Random()
    val mentions = corefCollection.loadAll

    val model = new WikiCorefModel
    val numSamples = 20000

    val sampler = new HierarchicalCorefSampler[WikiCorefVars](model, mentions, numSamples) {
      override def newInstance(implicit d: DiffList): Node[WikiCorefVars] = new Node[WikiCorefVars](new WikiCorefVars)
    }

    sampler.infer

    mentions.filter{e => e.isMention && e.parent.isDefined}.map{m => m.id -> m.root.id}
  }
}
