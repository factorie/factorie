package cc.factorie.tutorial

import cc.factorie._
import cc.factorie.app.nlp.hcoref._
import cc.factorie.util.EvaluatableClustering
import cc.factorie.variable.{NoopDiff, BagOfWordsVariable, Var}
import com.mongodb.{DB, MongoClient}

import scala.collection.mutable
import scala.util.Random

/**
 * @author John Sullivan
 */
object HierCorefDemo {
  class WikiCorefVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val mentions:BagOfWordsVariable, val truth:BagOfWordsVariable) extends NodeVariables[WikiCorefVars] with Canopy with GroundTruth {

    //println("created Node with tbag: %s".format(truth.members.asHashMap))

    assert(truth.size <= 1, "created Node with tbag: %s".format(truth.members.asHashMap))

    def canopies = names.value.iterator.map(_._1).toList

    def this(n:Map[String, Double], c:Map[String, Double], m:Map[String, Double], t:String) = this(new BagOfWordsVariable(Nil, n), new BagOfWordsVariable(Nil, c), new BagOfWordsVariable(Nil, m), new BagOfWordsVariable(Seq(t)))
    def this()(implicit d:DiffList) = {this(new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable(), new BagOfWordsVariable()); if(d!=null) d += new NoopDiff(this)}

    def ++(other: WikiCorefVars)(implicit d: DiffList): WikiCorefVars = new WikiCorefVars(this.names ++ other.names,this.context ++ other.context, this.mentions ++ other.mentions, this.truth ++ other.truth)

    def --(other: WikiCorefVars)(implicit d: DiffList): WikiCorefVars = new WikiCorefVars(this.names -- other.names, this.context -- other.context, this.mentions -- other.mentions, this.truth -- other.truth)

    def ++=(other: WikiCorefVars)(implicit d: DiffList) {
      this.names.add(other.names.members)(d)
      this.context.add(other.context.members)(d)
      this.mentions.add(other.mentions.members)(d)
      this.truth.add(other.truth.members)(d)
      d += new NoopDiff(this) // this is needed to trigger unrolling on SingleBagTemplates like Entropy
    }

    def --=(other: WikiCorefVars)(implicit d: DiffList) {
      this.names.remove(other.names.members)(d)
      this.context.remove(other.context.members)(d)
      this.mentions.remove(other.mentions.members)(d)
      //println("removing tbag: %s of %s from %s of %s".format(this.truth.value.asHashMap, this.node.id, other.truth.value.asHashMap, other.node.id))
      this.truth.remove(other.truth.members)(d)
      d += new NoopDiff(this)
    }

    def getVariables: Seq[Var] = Seq(names, context, mentions)
  }

  object WikiCorefVars {
    def fromNodeCubbieVars[V <: Var](truth:String, vars: Seq[V]):WikiCorefVars = {
      val context = vars(0).asInstanceOf[BagOfWordsVariable]
      val names = vars(1).asInstanceOf[BagOfWordsVariable]
      val mentions = vars(2).asInstanceOf[BagOfWordsVariable]
      if(truth == ""){
        new WikiCorefVars(names, context, mentions, new BagOfWordsVariable())
      } else {
        new WikiCorefVars(names, context, mentions, new BagOfWordsVariable(Seq(truth)))
      }
    }
  }

  class WikiCorefModel(namesWeight:Double, namesShift: Double, contextWeight:Double, contextShift: Double, mentionsWeight:Double, mentionsShift: Double, distanceWeight:Double, distanceShift:Double)
    extends CorefModel[WikiCorefVars]
    with DirectScoringModel[WikiCorefVars] {
    this += new ChildParentCosineDistance(namesWeight, namesShift, {w:WikiCorefVars => w.names}, "names") {this.debugOff()}
    this += new ChildParentCosineDistance(contextWeight, contextShift, {w:WikiCorefVars => w.context}, "context") {this.debugOff()}
    this += new ChildParentCosineDistance(mentionsWeight, mentionsShift, {w:WikiCorefVars => w.mentions}, "mentions") {this.debugOff()}
    this += new ChildParentStringDistance(distanceWeight, distanceShift, {w:WikiCorefVars => w.names}, "names")
    this += new BagOfWordsEntropy(0.25, {w:WikiCorefVars => w.names})
  }

  val deletedEntries = mutable.HashSet[String]()
  class HcorefNodeCubbie extends NodeCubbie[WikiCorefVars] {

    val wikiUrl = StringSlot("wurl")
    val moveable = BooleanSlot("mv")

    val deletionSet: mutable.HashSet[String] = deletedEntries
    def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie()
  }

  class HcorefCubbieCollection(names: Seq[String], mongoDB: DB)
    extends MongoNodeCollection[WikiCorefVars, HcorefNodeCubbie](names, Seq.empty[String], mongoDB) {

    val WikiTitleExtractor1 = """.+?/wiki/(.+)""".r
    val WikiTitleExtractor2 = """.+?/\.\.\./(.+)""".r
    val WikiTitleExtractor3 = """.+?/(.+)""".r

    def getTitleFromWikiURL(wikiUrl: String): String = {
      val name = wikiUrl match {
        case WikiTitleExtractor1(name) => name
        case WikiTitleExtractor2(name) => name
        case WikiTitleExtractor3(name) => name
        case "" => ""
        case _ => throw new Error("cannot extract wikititle from " + wikiUrl)
      }
      name.replaceAll("_", " ")
    }

    def getTruth(nc: HcorefNodeCubbie) = getTitleFromWikiURL(nc.wikiUrl.value)

    protected def newNodeVars[V <: Var](truth: String, vars: V*) = WikiCorefVars.fromNodeCubbieVars(truth, vars)
    protected def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie
  }

  def main(args: Array[String]) = {

    implicit val random = new scala.util.Random()

    val mongoConn = new MongoClient("localhost", 27017)
    val mongoDb = mongoConn.getDB("wikicoref")
    val corefCollection = new HcorefCubbieCollection(Seq("mentions", "cbag", "nbag", "mbag"), mongoDb)
    val allMentions = corefCollection.loadAll.filterNot(_.variables.truth.size == 0)//.filterNot(_.source == "wp")
    println("Done loading")

    val model = new WikiCorefModel(2.0, -0.25, 2.0, -0.25, 2.0, -0.25, 0.0, 0.0)

    val numSamples = 200000
    val time = System.currentTimeMillis()

    val sampler = new CorefSampler[WikiCorefVars](model, allMentions, numSamples)
      with AutoStoppingAcceptSampler[WikiCorefVars]
      with CanopyPairGenerator[WikiCorefVars]
      with NoSplitMoveGenerator[WikiCorefVars]
      with DebugCoref[WikiCorefVars]
      with PostSampler[WikiCorefVars, WikiCorefModel] {
      def autoStopAcceptThreshold = 10000
      val logger = Logger.default

      def newInstance(implicit d: DiffList): Node[WikiCorefVars] = new Node[WikiCorefVars](new WikiCorefVars/*, nextId*/) {
        def canopyIds: Set[String] = Set.empty[String]
      }
    }

    sampler.infer

    println(EvaluatableClustering.evaluationString(allMentions.predictedClustering, allMentions.trueClustering))
  }

  /*
    Example system to perform coreference and return a map of mention id -> entity id
   */
  def doCoreference:Iterable[(String, String)] = {

    val mongoConn = new MongoClient("localhost", 27017)
    val mongoDb = mongoConn.getDB("wikicoref-bos")
    val corefCollection = new HcorefCubbieCollection(Seq("mentions", "cbag", "nbag", "mbag"), mongoDb)

    implicit val random = new Random()
    val mentions = corefCollection.loadAll

    val model = new WikiCorefModel(2.0, -0.25, 2.0, -0.25, 2.0, -0.25, 0.0, 0.0)
    val numSamples = 20000

    val sampler = new HierarchicalCorefSampler[WikiCorefVars](model, mentions, numSamples) {
      override def newInstance(implicit d: DiffList): Node[WikiCorefVars] = new Node[WikiCorefVars](new WikiCorefVars)
    }

    sampler.infer

    mentions.filter{  e => e.isMention && e.getParent.isDefined}.map{m => m.uniqueId -> m.root.uniqueId}
  }

  def doCoreference(mentions:Iterable[Node[WikiCorefVars]], iterations:Int, model:WikiCorefModel):Iterable[(Node[WikiCorefVars], Node[WikiCorefVars])] = {

    implicit val random = new Random()

    val sampler = new HierarchicalCorefSampler[WikiCorefVars](model, mentions, iterations) with DebugCoref[WikiCorefVars] {
      val logger = Logger.default
      override def newInstance(implicit d: DiffList): Node[WikiCorefVars] = new Node[WikiCorefVars](new WikiCorefVars)
    }

    sampler.infer
    mentions.filter{  e => e.isMention && e.getParent.isDefined}.map{m => m -> m.root}
  }


}
