package cc.factorie.tutorial

import cc.factorie.app.nlp.hcoref._
import cc.factorie.variable.BagOfWordsVariable
import cc.factorie.variable.{Var, DiffList}
import com.mongodb.{MongoClient, DB}
import cc.factorie._
import cc.factorie.util.EvaluatableClustering
import scala.io.Source
import java.io.{FileWriter, BufferedWriter}
import cc.factorie.infer.SettingsMaximizer

/**
 * @author John Sullivan
 */
object HierCorefDemo {
  class WikiCorefVars(val names:BagOfWordsVariable, val context:BagOfWordsVariable, val mentions:BagOfWordsVariable, val trueCluster:String = null) extends NodeVariables[WikiCorefVars] with Canopy with GroundTruth {

    val truth = new BagOfWordsVariable(Seq(trueCluster))

    def canopies = names.value.iterator.map(_._1).toList

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
  }

  object WikiCorefModel extends CorefModel[WikiCorefVars]{
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.names}) {this.debugOff()}
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.context}) {this.debugOff()}
    this += new ChildParentCosineDistance(2.0, -0.25, {w:WikiCorefVars => w.mentions}) {this.debugOff()}
    this += new BagOfWordsEntropy(0.25,{w:WikiCorefVars => w.names})
  }

  class HcorefNodeCubbie extends NodeCubbie[WikiCorefVars, Node[WikiCorefVars] with Persistence with NodeSource] {

    def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie()
  }

  class HcorefCubbieCollection(names: Seq[String], mongoDB: DB)
    extends MongoNodeCollection[WikiCorefVars, Node[WikiCorefVars] with Persistence with NodeSource, HcorefNodeCubbie](names, mongoDB) {

    protected def newBOWCubbie = new BOWCubbie()

    protected def newNodeVars[V <: Var](truth: String, vars: V*): WikiCorefVars = {
      val context = vars(0).asInstanceOf[BagOfWordsVariable]
      val names = vars(1).asInstanceOf[BagOfWordsVariable]
      val mentions = vars(2).asInstanceOf[BagOfWordsVariable]
      if(truth == ""){
        new WikiCorefVars(names, context, mentions)
      } else {
        new WikiCorefVars(names, context, mentions, truth)
      }
      //new WikiCorefVars(vars(0).asInstanceOf[BagOfWordsVariable], vars(1).asInstanceOf[BagOfWordsVariable], vars(2).asInstanceOf[BagOfWordsVariable], truth)(null)
    }

    protected def newNodeCubbie: HcorefNodeCubbie = new HcorefNodeCubbie

    protected def newNode(v: WikiCorefVars, nc: HcorefNodeCubbie) = {
      if (nc.isMention.value) {
        new Mention[WikiCorefVars](v, nc.id.toString)(null) with Persistence with NodeSource {
          def canopyIds: Set[String] = nc.canopies.value.toSet

          protected val loadedFromDb = true
          def source = nc.source.value
          def moveable = nc.moveable.value
        }
      }
      else {
        new Node[WikiCorefVars](v, nc.id.toString)(null) with Persistence with NodeSource {

          def canopyIds: Set[String] = this.leaves.collect {
            case leaf: Mention[WikiCorefVars] => leaf.variables.canopies
          }.flatten.toSet

          protected val loadedFromDb = true
          def source = nc.source.value
          def moveable = nc.moveable.value
        }
      }
    }
  }

  def main(args: Array[String]) = {

    implicit val random = new scala.util.Random()

    val mongoConn = new MongoClient("localhost", 27017)
    val mongoDb = mongoConn.getDB("wikicoref")
    val corefCollection = new HcorefCubbieCollection(Seq("mentions", "cbag", "nbag", "mbag"), mongoDb)
    val allMentions = corefCollection.loadAll.filterNot(_.variables.trueCluster == null).filterNot(_.source == "wp")
    println("Done loading")

    //println("Found %d unmovable entities, here is an example: %s".format(allMentions.count(e => !e.moveable), allMentions.filter(e => !e.moveable).head))

    println(allMentions.map(_.variables.truth).toSet)
    //assert(allMentions.map(_.variables.truth).toSet.size == 4)

    println(allMentions.groupBy(_.variables.trueCluster).mapValues(_.size))

    val mentionSeq = Source.fromFile("gold-moves").getLines().map{ line =>
      val (e1, e2, parent) = line.split('\t') match {
        case Array(e) => (e, "NONE", "NONE")
        case Array(eOne, eTwo) => (eOne, eTwo, "NONE")
        case Array(eOne, eTwo, pOne) => (eOne, eTwo, pOne)
      }
      (e1, e2, parent)
    }.toIndexedSeq

    val numSamples = 100000
    val time = System.currentTimeMillis()
    val sampler = new CorefSampler[WikiCorefVars](WikiCorefModel, allMentions, numSamples)
      with AutoStoppingSampler[WikiCorefVars]
      with CanopyPairGenerator[WikiCorefVars]
      with NoSplitMoveGenerator[WikiCorefVars]
      with DebugCoref[WikiCorefVars] {


      def autoStopThreshold = 10000

      def mentionSequence = mentionSeq


      def newInstance(implicit d: DiffList): Node[WikiCorefVars] = {
        // println("New old WikiCoref Node")
        new Node[WikiCorefVars](new WikiCorefVars/*, nextId*/) {

          def canopyIds: Set[String] = Set.empty[String]
        }
      }

      def outerGetBagSize(n: Node[WikiCorefVars]) = n.variables.context.size

      //def mentionSequence = mentionSeq
    }

    sampler.infer
    /*
    val wrt = new BufferedWriter(new FileWriter("trace"))
    wrt.write(Verbosity.header)
    wrt.newLine()
    sampler.verbosities.foreach { v =>
      wrt.write(v.writeOut)
      wrt.newLine()
    }
    wrt.flush()
    wrt.close()
    */

    println(EvaluatableClustering.evaluationString(allMentions.predictedClustering, allMentions.trueClustering))
  }
  
}
