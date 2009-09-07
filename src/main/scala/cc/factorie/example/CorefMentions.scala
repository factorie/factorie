package cc.factorie.example

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie.util.Implicits._

class CorefMentionsModel extends Model 
  with MHPerceptronLearning
  //with MHMIRALearning 
  //with MHCWLearning
{
  /** A random variable for a mention */
  class Mention(val name:String, val trueEntity:Int, initialEntity:Entity, val allMentions:Seq[Mention]) extends PrimitiveVariable(initialEntity) {
    //Console.println ("new Mention "+name+" domain "+domain+" size "+mentionList.size)
    // When this mention is assigned to an entity, update the mention
    override def set(e:Entity)(implicit d:DiffList) : Unit = {
      //        Console.println ("Mention set old="+value+" new="+e)
      if (e != entity) {
        if (entity != null) entity.remove(this)
        e.add(this)
        super.set(e)
      }
    }
    // Just for readability
    def entity = _value
    // Define true eval measure by number of other mentions that are correctly coref'ed with this
    override def trueScore = {
      allMentions.foldLeft(0.0)((total,m) => 
        if (m.trueEntity == this.trueEntity) {
          if (m.entity == this.entity) total + 1
          else total - 1
        } else {
          if (m.entity == this.entity) total - 1
          else total + 1
        })
    }
    override def toString = "Mention(" + name +"=="+ entity.canonical +")"
  }

  /** A random variable for an entity, which is merely a HashSet collection of Mentions */
  class Entity(val canonical:String) extends SetVariable[Mention] {
    def trueScore = 0.0 // Mention takes care of all trueScore'ing
    def mentions = members // Just for readability
    override def toString = "Entity("+canonical+":"+mentions.toSeq.size+")"
  }

  /** A feature vector random variable measuring affinity between two mentions */
  val features = List("match", "-match", "prefix1", "-prefix1", "prefix2", "-prefix2", 
                      "prefix3", "-prefix3", "substring", "-substring",
                      "length", "containsword")
  features.foreach(f=>IndexedDomain[AffinityVector].index(f)) // initialize the index
  IndexedDomain[AffinityVector].freeze
  class AffinityVector(s1:String, s2:String) extends VectorVariable[String] {
    type VariableType = AffinityVector
    if (s1 equals s2) this += "match" else this += "-match"
    if (s1.substring(0,1) equals s2.substring(0,1)) this += "prefix1" else this += "-prefix1"
    if (s1.substring(0,2) equals s2.substring(0,2)) this += "prefix2" else this += "-prefix2"
    if (s1.substring(0,3) equals s2.substring(0,3)) this += "prefix3" else this += "-prefix3"
    if (s1.contains(s2) || s2.contains(s1)) this += "substring" else this += "-substring"
    if (s1.length == s2.length) this += "length"
    s1.split(" ").foreach(s => if (s2.contains(s)) this += "containsword")
    s2.split(" ").foreach(s => if (s1.contains(s)) this += "containsword")
    // Also consider caching mechanisms
  }


  // Pairwise affinity factor between Mentions in the same partition
  modelTemplates += new Template2[Mention,Mention] with Statistic1[AffinityVector]
    with PerceptronLearning 
    //with MIRALearning
    //with CWLearning
  {
    def unroll1 (mention:Mention) = for (other <- mention.entity.mentions; if (other != mention)) yield 
      if (mention.hashCode > other.hashCode) Factor(mention, other)
      else Factor(other, mention)
    def unroll2 (mention:Mention) = Nil // symmetric
    def statistic (mention1:Mention, mention2:Mention) = Stat(new AffinityVector(mention1.name, mention2.name))
  }.init

  // Pairwise repulsion factor between Mentions in different partitions
  modelTemplates += new Template2[Mention,Mention] with Statistic1[AffinityVector]
  with PerceptronLearning 
  //with MIRALearning
  //with CWLearning
  {
    override def unroll(d:Diff) = d.variable match {
      case mention : Mention => d match {
        case mention.PrimitiveDiff(oldEntity:Entity, newEntity:Entity) => 
          for (other <- oldEntity.mentions; if (other.entity != mention.entity)) yield Factor(mention, other);
        case _ => super.unroll(d)
      }
      case _ => super.unroll(d)
    }
    def unroll1 (mention:Mention) = for (other <- mention.allMentions; if (other.entity != mention.entity)) yield Factor(mention, other);
    def unroll2 (mention:Mention) = Nil // symmetric
    def statistic(mention1:Mention, mention2:Mention) = Stat(new AffinityVector(mention1.name, mention2.name))
  }.init
  
  /*    // Factor testing if all the mentions in this entity share the same prefix of length 1
   // A first-order-logic feature!
   object ForallMentionPrefix1Matches extends Factor11[Entity,Bool] with PerceptronLearning {
   def sufficient (entity:Entity) = {
   if (entity.mentions.isEmpty)
   Bool(true)
   else {
   val prefix1 = entity.mentions.elements.next.name.substring(0,1)
   if (entity.mentions.forall(m => prefix1 equals m.name.substring(0,1)))
   Bool(true)
   else
   Bool(false)
   }
   }
   } */
} 


object CorefMentionsDemo {

  def main(args: Array[String]) : Unit = {
    val model = new CorefMentionsModel()
    val corefWorld = new World {
      import model._

      var mentionList = new ArrayBuffer[Mention]();
      var entityList = new ArrayBuffer[Entity]();

      val data = Array(
        Array("Andrew McCallum", "Andrew MacCallum", "Angrew McCallum", "McCallum", "A. McCallum"),
        Array("Michael Wick", "Mike Wick", "Michael Andrew Wick", "Wick", "Wick"),
        Array("Khashayar Rohanemanesh", "Khash R.", "Kesh Rohanemanesh"),
        Array("Aron Culotta", "Andrew Culotta", "A. Culotta", "Culotta McCallum", "Culotta", "Culotta"),
        Array("Charles Sutton", "Charles A. Sutton", "Sutton", "Sutton")
      )
      
      // Create variables for the data
      var eIndex = 0
      def entityIndex = { eIndex += 1; eIndex }
      var mentions = data.zipWithIndex.map(x => {
        val names = x._1
        val trueEntityIndex = x._2
        // Initialize by putting each mention into its own separate entity
        names.map(s => {
          val newEntity = new Entity("e" + entityIndex);
          val newMention = new Mention(s, trueEntityIndex, newEntity, mentionList);
          entityList += newEntity;
          mentionList += newMention;
          newMention;
        })
      })	


      // Define the proposal distribution
      //var sampler = new CWLearner {
      //var sampler = new MHMIRALearner {
      var sampler = new MHPerceptronLearner {
        def propose(difflist:DiffList) : Double = {
          // Pick a random mention
          val m = mentionList.sample
          // Pick a random place to move it, either an existing Entity or a newly created one
          var e = if (random.nextDouble < 0.8) entityList.sampleFiltered((e:Entity)=>e.size>0) else { var ne=new Entity("e"+entityIndex); entityList += ne; ne}
          // Make sure that we don't try to move it to where it already was
          if (e == m.entity) {
            //              Console.println ("Proposal: Trying to move to self, so create new Entity.")
            e = new Entity("e"+entityIndex)
            entityList += e
          }
          // Move it
          //            Console.println ("Proposal.jump moving "+m+" to "+e)
          m.set(e)(difflist)
          // log-Q-ratio shows that forward and backward jumps are equally likely
          return 0.0
        }
        override def mhPerceptronPostProposalHook = {
          if (/*false*/ iterations % 1000 == 0) {
            System.out.println("UPS: " + numUpdates);
            modelTemplatesOf[LogLinearScoring].foreach(f => Console.println (f.toString+" weights = "+f.weights.toList))
            Console.println ("All entities")
            entityList.filter(e=>e.size>0).foreach(e => Console.println(e.toString +" "+ e.mentions.toList))
            Console.println ("All mentions")
            mentionList.foreach(m => Console.println(m.toString +" "+ m.entity))
            Console.println ()
          }
        }
      }

      // Sample and learn, providing jump function, temperature, learning rate, #iterations, and diagnostic-printing-function
      Console.println ("About to sampleAndLearn")
      sampler.sampleAndLearn(3000)
    }
    0;
  }
}

