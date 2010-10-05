/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.example

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie._

/** A simple coreference engine on toy data.  
    Demonstrates the use of RefVariable and SetVariable for representing mentions and entities. */
object CorefMentionsDemo {

  // The data

  /** A random variable for a mention of an entity */
  class Mention(val name:String, val trueEntity:Int, initialEntity:Entity) extends RefVariable(initialEntity) {
    initialEntity.add(this)(null)
    // When this mention is assigned to an entity, update the mention
    override def set(e:Entity)(implicit d:DiffList) : Unit = {
      if (e != entity) {
        if (entity != null) entity.remove(this)
        e.add(this)
        super.set(e)
      }
    }
    def entity = value // an alias just for readability
    override def toString = "Mention(" + name +"=="+ entity.canonical +")"
  }
  
  /** A random variable for an entity, which is merely a set of Mentions */
  class Entity(val canonical:String) extends SetVariable[Mention] {
    def mentions = members // an alias just for readability
    override def toString = "Entity("+canonical+":"+mentions.toSeq.size+")"
  }

  /** A feature vector variable measuring affinity between two mentions */
  class AffinityVector(s1:String, s2:String) extends BinaryFeatureVectorVariable[String] {
    import AffinityDomain._
    type VariableType = AffinityVector
    if (s1 equals s2) this += streq else this += nstreq
    if (s1.substring(0,1) equals s2.substring(0,1)) this += prefix1 else this += nprefix1
    if (s1.substring(0,2) equals s2.substring(0,2)) this += prefix2 else this += nprefix2
    if (s1.substring(0,3) equals s2.substring(0,3)) this += prefix3 else this += nprefix3
    if (s1.contains(s2) || s2.contains(s1)) this += substring else this += nsubstring
    if (s1.length == s2.length) this += lengtheq
    s1.split(" ").foreach(s => if (s2.contains(s)) this += containsword)
    s2.split(" ").foreach(s => if (s1.contains(s)) this += containsword)
    // Also consider caching mechanisms
  }
  object AffinityDomain extends StringDomain[AffinityVector] {
    val streq, nstreq, prefix1, nprefix1, prefix2, nprefix2, prefix3, nprefix3, substring, nsubstring, lengtheq, containsword = Value
    freeze
  }
  Domain += AffinityDomain


  def main(args: Array[String]) : Unit = {
      var mentionList = new ArrayBuffer[Mention]();
      var entityList = new ArrayBuffer[Entity]();

      val data = List(
        List("Andrew McCallum", "Andrew MacCallum", "Angrew McCallum", "McCallum", "A. McCallum"),
        List("Michael Wick", "Mike Wick", "Michael Andrew Wick", "Wick", "Wick"),
        List("Khashayar Rohanemanesh", "Khash R.", "Kesh Rohanemanesh"),
        List("Aron Culotta", "Andrew Culotta", "A. Culotta", "Culotta McCallum", "Culotta", "Culotta"),
        List("Charles Sutton", "Charles A. Sutton", "Sutton", "Sutton"),
        List("Nicola Cancceda", "Nicola Canceda", "Nicolla Cancceda", "Nicol Cancheta", "Canceda", "Cancceda")
      )
      
      // Create variables for the data
      var eIndex = 0
      def entityIndex = { eIndex += 1; eIndex }
      var mentions = data.zipWithIndex.map {case (names, trueEntityIndex) => {
        // Initialize by putting each mention into its own separate entity
        names.map(s => {
          val newEntity = new Entity("e" + entityIndex);
          val newMention = new Mention(s, trueEntityIndex, newEntity);
          entityList += newEntity;
          mentionList += newMention;
          newMention;
        })
      }}
      
      val model = new Model
  
      // Pairwise affinity factor between Mentions in the same partition
      model += new Template2[Mention,Mention] with DotStatistics1[AffinityVector] {
        def unroll1 (mention:Mention) = for (other <- mention.entity.mentions; if (other != mention)) yield 
          if (mention.hashCode > other.hashCode) Factor(mention, other)
          else Factor(other, mention)
        def unroll2 (mention:Mention) = Nil // symmetric
        def statistics (mention1:Mention, mention2:Mention) = Stat(new AffinityVector(mention1.name, mention2.name))
      }

      // Pairwise repulsion factor between Mentions in different partitions
      model += new Template2[Mention,Mention] with DotStatistics1[AffinityVector] {
        override def factors(d:Diff) = d.variable match {
          case mention : Mention => d match {
            case mention.RefDiff(oldEntity:Entity, newEntity:Entity) => 
              for (other <- oldEntity.mentions; if (other.entity != mention.entity)) yield Factor(mention, other);
            case _ => super.factors(d)
          }
          case _ => super.factors(d)
        }
        def unroll1 (mention:Mention) = for (other <- mentionList; if (other.entity != mention.entity)) yield Factor(mention, other);
        def unroll2 (mention:Mention) = Nil // symmetric
        def statistics(mention1:Mention, mention2:Mention) = Stat(new AffinityVector(mention1.name, mention2.name))
      }
  
      // Factor testing if all the mentions in this entity share the same prefix of length 1.  A first-order-logic feature!
      model += new Template1[Entity] with DotStatistics1[BooleanVar] {
        def statistics(entity:Entity) = {
          if (entity.mentions.isEmpty) Stat(BooleanObservation(true))
          else {
            val prefix1 = entity.mentions.iterator.next.name.substring(0,1)
            if (entity.mentions.forall(m => prefix1 equals m.name.substring(0,1)))
              Stat(true)
            else
              Stat(false)
          }
        }
      }


      val objective1 = new Model(new TemplateWithStatistics1[Mention] {
        def score(s:Stat) = {
          val thisMention = s._1
          mentionList.foldLeft(0.0)((total,m) => 
          if (m.trueEntity == thisMention.trueEntity) {
            if (m.entity == thisMention.entity) total + 1
            else total - 1
          } else {
            if (m.entity == thisMention.entity) total - 1
            else total + 1
          })
        }
      })

      var sampler = new MHSampler[Null](model) 
  with SampleRank 
  with ConfidenceWeightedUpdates
  //with MIRAUpdates
  //with PerceptronUpdates
 {
  temperature = 0.001
        override val objective = objective1
        def propose(context:Null)(implicit difflist:DiffList) : Double = {
          // Pick a random mention
          val m = mentionList.sampleUniformly(cc.factorie.random)
          //println("CorefMentions MHPerceptronLearner mention="+m)
          // Pick a random place to move it, either an existing Entity or a newly created one
          var e: Entity = null
          // Pick an existing entity to move it to
          if (m.entity.size == 1 || random.nextDouble < 0.8) {
            val s2 = entityList.filter((e: Entity) => e.size > 0 && e != m.entity)
            if (s2.size != 0) e = s2(random.nextInt(s2.size))
          }
          // Pick an empty entity to move it to (create one if it doesn't exist)
          if (e == null) {
            val s2 = entityList.filter((e: Entity) => e.size == 0)
            if (s2.size != 0) e = s2(random.nextInt(s2.size))
            else {e = new Entity("e"+entityIndex); entityList += e}
          }
      	  // Move it
          //            Console.println ("Proposal.jump moving "+m+" to "+e)
          m.set(e)(difflist)
          // log-Q-ratio shows that forward and backward jumps are equally likely
          return 0.0
        }
        override def postProcessHook(context:Null, difflist:DiffList) : Unit = {
          super.postProcessHook(context, difflist)
          if (processCount % 500 == 0) {
            //learningRate *= .9
            // TODO put back numUpdates   System.out.println("UPS: " + numUpdates);
            // model.templatesOf[DotTemplate].foreach(f => println (f.toString+" weights = "+f.weights.toList)) // TODO Commented out when DenseVectors.toList stopped working, most likely due to FACTORIE using 2.8.0.RC1 and Scalala using 2.8.0.Beta1
            println ("All entities")
            entityList.filter(e=>e.size>0).foreach(e => println(e.toString +" "+ e.mentions.toList))
            //Console.println ("All mentions"); mentionList.foreach(m => Console.println(m.toString +" "+ m.entity))
            println
          }
        }
      }

      // Sample and learn, providing jump function, temperature, learning rate, #iterations, and diagnostic-printing-function
      Console.println ("Beginning inference and learning")
      sampler.process(3000)
    }
    0;

}

