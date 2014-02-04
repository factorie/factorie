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



package cc.factorie.tutorial

import cc.factorie._
import cc.factorie.optimize._
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import cc.factorie.variable._
import cc.factorie.model._
import cc.factorie.infer.MHSampler

/** A simple coreference engine on toy data.  
    Demonstrates the use of RefVariable and SetVariable for representing mentions and entities. */
object CorefMentions {

  /** random variable that defines the entity each mention refers to */
  class EntityRef(val mention:Mention, initialEntity:Entity) extends RefVariable(initialEntity) {
    initialEntity.add(mention)(null)
    // When this mention is assigned to an entity, update the mention
    override def set(e:Entity)(implicit d:DiffList) : Unit = {
      if (e != value) {
        if (value != null) value.remove(mention)
        e.add(mention)
        super.set(e)
      }
    }
  }

  abstract class TrueEntityIndex(i:Int) extends IntegerVariable(i) {
    def mention: Mention
  }

  /** A observed properties of a mention */
  class Mention(val name:String, trueEntity:Int, initialEntity:Entity) extends StringVariable(name) {
    val trueEntityIndex = new TrueEntityIndex(trueEntity) {
      def mention = Mention.this
    }
    val entityRef = new EntityRef(this, initialEntity)
    override def toString = "Mention(" + name +"=="+ entityRef.value.canonical +")"
  }
  
  /** A random variable for an entity, which is merely a set of Mentions */
  class Entity(val canonical:String) extends SetVariable[Mention] {
    def mentions = members // an alias just for readability
    override def toString = "Entity("+canonical+":"+mentions.toSeq.size+")"
  }

  /** A feature vector variable measuring affinity between two mentions */
  class AffinityVector(s1:String, s2:String) extends BinaryFeatureVectorVariable[String] {
    import AffinityDomain._
    def domain = AffinityVectorDomain
    if (s1 equals s2) value += streq else value += nstreq
    if (s1.substring(0,1) equals s2.substring(0,1)) value.update(prefix1, 1) else value.update(nprefix1, 1)
    if (s1.substring(0,2) equals s2.substring(0,2)) value.update(prefix2, 1) else value(nprefix2) = 1
    if (s1.substring(0,3) equals s2.substring(0,3)) value(prefix3) = 1 else value(nprefix3) = 1
    if (s1.contains(s2) || s2.contains(s1)) value(substring) = 1 else value(nsubstring) = 1
    if (s1.length == s2.length) value(lengtheq) = 1
    s1.split(" ").foreach(s => if (s2.contains(s)) value(containsword) = 1)
    s2.split(" ").foreach(s => if (s1.contains(s)) value(containsword) = 1)
    // Also consider caching mechanisms
  }
  object AffinityDomain extends EnumDomain {
    val streq, nstreq, prefix1, nprefix1, prefix2, nprefix2, prefix3, nprefix3, substring, nsubstring, lengtheq, containsword = Value
  }
  object AffinityVectorDomain extends CategoricalVectorDomain[String] {
    override lazy val dimensionDomain = AffinityDomain
  }


  def main(args: Array[String]) : Unit = {
      var mentionList = new ArrayBuffer[Mention]()
    var entityList = new ArrayBuffer[Entity]()

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
          val newEntity = new Entity("e" + entityIndex)
          val newMention = new Mention(s, trueEntityIndex, newEntity)
          entityList += newEntity
          mentionList += newMention
          newMention
        })
      }}

      val model = new TemplateModel with Parameters

      // Pairwise affinity factor between Mentions in the same partition
      model += new DotTemplate4[EntityRef,EntityRef,Mention,Mention] {
        //def statisticsDomains = Tuple1(AffinityVectorDomain)
        val weights = model.Weights(new la.DenseTensor1(AffinityVectorDomain.dimensionSize))
        def unroll1 (er:EntityRef) = for (other <- er.value.mentions; if other.entityRef.value == er.value) yield
          if (er.mention.hashCode > other.hashCode) Factor(er, other.entityRef, er.mention, other.entityRef.mention)
          else Factor(er, other.entityRef, other.entityRef.mention, er.mention)
        def unroll2 (er:EntityRef) = Nil // symmetric
        def unroll3 (mention:Mention) = throw new Error
        def unroll4 (mention:Mention) = throw new Error
        def statistics (e1:EntityRef#Value, e2:EntityRef#Value, m1:Mention#Value, m2:Mention#Value) = new AffinityVector(m1, m2).value
      }

      // Pairwise repulsion factor between Mentions in different partitions
      model += new DotTemplate4[EntityRef,EntityRef,Mention,Mention] {
        //def statisticsDomains = Tuple1(AffinityVectorDomain)
        val weights = model.Weights(new la.DenseTensor1(AffinityVectorDomain.dimensionSize))
        /*override def factors(d:Diff) = d.variable match {
          case mention: Mention => d match {
            case mention.entityRef.RefVariableDiff(oldEntity:Entity, newEntity:Entity) => 
              for (other <- oldEntity.mentions; if (other.entityRef.value != mention.entityRef.value)) yield Factor(mention, other);
            case _ => super.factors(d)
          }
          case _ => super.factors(d)
        }*/
        def unroll1 (er:EntityRef) = for (other <- er.value.mentions; if other.entityRef.value != er.value) yield
          if (er.mention.hashCode > other.hashCode) Factor(er, other.entityRef, er.mention, other.entityRef.mention)
          else Factor(er, other.entityRef, other.entityRef.mention, er.mention)
        def unroll2 (er:EntityRef) = Nil // symmetric
        def unroll3 (mention:Mention) = throw new Error
        def unroll4 (mention:Mention) = throw new Error
        override def statistics (e1:EntityRef#Value, e2:EntityRef#Value, m1:Mention#Value, m2:Mention#Value) = new AffinityVector(m1, m2).value
      }

      // Factor testing if all the mentions in this entity share the same prefix of length 1.  A first-order-logic feature.
      model += new DotTemplate1[Entity] {
        //def statisticsDomains = Tuple1(BooleanDomain)
        val weights = model.Weights(new la.DenseTensor1(BooleanDomain.size))
        override def statistics(e:Entity#Value) = {
          val mentions: Entity#Value= e
          if (mentions.isEmpty) BooleanDomain.trueValue
          else {
            val prefix1 = mentions.iterator.next().name.substring(0,1)
            if (mentions.forall(m => prefix1 equals m.name.substring(0,1)))
              BooleanDomain.trueValue
            else
              BooleanDomain.falseValue
          }
        }
      }


      val objective1 = new CombinedModel(new TupleTemplateWithStatistics2[EntityRef,TrueEntityIndex] {
        def unroll1(er:EntityRef) = Factor(er, er.mention.trueEntityIndex)
        def unroll2(tei:TrueEntityIndex) = Factor(tei.mention.entityRef, tei)
        def score(thisMentionEntity:EntityRef#Value, thisMentionTrueEntityIndex:TrueEntityIndex#Value) = {
          mentionList.foldLeft(0.0)((total,m) => 
          if (m.trueEntityIndex.value == thisMentionTrueEntityIndex) {
            if (m.entityRef.value == thisMentionEntity) total + 1
            else total - 1
          } else {
            if (m.entityRef.value == thisMentionEntity) total - 1
            else total + 1
          })
        }
      })

      val sampler = new MHSampler[Null](model)(new scala.util.Random(0)) {
        temperature = 0.001
        override val objective = objective1
        def propose(context:Null)(implicit difflist:DiffList): Double = {
          // Pick a random mention
          val m = mentionList.sampleUniformly(random)
          //println("CorefMentions MHSampler mention="+m)
          // Pick a random place to move it, either an existing Entity or a newly created one
          var e: Entity = null
          // Pick an existing entity to move it to
          if (m.entityRef.value.size == 1 || random.nextDouble < 0.8) {
            val s2 = entityList.filter((e: Entity) => e.size > 0 && e != m.entityRef.value)
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
          m.entityRef.set(e)(difflist)
          // log-Q-ratio shows that forward and backward jumps are equally likely
          0.0
        }
        override def postProposalHook(difflist:DiffList) : Unit = {
          super.postProposalHook(difflist)
          //println("CorefMentions postProcessHook")
          if (proposalsCount % 500 == 0) {
            //learningRate *= .9
            // TODO put back numUpdates   System.out.println("UPS: " + numUpdates);

            // model.templatesOf[DotTemplate].foreach(f => println (f.toString+" weightsSet = "+f.weightsSet.toList)) // TODO Commented out when DenseVectors.toList stopped working, most likely due to FACTORIE using 2.8.0.RC1 and Scalala using 2.8.0.Beta1
            //println ("All entities")
            // entityList.filter(e=>e.size>0).foreach(e => println(e.toString +" "+ e.mentions.toList))
            //Console.println ("All mentions"); mentionList.foreach(m => Console.println(m.toString +" "+ m.entityRef.value))
            // println
          }
        }
      }
      val learner = new SampleRankTrainer(sampler, new MIRA)

      // Sample and learn, providing jump function, temperature, learning rate, #iterations, and diagnostic-printing-function
      // Console.println ("Beginning inference and learning")
      learner.processContext(null, 3000) // 3000
    }

}

