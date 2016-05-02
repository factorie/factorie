/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.directed

import cc.factorie.infer._
import cc.factorie.model.Factor
import cc.factorie.variable._

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

/** A GibbsSampler that can also collapse some Parameters. */
class CollapsedGibbsSampler(collapse:Iterable[Var], val model:DirectedModel)(implicit val random: scala.util.Random) extends Sampler[Iterable[MutableVar]] {
  var debug = false
  makeNewDiffList = false // override default in cc.factorie.Sampler
  var temperature = 1.0 // TODO Currently ignored?
  val handlers = new ArrayBuffer[CollapsedGibbsSamplerHandler]
  def defaultHandlers = Seq(
      PlatedGateDiscreteCollapsedGibbsSamplerHandler,
      PlatedGateGategoricalCollapsedGibbsSamplerHandler,
      GateCollapsedGibbsSamplerHandler, 
      //PlatedMixtureChoiceCollapsedDirichletGibbsSamplerHandler,
      GeneratedVarCollapsedGibbsSamplerHandler
      )
  handlers ++= defaultHandlers
  val cacheClosures = true
  val closures = new HashMap[Var, CollapsedGibbsSamplerClosure]
  private val collapsed = new HashSet[Var] ++ collapse

  // Initialize collapsed parameters specified in constructor
  val collapser = new Collapse(model)
  collapse.foreach(v => collapser(Seq(v)))
  // TODO We should provide an interface that handlers can use to query whether or not a particular variable was collapsed or not?

  def isCollapsed(v:Var): Boolean = collapsed.contains(v)
  
  def process1(v:Iterable[MutableVar]): DiffList = {
    //assert(!v.exists(_.isInstanceOf[CollapsedVar])) // We should never be sampling a CollapsedVariable
    val d = newDiffList
    // If we have a cached closure, just use it and return
    if (cacheClosures && v.size == 1 && closures.contains(v.head)) { 
      closures(v.head).sample(d)
    } else {
      // Get factors, no guarantees about their order
      val factors: Iterable[Factor] = model.factors(v)
      //println("CollapsedGibbsSampler.process1 factors = "+factors.map(_.template.getClass).mkString)
      var done = false
      val handlerIterator = handlers.iterator
      while (!done && handlerIterator.hasNext) {
        val closure = handlerIterator.next().sampler(v, factors, this)
        if (closure ne null) {
          done = true
          closure.sample(d)
          if (cacheClosures && v.size == 1) {
            closures(v.head) = closure
          }
        }
      }
      if (!done) throw new Error("CollapsedGibbsSampler: No sampling method found for variable "+v+" with factors "+factors.map(_.factorName).toList.mkString)
    }
    d
  }

  /** Convenience for sampling single variable */
  def process(v:MutableVar): DiffList = process(Seq(v))

}


trait CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Var], factors:Iterable[Factor], sampler:CollapsedGibbsSampler)(implicit random: scala.util.Random): CollapsedGibbsSamplerClosure
}

trait CollapsedGibbsSamplerClosure {
  def sample(implicit d:DiffList = null): Unit
}



object GeneratedVarCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Var], factors:Iterable[Factor], sampler:CollapsedGibbsSampler)(implicit random: scala.util.Random): CollapsedGibbsSamplerClosure = {
    if (v.size != 1 || factors.size != 1) return null
    val pFactor = factors.collectFirst({case f:DirectedFactor => f}) // TODO Yipes!  Clean up these tests!
    if (pFactor == None) return null
    // Make sure all parents are collapsed?
    //if (!pFactor.get.variables.drop(1).asInstanceOf[Seq[Parameter]].forall(v => sampler.collapsedMap.contains(v))) return null
    new Closure(pFactor.get)
  }
  class Closure(val factor:DirectedFactor)(implicit random: scala.util.Random) extends CollapsedGibbsSamplerClosure {
    def sample(implicit d:DiffList = null): Unit = {
      factor.updateCollapsedParents(-1.0)
      val variable = factor.child.asInstanceOf[MutableVar]
      variable.set(factor.sampledValue.asInstanceOf[variable.Value])
      factor.updateCollapsedParents(1.0)
      // TODO Consider whether we should be passing values rather than variables to updateChildStats
      // TODO What about collapsed children?
    }
  }
}



// TODO This the "one outcome" and "one outcome parent" case for now.
object GateCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Var], factors:Iterable[Factor], sampler:CollapsedGibbsSampler)(implicit random: scala.util.Random): CollapsedGibbsSamplerClosure = {
    if (v.size != 1 || factors.size != 2) return null
    //println("GateCollapsedGibbsSamplerHander: "+factors.map(_.asInstanceOf[Family#Factor].family.getClass).mkString)
    //val gFactor = factors.collectFirst({case f:Discrete.Factor if (f.family == Discrete) => f}) // TODO Should be any DiscreteGeneratingFamily#Factor => f
    val gFactor = factors.collectFirst({case f:DiscreteGeneratingFactor => f}) // TODO Should be any DiscreteGeneratingFamily#Factor => f
    val mFactor = factors.collectFirst({case f:MixtureFactor => f})
    if (gFactor == None || mFactor == None) {
      //println("GateCollapsedGibbsSamplerHander: "+gFactor+" "+mFactor)
      return null
    }
    //println("GateCollapsedGibbsSamplerHandler gFactor "+gFactor.get.family.getClass)
    //println("GateCollapsedGibbsSamplerHandler mFactor "+mFactor.get.family.getClass)
    //println("GateCollapsedGibbsSamplerHandler factors equal "+(mFactor.get == gFactor.get))
    new Closure(gFactor.get, sampler.isCollapsed(gFactor.get.parents.head), mFactor.get, sampler.isCollapsed(mFactor.get.parents.head))
  }
    
  class Closure(val gFactor:DiscreteGeneratingFactor, val gCollapsed:Boolean, val mFactor:MixtureFactor, val mCollapsed:Boolean)(implicit random: scala.util.Random) extends CollapsedGibbsSamplerClosure
  {
    def sample(implicit d:DiffList = null): Unit = {
      val gate = mFactor.gate //family.child(gFactor)
      //val gateParent = gFactor._2
      // Remove sufficient statistics from collapsed dependencies
      if (gCollapsed) gFactor.updateCollapsedParents(-1.0)
      if (mCollapsed) mFactor.updateCollapsedParents(-1.0)
      // Calculate distribution of new value
      val mStat = mFactor.currentStatistics // TODO Are these two still necessary?
      val gStat = gFactor.currentStatistics
      val domainSize = gate.domain.size
      val distribution = new Array[Double](domainSize)
      var sum = 0.0
      //println("GateCollapsedGibbsSamplerHandler gFactor "+gFactor.family.getClass)
      //println("GateCollapsedGibbsSamplerHandler mFactor "+mFactor.family.getClass)
      for (i <- 0 until domainSize) {
        //throw new Error
        distribution(i) = /*gStat.prValue(i) * */ 
          gFactor.prValue(i) // * mFactor.prChoosing(i) // TODO Re-implement these methods so that they don't allocate new Statistics objects with each call
        throw new Error("Not yet implemented")
        sum += distribution(i)
      }
      assert(sum == sum, "Distribution sum is NaN")
      assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
      // Sample
      //println("MixtureChoiceCollapsedGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
      // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
      if (sum == 0) gate.set(random.nextInt(domainSize))(null)
      else gate.set(cc.factorie.maths.nextDiscrete(distribution, sum)(random))(null)
      // Put back sufficient statistics of collapsed dependencies
      if (gCollapsed) gFactor.updateCollapsedParents(1.0)
      if (mCollapsed) mFactor.updateCollapsedParents(1.0)
    }
  }
}

object PlatedGateDiscreteCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Var], factors:Iterable[Factor], sampler:CollapsedGibbsSampler)(implicit random: scala.util.Random): CollapsedGibbsSamplerClosure = {
    if (v.size != 1 || factors.size != 2) return null
    val gFactor = factors.collectFirst({case f:PlatedDiscrete.Factor => f}) // TODO Should be any DiscreteGeneratingFamily#Factor => f
    val mFactor = factors.collectFirst({case f:PlatedDiscreteMixture.Factor => f})
    if (gFactor == None || mFactor == None) return null
    assert(gFactor.get._1 == mFactor.get._3)
    new Closure(sampler, gFactor.get, mFactor.get)
  }
    
  class Closure(val sampler:CollapsedGibbsSampler, val gFactor:PlatedDiscrete.Factor, val mFactor:PlatedDiscreteMixture.Factor)(implicit random: scala.util.Random) extends CollapsedGibbsSamplerClosure
  {
    def sample(implicit d:DiffList = null): Unit = {
      val gates = mFactor._3.asInstanceOf[DiscreteSeqVariable]
      val domainSize = gates(0).dim1 // domain.size
      val distribution = new Array[Double](domainSize)
      val gParent = gFactor._2.asInstanceOf[ProportionsVariable]
      val gParentCollapsed = sampler.isCollapsed(gParent)
      val mixture = mFactor._2.asInstanceOf[Mixture[ProportionsVariable]]
      val mixtureCollapsed = sampler.isCollapsed(mixture)
      for (index <- 0 until gates.length) {
        val outcomeIntValue = mFactor._1(index).intValue
        // Remove sufficient statistics from collapsed dependencies
        var z: Int = gates(index).intValue
        if (gParentCollapsed) gParent.incrementMasses(z, -1.0)
        if (mixtureCollapsed) mixture(z).incrementMasses(outcomeIntValue, -1.0)
        // Calculate distribution of new value
        //val mStat = mFactor.statistics
        //val gStat = gFactor.statistics
        var sum = 0.0
        java.util.Arrays.fill(distribution, 0.0)
        var i = 0
        while (i < domainSize) {
          distribution(i) = gParent.value(i) * mixture(i).value(outcomeIntValue)
          sum += distribution(i)
          i += 1
        }
        assert(sum == sum, "Distribution sum is NaN")
        assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
        // Sample
        // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
        if (sum == 0) z = random.nextInt(domainSize)
        else z = cc.factorie.maths.nextDiscrete(distribution, sum)(random)
        gates.set(index, z)(null)
        // Put back sufficient statistics of collapsed dependencies
        if (gParentCollapsed) gParent.incrementMasses(z, 1.0)
        if (mixtureCollapsed) mixture(z).incrementMasses(outcomeIntValue, 1.0)
      }
    }
  }
}



object PlatedGateGategoricalCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Var], factors:Iterable[Factor], sampler:CollapsedGibbsSampler)(implicit random: scala.util.Random): CollapsedGibbsSamplerClosure = {
    if (v.size != 1 || factors.size != 2) return null
    val gFactor = factors.collectFirst({case f:PlatedDiscrete.Factor => f}) // TODO Should be any DiscreteGeneratingFamily#Factor => f
    val mFactor = factors.collectFirst({case f:PlatedCategoricalMixture.Factor => f})
    if (gFactor == None || mFactor == None) return null
    assert(gFactor.get._1 == mFactor.get._3)
    new Closure(sampler, gFactor.get, mFactor.get)
  }

  class Closure(val sampler:CollapsedGibbsSampler, val gFactor:PlatedDiscrete.Factor, val mFactor:PlatedCategoricalMixture.Factor)(implicit random: scala.util.Random) extends CollapsedGibbsSamplerClosure
  {
    def sample(implicit d:DiffList = null): Unit = {
      val gates = mFactor._3.asInstanceOf[DiscreteSeqVariable]
      val domainSize = gates(0).dim1 // domain.size
      val distribution = new Array[Double](domainSize)
      val gParent = gFactor._2.asInstanceOf[ProportionsVariable]
      val gParentCollapsed = sampler.isCollapsed(gParent)
      val mixture = mFactor._2.asInstanceOf[Mixture[ProportionsVariable]]
      val mixtureCollapsed = sampler.isCollapsed(mixture)
      for (index <- 0 until gates.length) {
        val outcomeIntValue = mFactor._1(index).intValue
        // Remove sufficient statistics from collapsed dependencies
        var z: Int = gates(index).intValue
        if (gParentCollapsed) gParent.incrementMasses(z, -1.0)
        if (mixtureCollapsed) mixture(z).incrementMasses(outcomeIntValue, -1.0)
        // Calculate distribution of new value
        //val mStat = mFactor.statistics
        //val gStat = gFactor.statistics
        var sum = 0.0
        java.util.Arrays.fill(distribution, 0.0)
        var i = 0
        while (i < domainSize) {
          distribution(i) = gParent.value(i) * mixture(i).value(outcomeIntValue)
          sum += distribution(i)
          i += 1
        }
        assert(sum == sum, "Distribution sum is NaN")
        assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
        // Sample
        // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
        if (sum == 0) z = random.nextInt(domainSize)
        else z = cc.factorie.maths.nextDiscrete(distribution, sum)(random)
        gates.set(index, z)(null)
        // Put back sufficient statistics of collapsed dependencies
        if (gParentCollapsed) gParent.incrementMasses(z, 1.0)
        if (mixtureCollapsed) mixture(z).incrementMasses(outcomeIntValue, 1.0)
      }
    }
  }
}

/*
object PlatedMixtureChoiceCollapsedDirichletGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    v.head match {
      case v: PlatedMixtureChoiceVar => {
        require(v.outcomes.size == 1) // TODO write code to handle more outcomes.
        if (! v.outcomes.head.isInstanceOf[PlatedDiscreteMixtureVar]) return null
        require(factors.size == 2, "factors size = "+factors.size)
        //println(factors(0)); println(factors(1))
        val choiceFactor = factors(1).copy(sampler.collapsedMap).asInstanceOf[PlatedDiscreteTemplate#Factor]
        val outcomeFactor = factors(0).copy(sampler.collapsedMap).asInstanceOf[PlatedDiscreteMixtureTemplate#Factor]
        if (! outcomeFactor._2.isInstanceOf[CollapsedFiniteMixture[DirichletMultinomial]]) return null
        val choiceParent = choiceFactor._2
        require(outcomeFactor.numVariables == 3)
        require(outcomeFactor.variable(1).isInstanceOf[Parameter])
        require(outcomeFactor.variable(2).isInstanceOf[PlatedMixtureChoiceVar])
        val outcomeParent = outcomeFactor.variable(1)
        new Closure(v, v.outcomes.head.asInstanceOf[PlatedDiscreteMixtureVar],
                    choiceParent match { case cp:DirichletMultinomial => cp case _ => null.asInstanceOf[DirichletMultinomial] },
                    outcomeParent match { case cp:CollapsedFiniteMixture[DirichletMultinomial] => cp case _ => null.asInstanceOf[CollapsedFiniteMixture[DirichletMultinomial]] })
      }
      case _ => null
    }
  }
  class Closure(val choice:PlatedMixtureChoiceVar, val outcome:PlatedDiscreteMixtureVar, 
                val collapsedChoiceParent: DirichletMultinomial, val collapsedOutcomeParent:CollapsedFiniteMixture[DirichletMultinomial])
  extends CollapsedGibbsSamplerClosure
  {
    assert(collapsedChoiceParent ne null)
    assert(collapsedOutcomeParent ne null)
    def sample(implicit d:DiffList = null): Unit = {
      val choiceParent = collapsedChoiceParent
      // Calculate distribution of new value
      val domainSize = choice.domain.elementDomain.size
      val seqSize = choice.length
      val distribution = new Array[Double](domainSize)
      forIndex(seqSize)(seqIndex => {
        // Remove sufficient statistics from collapsed dependencies
        var choiceIntValue = choice.intValue(seqIndex)
        if (collapsedChoiceParent ne null) collapsedChoiceParent.incrementMasses(choiceIntValue, -1.0)
        if (collapsedOutcomeParent ne null) collapsedOutcomeParent(choiceIntValue).incrementMasses(outcome.intValue(seqIndex), -1.0)
        var sum = 0.0
        forIndex(domainSize)(i => {
          distribution(i) = collapsedChoiceParent(i) * collapsedOutcomeParent(i).pr(outcome.intValue(seqIndex))
          sum += distribution(i)
        })
        assert(sum == sum, "Distribution sum is NaN")
        assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
        // println("MixtureChoiceCollapsedDirichletGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
        // Sample
        // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
        if (sum == 0) choiceIntValue = cc.factorie.random.nextInt(domainSize)
        else choiceIntValue = cc.factorie.maths.nextDiscrete(distribution, sum)(cc.factorie.random)
        choice.update(seqIndex, choiceIntValue)
        // Put back sufficient statitics of collapsed dependencies
        if (collapsedChoiceParent ne null) collapsedChoiceParent.incrementMasses(choiceIntValue, 1.0)
        if (collapsedOutcomeParent ne null) collapsedOutcomeParent(choiceIntValue).incrementMasses(outcome.intValue(seqIndex), 1.0)
      }
    )}
  }
}
*/

