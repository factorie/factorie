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

package cc.factorie.generative
import cc.factorie._
import scala.collection.mutable.{HashMap, HashSet, ArrayBuffer}

/** A GibbsSampler that can also collapse some Parameters. */
class CollapsedGibbsSampler(collapse:Iterable[CollapsibleParameter], val model:Model = cc.factorie.generative.defaultGenerativeModel) extends Sampler[Iterable[MutableGeneratedVar]] {
  var debug = false
  makeNewDiffList = false // override default in cc.factorie.Sampler
  var temperature = 1.0 // TODO Currently ignored?
  val handlers = new ArrayBuffer[CollapsedGibbsSamplerHandler]
  def defaultHandlers = Seq(
      GeneratedVarCollapsedGibbsSamplerHandler, 
      MixtureChoiceCollapsedGibbsSamplerHandler, 
      PlatedMixtureChoiceCollapsedDirichletGibbsSamplerHandler)
  handlers ++= defaultHandlers
  val cacheClosures = true
  val closures = new HashMap[Variable, CollapsedGibbsSamplerClosure]

  private val _c = new HashMap[Parameter,Parameter] with cc.factorie.util.Substitutions {
    def sub[A](x:A): A = x match {
      case p:Parameter => getOrElse(p, p).asInstanceOf[A]
      case _ => x
    }
  }
  def collapsedMap = _c
  def collapsed[V<:CollapsibleParameter](v:V): V#CollapsedType = _c(v).asInstanceOf[V#CollapsedType]
  //def collapsed(p:Parameter) = _c(p).asInstanceOf[CollapsedParameter] // generic version of the above
  def collapsedOrNull(p:Parameter): Parameter = _c.getOrElse(p, null)
  def collapsedOrSelf(p:Parameter): Parameter = _c.getOrElse(p, p)
  def isCollapsed(v:Parameter) = _c.contains(v)
  // TODO Consider renaming collapseOrRegisterParameter ?
  def collapseParameter[V<:CollapsibleParameter](p:V): Unit = {
    //println("CollapsedGibbsSampler collapseParameter "+p)
    // If already collapsed, just clearChildStats, otherwise create a newCollapsed
    val cp = p.newCollapsed
    _c(p) = cp
  }

  // Initialize collapsed parameters specified in constructor
  collapse.foreach(collapseParameter(_))

  def process1(v:Iterable[MutableGeneratedVar]): DiffList = {
    //assert(!v.exists(_.isInstanceOf[CollapsedVar])) // We should never be sampling a CollapsedVariable
    val d = newDiffList
    // If we have a cached closure, just use it and return
    if (cacheClosures && v.size == 1 && closures.contains(v.head)) { 
      closures(v.head).sample(d)
    } else {
      // Get factors, in sorted order of the their classname
      val factors: Seq[Factor] = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.factorName < f2.factorName)
      //println("CollapsedGibbsSampler.process1 factors = "+factors.map(_.template.getClass).mkString)
      var done = false
      val handlerIterator = handlers.iterator
      while (!done && handlerIterator.hasNext) {
        val closure = handlerIterator.next.sampler(v, factors, this)
        if (closure ne null) {
          done = true
          closure.sample(d)
          if (cacheClosures && v.size == 1) {
            closures(v.head) = closure
          }
        }
      }
      if (!done) throw new Error("CollapsedGibbsSampler: No sampling method found for variable "+v+" with factors "+factors.map(_.factorName).mkString("List(",",",")"))
    }
    d
  }

  /** Set variables' values to the mean of their collapsed representation */
  def export(implicit d:DiffList = null): Unit = {
    collapsedMap.foreach({case(p:CollapsibleParameter,cp:CollapsedParameter) => p.setFrom(cp)})
  }

  /** Convenience for sampling single variable */
  def process(v:MutableGeneratedVar): DiffList = process(Seq(v))

}


trait CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure
}

trait CollapsedGibbsSamplerClosure {
  def sample(implicit d:DiffList = null): Unit
}



object GeneratedVarCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    if (factors.size == 1 && classOf[GenerativeTemplate#Factor].isAssignableFrom(factors.head.getClass)) {
      val factor = factors.head
      val gv = v.head.asInstanceOf[MutableGeneratedVar]
      new Closure(gv, factor.copy(sampler.collapsedMap).asInstanceOf[GenerativeTemplate#Factor], 
                  factor.variables.filter(v => classOf[CollapsedParameter].isAssignableFrom(v.getClass)).asInstanceOf[Seq[CollapsedParameter]])
    } else null
  }
  class Closure(val variable:MutableGeneratedVar, val factor:GenerativeTemplate#Factor, val collapsedParents:Seq[CollapsedParameter]) extends CollapsedGibbsSamplerClosure {
    def sample(implicit d:DiffList = null): Unit = {
      for (p <- collapsedParents) p.updateChildStats(variable, -1.0)
      variable.set(factor.template.sampledValue(factor.statistics).asInstanceOf[variable.Value])
      for (p <- collapsedParents) p.updateChildStats(variable, 1.0) 
      // TODO Consider whether we should be passing values rather than variables to updateChildStats
      // TODO What about collapsed children?
    }
  }
}



// TODO This the "one outcome" and "one outcome parent" case for now.
object MixtureChoiceCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    v.head match {
      case v: MixtureChoiceVar => {
        require(v.outcomes.size == 1) // TODO write code to handle more outcomes.
        val choiceFactor = factors(1).copy(sampler.collapsedMap).asInstanceOf[DiscreteTemplate#Factor]
        val outcomeFactor = factors(0).copy(sampler.collapsedMap).asInstanceOf[MixtureGenerativeTemplate#Factor]
        val choiceParent = choiceFactor._2
        require(outcomeFactor.numVariables == 3)
        require(outcomeFactor.variable(1).isInstanceOf[Parameter])
        require(outcomeFactor.variable(2).isInstanceOf[MixtureChoiceVar])
        val outcomeParent = outcomeFactor.variable(1)
        new Closure(v, v.outcomes.head, choiceFactor, outcomeFactor,
                    choiceParent match { case cp:CollapsedParameter => cp case _ => null.asInstanceOf[CollapsedParameter] },
                    outcomeParent match { case cp:CollapsedParameter => cp case _ => null.asInstanceOf[CollapsedParameter] })
      }
      case _ => null
    }
  }
    
  class Closure(val choice:MixtureChoiceVar, val outcome:MixtureGeneratedVar, 
                val cFactor:DiscreteTemplate#Factor, val oFactor:MixtureGenerativeTemplate#Factor,
                val collapsedChoiceParent: CollapsedParameter, val collapsedOutcomeParent:CollapsedParameter)
  extends CollapsedGibbsSamplerClosure
  {
    def sample(implicit d:DiffList = null): Unit = {
      val choiceParent = cFactor._2
      // Remove sufficient statistics from collapsed dependencies
      if (collapsedChoiceParent ne null) collapsedChoiceParent.updateChildStats(choice, -1.0)
      if (collapsedOutcomeParent ne null) collapsedOutcomeParent.updateChildStats(outcome, -1.0)
      // Calculate distribution of new value
      val oStat = oFactor.statistics
      val domainSize = choice.domain.size
      val distribution = new Array[Double](domainSize)
      var sum = 0.0
      forIndex(domainSize)(i => {
        distribution(i) = choiceParent(i) * oFactor.template.prChoosing(oStat, i)
        sum += distribution(i)
      })
      assert(sum == sum, "Distribution sum is NaN")
      assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
      // Sample
      //println("MixtureChoiceCollapsedGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
      // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
      if (sum == 0) choice.set(cc.factorie.random.nextInt(domainSize))
      else choice.set(cc.factorie.maths.nextDiscrete(distribution, sum)(cc.factorie.random))
      // Put back sufficient statistics of collapsed dependencies
      if (collapsedChoiceParent ne null) collapsedChoiceParent.updateChildStats(choice, 1.0)
      if (collapsedOutcomeParent ne null) collapsedOutcomeParent.updateChildStats(outcome, 1.0)
    }
  }
}

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
        if (collapsedChoiceParent ne null) collapsedChoiceParent.increment(choiceIntValue, -1.0)
        if (collapsedOutcomeParent ne null) collapsedOutcomeParent(choiceIntValue).increment(outcome.intValue(seqIndex), -1.0)
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
        if (collapsedChoiceParent ne null) collapsedChoiceParent.increment(choiceIntValue, 1.0)
        if (collapsedOutcomeParent ne null) collapsedOutcomeParent(choiceIntValue).increment(outcome.intValue(seqIndex), 1.0)
      }
    )}
  }
}

