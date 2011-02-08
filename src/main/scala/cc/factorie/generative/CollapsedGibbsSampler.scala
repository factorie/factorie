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
  def defaultHandlers = Seq(GeneratedVarCollapsedGibbsSamplerHandler, MixtureChoiceCollapsedGibbsSamplerHandler)
  handlers ++= defaultHandlers
  val cacheClosures = true
  def closures = new HashMap[Variable, CollapsedGibbsSamplerClosure]

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
    /*p match {
      mcs:CollapsibleFiniteMixture[_] => 
        forIndex(mcs.size)(i => _c(mcs(i)) = cp.asInstanceOf[CollapsedFiniteMixture[_]].apply(i))
      case _ => {}
    }*/
    /*
    for (child <- p.children) child match {
      case mcs:MixtureComponents[_] => { 
        mcs.childrenOf(p).foreach(cp.updateChildStats(_, 1.0))
        //println("CollapsedGibbsSampler init MixtureComponents child "+child+" count="+mcs.childrenOf(p).size)
      }
      case v:GeneratedVar => cp.updateChildStats(v, 1.0)
    }*/
  }
  // To make sure that we don't account more than once for the sufficient statistics of a child-of-collapsed-parameter.
  //private val _children = new HashSet[Variable] // TODO Consider using this, but this table could get very big :-(
  /*def addChild(child:MutableGeneratedVar): Unit = child match {
    case mcs:MixtureComponents[_] => {
      for (parent <- mcs.parents; if (isCollapsed(parent))) {
        mcs.childrenOf(parent).foreach(collapsed(parent).updateChildStats(_, 1.0))
        //println("CollapsedGibbsSampler init MixtureComponents child "+child+" count="+mcs.childrenOf(cv).size)
      }
    }
    case mo:MixtureGeneratedVar => {
      for (parent <- mo.chosenParents; if (isCollapsed(parent))) collapsed(parent).updateChildStats(mo, 1.0)
    }
    case gv:GeneratedVar => {
      for (parent <- gv.parents; if (isCollapsed(parent))) collapsed(parent).updateChildStats(gv, 1.0)
    }
  }*/
  


  // Initialize collapsed parameters specified in constructor
  collapse.foreach(collapseParameter(_))

  def process1(v:Iterable[MutableGeneratedVar]): DiffList = {
    assert(!v.exists(_.isInstanceOf[CollapsedVar])) // We should never be sampling a CollapsedVariable
    val d = newDiffList
    // If we have a cached closure, just use it and return
    if (cacheClosures && v.size == 1 && closures.contains(v.head)) { closures(v.head).sample(d); return d }

    // Get factors, in sorted order of the their classname
    val factors: Seq[Factor] = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.template.getClass.getName < f2.template.getClass.getName)
    var done = false
    val handlerIterator = handlers.iterator
    while (!done && handlerIterator.hasNext) {
      val closure = handlerIterator.next.sampler(v, factors, this)
      if (closure ne null) {
        done = true
        closure.sample(d)
        if (cacheClosures && v.size == 1) closures(v.head) = closure
      }
    }
    if (!done) throw new Error("CollapsedGibbsSampler: No sampling method found for variable "+v+" with factors "+factors.map(_.template.getClass.getName).mkString("List(",",",")"))
    d
  }

  /** Set variables' values to the mean of their collapsed representation */
  def export(implicit d:DiffList = null): Unit = {
    collapsedMap.foreach({case(p:CollapsibleParameter,cp:CollapsedParameter) => p.setFrom(cp)})
    //throw new Error("Not yet implemented.")
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
    val gv = v.head.asInstanceOf[MutableGeneratedVar]
    if (factors.size == 1 && classOf[GenerativeTemplate#Factor].isAssignableFrom(factors.head.getClass)) {
      val factor = factors.head
      new Closure(gv, factor.copy(sampler.collapsedMap).asInstanceOf[GenerativeTemplate#Factor], factor.variables.filter(v => classOf[CollapsedParameter].isAssignableFrom(v.getClass)).asInstanceOf[Seq[CollapsedParameter]])
    } else null
  }
  class Closure(val variable:MutableGeneratedVar, val factor:GenerativeTemplate#Factor, val collapsedParents:Seq[CollapsedParameter]) extends CollapsedGibbsSamplerClosure {
    def sample(implicit d:DiffList = null): Unit = {
      for (p <- collapsedParents) p.updateChildStats(variable, -1.0)
      //throw new Error("Change code to sample from subsituted (collapsed) parents")
      variable.set(factor.template.sampledValue(factor.statistics).asInstanceOf[variable.Value])
      //variable.sampleFromParents()
      for (p <- collapsedParents) p.updateChildStats(variable, 1.0) 
      // TODO Consider whether we should be passing values rather than variables to updateChildStats
      // TODO What about collapsed children?
    }
  }
}



// TODO This the "one outcome" case for now.
object MixtureChoiceCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    v.head match {
      case v: MixtureChoiceVar => {
        require(v.outcomes.size == 1) // TODO write code to handle more outcomes.
        new Closure(v, v.outcomes.head, factors(1).copy(sampler.collapsedMap).asInstanceOf[DiscreteTemplate#Factor], factors(0).copy(sampler.collapsedMap).asInstanceOf[MixtureGenerativeTemplate#Factor])
      }
      case _ => null
    }
  }
    
  class Closure(val choice:MixtureChoiceVar, val outcome:MixtureGeneratedVar, 
                val cFactor:DiscreteTemplate#Factor, val oFactor:MixtureGenerativeTemplate#Factor) 
  extends CollapsedGibbsSamplerClosure
  {
    def sample(implicit d:DiffList = null): Unit = {
      val choiceParent = cFactor._2
      // Remove sufficient statistics from collapsed dependencies
      choiceParent match { case cp:CollapsedParameter => cp.updateChildStats(choice, -1.0); case _ => {} }
      for (op <- oFactor.variables.drop(1)) op match { case cp:CollapsedParameter => cp.updateChildStats(outcome, -1.0); case _ => {} }
      // Calculate distribution of new value
      val oStat = oFactor.statistics
      val domainSize = choice.domain.size
      val distribution = new Array[Double](domainSize)
      var sum = 0.0
      forIndex(domainSize)(i => {
        val d1 = choiceParent(i)
        val d2 = oFactor.template.prChoosing(oStat, i)
        //println("MixtureChoiceCollapsedGibbsSamplerHandler "+choice+" "+i+" "+d1+" "+d2)
        distribution(i) = d1 * d2
        sum += distribution(i)
      })
      assert(sum == sum, "Distribution sum is NaN")
      assert(sum != Double.PositiveInfinity, "Distrubtion sum is infinity.")
      // Sample
      if (false) println("MixtureChoiceCollapsedGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
      // sum can be zero for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
      if (sum == 0) choice.set(cc.factorie.random.nextInt(domainSize))
      else choice.set(maths.nextDiscrete(distribution, sum)(cc.factorie.random))
      // Put back sufficient statitics of collapsed dependencies
      choiceParent match { case cp:CollapsedParameter => cp.updateChildStats(choice, 1.0); case _ => {} }
      for (op <- oFactor.variables.drop(1)) op match { case cp:CollapsedParameter => cp.updateChildStats(outcome, 1.0); case _ => {} }
    }
  }
}


