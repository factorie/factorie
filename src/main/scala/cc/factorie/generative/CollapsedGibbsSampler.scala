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
  def defaultHandlers = Seq(GeneratedVarCollapsedGibbsSamplerHandler /*, MixtureChoiceCollapsedGibbsSamplerHandler*/ )
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
  def collapsed[V<:CollapsibleParameter](v:V) = _c(v).asInstanceOf[V#CollapsedType]
  def collapsed(p:Parameter) = _c(p).asInstanceOf[CollapsedParameter] // generic version of the above
  def collapsedOrNull(p:Parameter): Parameter = _c.getOrElse(p, null)
  def collapsedOrSelf(p:Parameter): Parameter = _c.getOrElse(p, p)
  def isCollapsed(v:Parameter) = _c.contains(v)
  // TODO Consider renaming collapseOrRegisterParameter ?
  def collapseParameter[V<:CollapsibleParameter](p:V): Unit = {
    //println("CollapsedGibbsSampler collapseParameter "+p)
    // If already collapsed, just clearChildStats, otherwise create a newCollapsed
    val cp = p match { case cp:CollapsedParameter => { cp.clearChildStats; cp }; case _ => p.newCollapsed }
    _c(p) = cp
    //cp.clearChildStats // Is this necessary?  I don't think so because it is newly created.  If not newly created, we do it above in assignment of cp
    for (child <- p.children) child match {
      case mcs:MixtureComponents[_] => { 
        mcs.childrenOf(p).foreach(cp.updateChildStats(_, 1.0))
        //println("CollapsedGibbsSampler init MixtureComponents child "+child+" count="+mcs.childrenOf(p).size)
      }
      case v:GeneratedVar => cp.updateChildStats(v, 1.0)
    }
  }
  // To make sure that we don't account more than once for the sufficient statistics of a child-of-collapsed-parameter.
  //private val _children = new HashSet[Variable] // TODO Consider using this, but this table could get very big :-(
  def addChild(child:MutableGeneratedVar): Unit = child match {
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
  }   


  // Initialize collapsed parameters specified on command line
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
    collapsedMap.foreach({case(p:CollapsibleParameter,cp:CollapsedParameter) => p.setFromCollapsed(cp.asInstanceOf[p.CollapsedType])})
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
  class Closure(val variable:MutableGeneratedVar, val factor:GenerativeFactor, val collapsedParents:Seq[CollapsedParameter]) extends CollapsedGibbsSamplerClosure {
    def sample(implicit d:DiffList = null): Unit = {
      for (p <- collapsedParents) p.updateChildStats(variable, -1.0)
      variable.sampleFromParents()
      for (p <- collapsedParents) p.updateChildStats(variable, 1.0) 
      // TODO Consider whether we should be passing values rather than variables to updateChildStats
      // TODO What about collapsed children?
    }
  }
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    val gv = v.head.asInstanceOf[MutableGeneratedVar]
    if (factors.size == 1 && classOf[GenerativeTemplate#Factor].isAssignableFrom(factors.head.getClass)) {
      val factor = factors.head.asInstanceOf[GenerativeFactor]
      val cFactor = factor.copy(sampler.collapsedMap)
      new Closure(gv, cFactor, cFactor.variables.filter(v => classOf[CollapsedParameter].isAssignableFrom(v.getClass)).asInstanceOf[Seq[CollapsedParameter]])
    } else null
  }
}


/*
object MixtureChoiceCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  // This the "one outcome" case for now.
  class Closure(val variable:MixtureChoiceVar, val outcome:MixtureGeneratedVar, 
                val vFactor:DiscreteTemplate#Factor, val oFactor:MixtureGenerativeTemplate#Factor) 
  extends CollapsedGibbsSamplerClosure
  {
    var vParents: Seq[CollapsedParameter] = vFactor.variables.filterByType[CollapsedParameter].toSeq
    var oParents: Seq[CollapsedParameter] = oFactor.variables.filterByType[CollapsedParameter].toSeq
    def sample(implicit d:DiffList = null): Unit = {
      val mi = variable.intValue
      val parent = variable.proportions
      val vParents = parent match { case p:CollapsedParameter => p; case _ => null }
      val ocParents = for (f <- oFactors; v <- f.variables)
      // Remove sufficient statistics from collapsed dependencies
      for (p <- vParents) p.updateChildStats(variable, -1.0)
      for (p <- oParents) p.updateChildStats(outcome, -1.0)
      // Calculate distribution of new value
      val oStat = oFactor.statistics
      val domainSize = v.domain.size
      val distribution = new Array[Double](domainSize)
      var sum = 0.0
      val proportions: Proportions = vFactor._2
      forIndex(domainSize)(i => {
        distribution(i) = proportions(i) * oTemplate.prChoosing(oStat, i)
        sum += distribution(i)
      })
      // Sample
      if (sampler.debug) println("MixtureChoiceCollapsedGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
      if (sum == 0)
        // This can happen for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
        v.set(cc.factorie.random.nextInt(domainSize))
      else
        v.set(maths.nextDiscrete(distribution, sum)(cc.factorie.random))
      // Put back sufficient statitics of collapsed dependencies
      for (p <- vParents) p.updateChildStats(variable, 1.0)
      for (p <- oParents) p.updateChildStats(outcome, 1.0) // TODO Write this for MixtureComponents!!!  
      // TODO CollapsedMixtureComponents contains collapsed versions of all the components
      // and its updateChildStats looks at the corresponding version of the outcome.choice to find the right Parameters!
    }
  }
  def sampler(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler): CollapsedGibbsSamplerClosure = {
    if (v.size != 1) return null
    v.head match {
      case v: MixtureChoiceVar => {

    val gv = v.head.asInstanceOf[MutableGeneratedVar]
    factors match {
      // TODO We could try to gain some speed by handling specially the case in which there is only one parent
      case Seq(factor:GenerativeTemplate#Factor) => {
        val cFactor = factor.copy(sampler.collapsedMap)
        new Closure(gv, cFactor, cFactor.variables.filter(classOf[CollapsedParameter].isAssignableFrom(_)))
      }
      case _ => null
    }
  }


  def sample(v:Iterable[Variable], factors:Seq[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean = {
    if (v.size > 1) return false
    v.head match {
      case v: MixtureChoiceVariable => {
        // TODO We really should have a more careful check like this...
        //if (!(factors.forall(_ match { case f:GeneratedVarTemplate#Factor => f.n1 == v || f.n2 == v; case _ => false }))) return false
        // ...because this MixtureChoiceVariable might possibly be connected to some other variables other than its parents and outcomes.

        // This is the case for LDA's "z" variables
        //println("MixtureChoiceCollapsedGibbsSamplerHandler v="+v+" value="+v.intValue)
        val parent = v.proportions
        val domainSize = v.domain.size
        val distribution = new Array[Double](domainSize)
        var sum = 0.0

        // TODO Add a check to make sure that if a variable is a "Collapsed" one, it does appear in sampler.collapsed?
        // Try to catch those cases in which the user forgets to put a collapsed variable in the arguments to the CollapsedGibbsSampler constructor?

        // If parent of v is collapsed, decrement counts.  
        val cParent: Proportions = sampler.collapsedOrSelf(parent).asInstanceOf[Proportions]
        cParent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, -1.0)
          // TODO DirichletMultinomial can be changed to just CollapsedParameter
          //case null => {}
          case _ => {} // was: throw new Error
        }

        if (v.outcomes.size == 1) {
          // MixtureChoice v controls only one MixtureOutcome
          val outcome = v.outcomes.head
          // If parents of outcomes are collapsed, decrement counts
          for (chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => { p.updateChildStats(outcome, -1.0) }
            case _ => {}
          }
          forIndex(domainSize)(i => {
            distribution(i) = cParent.pr(i) * outcome.prFromMixtureComponent(sampler.collapsedMap, i)
            //distribution(i) = cParent.pr(i) * outcome.prFrom(outcome.parentsFromMixtureComponent(i).map(sampler.collapsedOrSelf(_)))
            //distribution(i) = cParent.pr(i) * outcome.prFromMixtureComponent(i)
            sum += distribution(i)
          })
          // Sample
          if (sampler.debug) println("MixtureChoiceCollapsedGibbsSamplerHandler outcome="+outcome+" sum="+sum+" distribution="+(distribution.mkString(",")))
          if (sum == 0)
            // This can happen for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
            v.set(cc.factorie.random.nextInt(domainSize))
          else
            v.set(maths.nextDiscrete(distribution, sum)(cc.factorie.random))
          //println("MixtureChoiceCollapsedGibbsSamplerHandler "+v+"@"+v.hashCode+" newValue="+v.intValue)
          // If parent of outcome is collapsed, increment counts
          for (chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => p.updateChildStats(outcome, 1.0)
            case _ => {}
          }
        } else {
          // MixtureChoice v controls multiple MixtureOutcomes
          val outcomes = v.outcomes
          // If parents of outcomes are collapsed, decrement counts
          for (outcome <- outcomes; chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => { p.updateChildStats(outcome, -1.0) }
            case _ => {}
          }
          forIndex(domainSize)(i => {
            distribution(i) = cParent.pr(i) * outcomes.foldLeft(1.0)((prod,o) => prod * o.prFromMixtureComponent(sampler.collapsedMap, i))
            sum += distribution(i)
          })
          // Sample
          //if (sampler.debug) println("MixtureChoiceCollapsedGibbsSamplerHandler outcomes="+outcomes+" sum="+sum+" distribution="+(distribution.mkString(",")))
          if (sum == 0)
            // This can happen for a new word in the domain and a non-collapsed growable Proportions has not yet placed non-zero mass there
            v.set(cc.factorie.random.nextInt(domainSize))
          else
            v.set(maths.nextDiscrete(distribution, sum)(cc.factorie.random))
          //println("MixtureChoiceCollapsedGibbsSamplerHandler "+v+"@"+v.hashCode+" newValue="+v.intValue)
          //println("CollapsedGibbsSampler distribution="+distribution.toList)
          //println("CollapsedGibbsSampler choice.intValue="+v.intValue)
          // If parents of outcomes are collapsed, decrement counts
          for (outcome <- outcomes; chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => { p.updateChildStats(outcome, 1.0); }
            case _ => {}
          }
        }

        // If parent of v is collapsed, increment counts
        cParent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, 1.0)
          //case null => {}
          case _ => {} // new Error // TODO Change this to just do nothing?
        }

        true
      }
      case _ => false
    }
  }
    }
*/

