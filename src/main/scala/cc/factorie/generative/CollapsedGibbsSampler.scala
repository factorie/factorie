/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.generative
import cc.factorie._
import scala.reflect.Manifest 
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue, ArrayBuffer}


/** A GibbsSampler that can also collapse some Parameters. */
class CollapsedGibbsSampler(collapse:Iterable[CollapsibleParameter], val model:Model = cc.factorie.generative.defaultGenerativeModel) extends Sampler[GeneratedVariable] {
  var debug = false
  var temperature = 1.0 // TODO Currently ignored?
  val handlers = new ArrayBuffer[CollapsedGibbsSamplerHandler]
  def defaultHandlers = List(GeneratedVariableCollapsedGibbsSamplerHandler, MixtureChoiceCollapsedGibbsSamplerHandler)
  handlers ++= defaultHandlers

  private val _c = new HashMap[Parameter,Parameter]
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
  def addChild(child:GeneratedVariable): Unit = child match {
    case mcs:MixtureComponents[_] => {
      for (parent <- mcs.parents; if (isCollapsed(parent))) {
        mcs.childrenOf(parent).foreach(collapsed(parent).updateChildStats(_, 1.0))
        //println("CollapsedGibbsSampler init MixtureComponents child "+child+" count="+mcs.childrenOf(cv).size)
      }
    }
    case mo:MixtureOutcome => {
      for (parent <- mo.chosenParents; if (isCollapsed(parent))) collapsed(parent).updateChildStats(mo, 1.0)
    }
    case gv:GeneratedVar => {
      for (parent <- gv.parents; if (isCollapsed(parent))) collapsed(parent).updateChildStats(gv, 1.0)
    }
  }   


  // Initialize collapsed parameters specified on command line
  collapse.foreach(collapseParameter(_))

  def process1(v:GeneratedVariable): DiffList = {
    assert(!v.isInstanceOf[CollapsedVariable]) // We should never be sampling a CollapsedVariable
    // Get factors, in sorted order of the their classname
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.template.getClass.getName < f2.template.getClass.getName)
    var done = false
    val handlerIterator = handlers.iterator
    val d = newDiffList
    while (!done && handlerIterator.hasNext) {
      done = handlerIterator.next.sample(v, factors, this)(d)
    }
    if (!done) throw new Error("CollapsedGibbsSampler: No sampling method found for variable "+v+" with factors "+factors.map(_.template.getClass.getName).mkString("List(",",",")"))
    d
  }

  /** Set variables' values to the mean of their collapsed representation */
  def export(implicit d:DiffList = null): Unit = {
    throw new Error("Not yet implemented.")
  }

}


trait CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean
}

object GeneratedVariableCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean = {
    factors match {
      // TODO We could try to gain some speed by handling specially the case in which there is only one parent
      case List(factor:GeneratedVarTemplate#Factor) => {
        for (parent <- factor._3) sampler.collapsedOrNull(parent) match {
          // When we are mapping from v to collapsed representation, do instead: sampler.collapsed(p).updateChildStats(v, -1.0)
          case p:CollapsedParameter => p.updateChildStats(v, -1.0)
          // TODO What about collapsed children?
        }
        factor._1 match { // Necessary because it might be a GeneratedVar, not a GeneratedVariable, in which case we should fail
          case gv: GeneratedVariable => gv.sample(d)
        }
        for (parent <- factor._3) sampler.collapsedOrNull(parent) match {
          case p:CollapsedParameter => p.updateChildStats(v, 1.0)
          // TODO What about collapsed children?
        }
        true
      }
      case _ => false
    }
  }
}


object MixtureChoiceCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean = {
    v match {
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
        sampler.collapsedOrNull(parent) match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, -1.0)
          // TODO DirichletMultinomial can be changed to just CollapsedParameter
          case null => {}
          case _ => new Error // TODO Change this to just do nothing?
        }

        if (v.outcomes.size == 1) {
          // MixtureChoice v controls only one MixtureOutcome
          val outcome = v.outcomes.head
          // If parents of outcomes are collapsed, decrement counts
          for (chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => { /*println("CollapsedGibbsSampler -1.0 p="+p); */ p.updateChildStats(outcome, -1.0) }
            case _ => {}
          }
          val cParent: Proportions = sampler.collapsedOrSelf(parent).asInstanceOf[Proportions]
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
            v.set(Maths.nextDiscrete(distribution, sum)(cc.factorie.random))
          //println("MixtureChoiceCollapsedGibbsSamplerHandler "+v+"@"+v.hashCode+" newValue="+v.intValue)
          // If parent of outcome is collapsed, increment counts
          for (chosenParent <- outcome.chosenParents) sampler.collapsedOrNull(chosenParent) match {
            case p:CollapsedParameter => p.updateChildStats(outcome, 1.0)
            case _ => {}
          }
        } else {
          throw new Error
          /*
          // MixtureChoice v controls multiple MixtureOutcomes
          val outcomes = v.outcomes
          // If parents of outcomes are collapsed, decrement counts
          for (outcome <- outcomes; chosenParent <- outcome.chosenParents; if (sampler.collapsed.contains(chosenParent)))
            chosenParent match { case p:CollapsedParameter => p.updateChildStats(outcome, -1.0) }
          for (i <- 0 until domainSize) {
            //println("parent.pr("+i+")="+parent.pr(i))
            //outcomes.foreach(o => println("mc.pr("+i+")="+o.prFromMixtureComponent(i)))
            distribution(i) = parent.pr(i) * outcomes.foldLeft(1.0)((prod,o) => prod * o.prFromMixtureComponent(i))
            sum += distribution(i)
          }
          // Sample
          //println("CollapsedGibbsSampler distribution="+distribution.toList)
          v.set(Maths.nextDiscrete(distribution, sum)(cc.factorie.random))
          //println("CollapsedGibbsSampler choice.intValue="+v.intValue)
          // If parents of outcomes are collapsed, decrement counts
          for (outcome <- outcomes; chosenParent <- outcome.chosenParents; if (sampler.collapsed.contains(chosenParent)))
            chosenParent match { case p:CollapsedParameter => p.updateChildStats(outcome, 1.0) }
          */
        }

        // If parent of v is collapsed, increment counts
        sampler.collapsedOrNull(parent) match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, 1.0)
          case null => {}
          case _ => new Error // TODO Change this to just do nothing?
        }

        true
      }
      case _ => false
    }
  }
}
