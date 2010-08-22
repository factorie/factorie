/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest 
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue, ArrayBuffer}

// How to think about Proposals and MCMC:
// Variables know their own range of values.  This needs to be coded on a per-variable basis
// Scored preferences about different values are known only by using the model.
// Sometimes we want to sample more than one variable together.  One variable cannot know how to do this on its own.
// Sometimes we want to sample conditioned on other fixed values.  One variable cannot know about this either.  It must be something like a Template
// Sometimes we want sampling to chain: sample v1, then v2 conditioned on the value of v1, etc.
// Making proposals is sometimes keyed by a single variable, a list of variables, or nothing (proposer itself maintains context of what to change next)
// Perhaps proposers should be in a list of Template-like objects; given a variable, first Template in the list to claim it gets to make the change.
// To facilitate ease of use, variable classes could provide something like:
//   class Label[T] { def defaultSampler = LabelSampler; def sample(model:Model) = defaultSampler.sample(this,model) }
//   object LabelSampler extends Sampler1[Label]
    

/** Simple GibbsSampler.
    @author Andrew McCallum */
class GibbsSampler(val model:Model = Global.defaultGenerativeModel) extends Sampler[Variable] {
  var temperature = 1.0
  val handlers = new ArrayBuffer[GibbsSamplerHandler]
  def defaultHandlers = List(GeneratedVariableGibbsSamplerHandler, MixtureChoiceGibbsSamplerHandler, IterableSettingsGibbsSamplerHandler)
  handlers ++= defaultHandlers
  // TODO Consider Either[] type checking instead of generative Sampler[Variable]
  def process1(v:Variable): DiffList = {
    // Get factors, in sorted order of the their classname
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.getClass.getName < f2.getClass.getName)
    var done = false
    val handlerIterator = handlers.iterator
    val d = newDiffList
    while (!done && handlerIterator.hasNext) {
      done = handlerIterator.next.sample(v, factors, this)(d)
    }
    if (!done) throw new Error("GibbsSampler: No sampling method found for "+factors)
    d
  }
}

trait GibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:GibbsSampler)(implicit d:DiffList): Boolean
}

object GeneratedVariableGibbsSamplerHandler extends GibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:GibbsSampler)(implicit d:DiffList): Boolean = {
    factors match {
      case List(factor:GeneratedVarTemplate#Factor) => {
        v match {
          case v:GeneratedVariable => v.sample(d)
        }
        true
      }
      case _ => false
    }
  }
}

object MixtureChoiceGibbsSamplerHandler extends GibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:GibbsSampler)(implicit d:DiffList): Boolean = {
    return false
    v match {
      case mc:MixtureChoiceVariable => {
        // TODO We really should have a more careful check like this
        //if (!(factors.forall(_ match { case f:GeneratedVarTemplate#Factor => f.n1 == v || f.n2 == v; case _ => false }))) return false
        val outcomes: Seq[MixtureOutcome] = mc.outcomes
        val domainSize = mc.domainSize
        val distribution = new Array[Double](domainSize)
        var sum = 0.0
        for (i <- 0 until domainSize) {
          distribution(i) = mc.proportions.pr(i) * outcomes.foldLeft(1.0)((prod:Double, outcome:MixtureOutcome) => prod * outcome.prFromMixtureComponent(i))
          sum += distribution(i)
        }
        mc.set(Maths.nextDiscrete(distribution, sum)(Global.random))(d)
        true
      }
    }
  }
}

object IterableSettingsGibbsSamplerHandler extends GibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:GibbsSampler)(implicit d:DiffList): Boolean = {
    v match {
      case v2: Variable with IterableSettings => {
        // Iterate over all settings of the variable 'v', score each change, and sample from those scores
        val proposals = v2.settings.map(d => {val m = d.scoreAndUndo(sampler.model); new Proposal(d, m, Math.NaN_DOUBLE, m/sampler.temperature)}).toList
        val proposal = proposals.sampleExpProportionally((p:Proposal) => p.acceptanceScore)
        proposal.diff.redo
        if (d ne null) d ++= proposal.diff
        true
      }
      case _ =>  false
    }
  }
}




/** A GibbsSampler that can also collapse some Parameters. */
class CollapsedGibbsSampler(collapsedVariables:Iterable[CollapsedParameter], val model:Model = Global.defaultGenerativeModel) extends Sampler[GeneratedVariable] {
  var temperature = 1.0
  val handlers = new ArrayBuffer[CollapsedGibbsSamplerHandler]
  def defaultHandlers = List(GeneratedVariableCollapsedGibbsSamplerHandler, MixtureChoiceCollapsedGibbsSamplerHandler)
  handlers ++= defaultHandlers

  /*val _collapsed = new HashMap[Variable,Variable] // with VariableMap
  def isCollapsed(v:Variable) = _collapsed.contains(v)
  def collapsed[V<:CollapsibleParameter](v:V): V#CollapsedType = _collapsed.getOrElse(v, null).asInstanceOf[V#CollapsedType]
  def collapse[V<:CollapsibleParameter](v:V): V#CollapsedType = {
    val cv = v.newCollapsed
    assert(! _collapsed.contains(v))
    _collapsed(v) = cv
    v.children.foreach(child => cv.addChild(child)(null)) // Notice that the children will not have 'cv' as a parent, though.
    cv
  }
  def logpr(v:GeneratedVariable): Double = 0.0 // TODO Implement pr given collapsing
  */
  val collapsed = new HashSet[AnyRef]
  //println("CollapsedGibbsSampler.init |C|="+collapsedVariables.size)
  collapsed ++= collapsedVariables
  //collapsibleVars.foreach(collapse(_))
  // Initialization of the collapsed variables
  for (cv <- collapsedVariables) {
    cv.clearChildStats
    for (child <- cv.children) child match {
      case mcs:MixtureComponents[_] => { 
        mcs.childrenOf(cv).foreach(cv.updateChildStats(_, 1.0))
        //println("CollapsedGibbsSampler init MixtureComponents child "+child+" count="+mcs.childrenOf(cv).size)
      }
      case v:GeneratedVar => cv.updateChildStats(v, 1.0)
    }
    //println("CollapsedGibbsSampler init cv: "+cv)
    // TODO This should look at model factors to make sure that we aren't improperly ignoring some other factors/variables
  }

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
}


trait CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean
}

object GeneratedVariableCollapsedGibbsSamplerHandler extends CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean = {
    factors match {
      // TODO We could try to gain some speed by handling specially the case in which there is only one parent
      case List(factor:GeneratedVarTemplate#Factor) => {
        for (parent <- factor._3; if (sampler.collapsed.contains(parent))) parent match {
          // When we are mapping from v to collapsed representation, do instead: sampler.collapsed(p).updateChildStats(v, -1.0)
          case p:CollapsedParameter => p.updateChildStats(v, -1.0)
          // TODO What about collapsed children?
        }
        factor._1 match {
          case gv: GeneratedVariable => gv.sample(d)
        }
        for (parent <- factor._3; if (sampler.collapsed.contains(parent))) parent match {
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
        if (sampler.collapsed.contains(parent)) parent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, -1.0)
          case _ => new Error // TODO Change this to just do nothing?
        }

        if (v.outcomes.size == 1) {
          // MixtureChoice v controls only one MixtureOutcome
          val outcome = v.outcomes.first
          // If parents of outcomes are collapsed, decrement counts
          for (chosenParent <- outcome.chosenParents; if (sampler.collapsed.contains(chosenParent)))
            chosenParent match { case p:CollapsedParameter => { /*println("CollapsedGibbsSampler -1.0 p="+p); */ p.updateChildStats(outcome, -1.0) } }
          forIndex(domainSize)(i => {
            distribution(i) = parent.pr(i) * outcome.prFromMixtureComponent(i)
            sum += distribution(i)
          })
          // Sample
          //println("MixtureChoiceCollapsedGibbsSamplerHandler distribution = "+(distribution.toList.map(_ / sum)))
          v.set(Maths.nextDiscrete(distribution, sum)(Global.random))
          //println("MixtureChoiceCollapsedGibbsSamplerHandler "+v+"@"+v.hashCode+" newValue="+v.intValue)
          // If parent of outcome is collapsed, increment counts
          for (chosenParent <- outcome.chosenParents; if (sampler.collapsed.contains(chosenParent)))
            chosenParent match { case p:CollapsedParameter => p.updateChildStats(outcome, 1.0) }
        } else {
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
          v.set(Maths.nextDiscrete(distribution, sum)(Global.random))
          //println("CollapsedGibbsSampler choice.intValue="+v.intValue)
          // If parents of outcomes are collapsed, decrement counts
          for (outcome <- outcomes; chosenParent <- outcome.chosenParents; if (sampler.collapsed.contains(chosenParent)))
            chosenParent match { case p:CollapsedParameter => p.updateChildStats(outcome, 1.0) }
        }

        // If parent of v is collapsed, increment counts
        if (sampler.collapsed.contains(parent)) parent match {
          case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, 1.0)
          case _ => new Error // TODO Change this to just do nothing?
        }

        true
      }
      case _ => false
    }
  }
}

