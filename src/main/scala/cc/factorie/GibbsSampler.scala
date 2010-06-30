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
      case List(factor:GeneratedValueTemplate#Factor) => {
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
    factors match {
      case List(factor1:GeneratedValueTemplate#Factor, factor2:MixtureChoiceVariableTemplate#Factor) => {
        val mc: MixtureChoiceVariable = factor2.n1
        // TODO Make sure that 'mc' doesn't do any extra factor coordination
        val outcomes: Seq[MixtureOutcome] = mc.gatedRefs.map(_ match { 
          case pr:GatedParameterRef[Parameter,MixtureOutcome] => pr.child
          case _ => throw new Error("MixtureChoice is controlling non-MixtureOutcome") 
        }).toSet.toList // In order to check for duplicates in 'outcomes' // TODO Try to speed this up for the common cases
        val domainSize = mc.domainSize
        val distribution = new Array[Double](domainSize)
        var sum = 0.0
        //mc.gateRefs.foreach(_.setToNull(d))
        for (i <- 0 until domainSize) {
          distribution(i) = mc.proportions.pr(i) * outcomes.foldLeft(1.0)((prod:Double, outcome:MixtureOutcome) => prod * outcome.prFromMixtureComponent(i))
          sum += distribution(i)
        }
        mc.setByIndex(Maths.nextDiscrete(distribution, sum)(Global.random))(d)
        true
      }
      case _ => false
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
class CollapsedGibbsSampler(val model:Model = Global.defaultGenerativeModel, collapsedVariables:Iterable[CollapsedParameter] = Nil) extends Sampler[GeneratedVariable] {
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
    for (child <- cv.children) cv.updateChildStats(child, 1.0)
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
    if (!done) throw new Error("GibbsSampler: No sampling method found for "+factors.map(_.template.getClass.getName).mkString("List(",",",")"))
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
      case List(factor:GeneratedValueTemplate#Factor) => {
        for (parent <- factor.n3; if (sampler.collapsed.contains(parent))) parent match {
          case p:CollapsedParameter => p.updateChildStats(v, -1.0)
          // TODO What about collapsed children?
        }
        factor.n1 match {
          case gv: GeneratedVariable => gv.sample(d)
        }
        for (parent <- factor.n3; if (sampler.collapsed.contains(parent))) parent match {
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
      case v: MixtureChoiceVariable => factors match {
        case List(factor1:GeneratedValueTemplate#Factor, factor2:MixtureChoiceVariableTemplate#Factor) => {
          // This is the case for LDA's "z" variables
          //println("MixtureChoiceCollapsedGibbsSamplerHandler v="+v+" value="+v.intValue)
          val parent = v.proportions
          val domainSize = v.domain.size
          val distribution = new Array[Double](domainSize)
          var sum = 0.0

          // If parent of v is collapsed, decrement counts.  
          if (sampler.collapsed.contains(parent)) parent match {
            case collapsedParent:DirichletMultinomial => collapsedParent.updateChildStats(v, -1.0)
            case _ => new Error // TODO Change this to just do nothing?
          }

          if (v.gatedRefs.size == 1) {
            // MixtureChoice v controls only one GatedRefVariable
            val ref = v.gatedRefs.first.asInstanceOf[GatedParameterRef[Proportions,MixtureOutcome]]
            // If parent of outcome is collapsed, decrement counts
            if (sampler.collapsed.contains(ref.value)) ref.value match { case p:CollapsedParameter => p.updateChildStats(ref.child, -1.0) }
            val outcome: MixtureOutcome = ref.child
            forIndex(domainSize)(i => {
              distribution(i) = parent.pr(i) * outcome.prFromMixtureComponent(i)
              sum += distribution(i)
            })
            // Sample
            //println("MixtureChoiceCollapsedGibbsSamplerHandler distribution = "+(distribution.toList.map(_ / sum)))
            v.setByIndex(Maths.nextDiscrete(distribution, sum)(Global.random))
            //println("MixtureChoiceCollapsedGibbsSamplerHandler "+v+"@"+v.hashCode+" newValue="+v.intValue)
            // If parent of outcome is collapsed, increment counts
            if (sampler.collapsed.contains(ref.value)) ref.value match { case p:CollapsedParameter => p.updateChildStats(ref.child, 1.0) }
          } else {
            // MixtureChoice v controls multiple GatedRefVariables
            val refs = v.gatedRefs.map(_ match { case gpr:GatedParameterRef[Proportions,MixtureOutcome] => gpr })
            // If parents of outcomes are collapsed, decrement counts
            for (ref <- refs; if sampler.collapsed.contains(ref.value)) ref.value match { case p:CollapsedParameter => p.updateChildStats(ref.child, -1.0) }
            val outcomes: Seq[MixtureOutcome] = refs.map(_.child).toSet.toList // TODO Faster way to unique the list?
            for (i <- 0 until domainSize) {
              distribution(i) = parent.pr(i) * outcomes.foldLeft(1.0)((prod,o) => prod * o.prFromMixtureComponent(i))
              sum += distribution(i)
            }
            // Sample
            v.setByIndex(Maths.nextDiscrete(distribution, sum)(Global.random))
            // If parents of outcomes are collapsed, decrement counts
            for (ref <- refs; if sampler.collapsed.contains(ref.value)) ref.value match { case p:CollapsedParameter => p.updateChildStats(ref.child, 1.0) }
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
      case _ => false
    }
  }
}

