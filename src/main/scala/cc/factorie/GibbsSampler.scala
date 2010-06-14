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
    

/** GibbsSampler for a subclass of Variable with IterableSettings.
    @author Andrew McCallum */
/*class GibbsSampler[V<:Variable with IterableSettings](model:Model = Global.defaultModel, objective:Model = null) extends SamplerOverSettings[V](model, objective) {
  def settings(v:V): SettingIterator = v.settings
}*/

/** Simple GibbsSampler.
    @author Andrew McCallum */
class GibbsSampler(val model:Model = Global.defaultModel, val objective:Model = null) extends Sampler[Variable] {
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
        // TODO Consider doing factor.v1.generativeSource.set(null) and then resetting??
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
      case List(factor1:GeneratedValueTemplate#Factor, factor2:MixtureChoiceTemplate#Factor) => {
        val mc: MixtureChoice = factor2.n1
        // TODO Make sure that 'mc' doesn't do any extra factor coordination
        val outcomes: Seq[MixtureOutcome] = mc.gatedRefs.map(_ match { 
          case pr:GatedParameterRef[Parameter,MixtureOutcome] => pr.child
          case _ => throw new Error("MixtureChoice is controlling non-MixtureOutcome") 
        }).toSet.toList // In order to check for duplicates in 'outcomes' // TODO Try to speed this up for the common cases
        val domainSize = mc.domain.size
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
        val proposals = v2.settings.map(d => {val (m,o) = d.scoreAndUndo(sampler.model, sampler.objective); new Proposal(d, m, o, m/sampler.temperature)}).toList
        val proposal = proposals.sampleExpProportionally((p:Proposal) => p.acceptanceScore)
        proposal.diff.redo
        if (d ne null) d ++= proposal.diff
        true
      }
      case _ =>  false
    }
  }
}




/** A GibbsSampler that can also collapse some GenverativeDistribution variables. */
/*
class CollapsedGibbsSampler(collapsibleVars:Iterable[CollapsibleVariable]) extends Sampler[GeneratedVariable[_]] {
  val _collapsed = new HashMap[Variable,Variable] // with VariableMap
  def isCollapsed(v:Variable) = _collapsed.contains(v)
  def collapsed[V<:CollapsibleVariable](v:V): V#CollapsedType = _collapsed.getOrElse(v, null).asInstanceOf[V#CollapsedType]

  //def collapsedDistribution[V<:CollapsibleDistribution[O],O](v:V): V#CollapsedType = _collapsed.getOrElse(v, null).asInstanceOf[V#CollapsedType]
  def map[V<:Variable](v:V): V = _collapsed.getOrElse(v, v).asInstanceOf[V]
  def collapse[V<:CollapsibleVariable](v:CollapsibleVariable): V#CollapsedType = {
    val cv = v.newCollapsed
    assert(! _collapsed.contains(v))
    _collapsed(v) = cv
    v match { case gd: Distribution[_] => gd.generatedSamples.foreach(cv.addSample(_)) }
  }

  // Initialize
  collapsibleVars.foreach(collapse(_))

  def logpr(v:GeneratedVariable): Double = {
    // pr given collapsing
    0.0
  }

  def process1(v:GeneratedVariable[_]): DiffList = {
    assert(!v.isInstanceOf[CollapsedVariable]) // We should never be sampling a CollapsedVariable
    val factors = model.factors(v).sortWith((f1:Factor,f2:Factor) => f1.getClass.getName < f2.getClass.getName)
    val d = newDiffList
    // TODO Consider here looking for any collapsed variables among the neighbors, and the unrolling them also.
    factors match {
      // v's only factor is the one with its generative source, which may or may not be collapsed
      case List(factor:GeneratedVariableTemplate#Factor) => {
        val src = v.generativeSource.value
        if (isCollapsed(src)) {
          // src has been collapsed, notify it before and after the sampling change
          val collapsedSrc = collapsed(src.asInstanceOf[CollapsibleDistribution[v.type]])
          assert(src != collapsedSrc)
          collapsedSrc.removeSample(v)(d)
          v.sampleFrom(collapsedSrc)(d) // Note that v.generativeSource is still the original uncollapsed distribution
          collapsedSrc.addSample(v)(d)
        } else {
          // src has not been collapsed, just sample normally.
          v.sampleFrom(src)(d)
        }
        d
      }
      // v is a MixtureChoice
      case List(factor1:GeneratedVariableTemplate#Factor, factor2:MixtureChoiceTemplate#Factor) => {
        val choice: MixtureChoice[MixtureChoice] = factor2.v1
        val choiceSrc = mc.generativeSource.value
        val domainSize = mc.domain.size
        val proportions = new Array[Double](domainSize)
        var sum = 0.0
        if (isCollapsed(choiceSrc)) {
          val collapsedChoiceSrc = collapsed(choiceSrc.asInstanceOf[DiscreteDistribution[choice.type] with CollapsibleDistribution[choice.type]])
          // Remove variables' sufficient statistics from collapsed distributions
          collapsedChoiceSrc.removeSample(choice)(d)
          for (ref <- choice.gateRefs; if (isCollapsed(ref.value))) {
            val collapsedMixtureComponent = collapsed(ref.value.asInstanceOf[CollapsibleDistribution[ref.outcome.type]])
            collapsedMixtureComponent.removeSample(choice.outcome)(d)
          }
          // Build the distribution from which we will sample
          for (i <- 0 until domainSize) {
            proportions(i) = collapsedChoiceSrc.pr(i)
            for (ref <- choice.gateRefs) {
              if (isCollapsed(ref.value)) proportions(i) *= collapsed(ref.value(i).asInstanceOf[CollapsibleDistribution[ref.outcome.type]]).pr(ref.outcome)
              else proportions(i) *= ref.outcomePr(i)
            }
            sum += proportions(i)
          }
          // Sample
          val newValue = Maths.nextDiscrete(proportions, sum)
          // Set the new value
          choice.setByIndex(newValue)(d)
          // Add variable' sufficient statistics to collapsed distributions
          collapsedChoiceSrc.addSample(choice)(d)
          for (ref <- choice.gateRefs; if (isCollapsed(ref.value))) {
            val collapsedMixtureComponent = collapsed(ref.value.asInstanceOf[CollapsibleDistribution[ref.outcome.type]])
            collapsedMixtureComponent.addSample(choice.outcome)(d)
          }
        } else {
          // choiceSrc is not collapsed, but the mixture components may be
          // Remove variables' sufficient statistics from collapsed distributions
          for (ref <- choice.gateRefs; if (isCollapsed(ref.value))) {
            val collapsedMixtureComponent = collapsed(ref.value.asInstanceOf[CollapsibleDistribution[ref.outcome.type]])
            collapsedMixtureComponent.removeSample(choice.outcome)(d)
          }
          // Build the distribution from which we will sample
          for (i <- 0 until domainSize) {
            proportions(i) = choiceSrc.pr(i)
            for (ref <- choice.gateRefs) {
              if (isCollapsed(ref.value)) proportions(i) *= collapsed(ref.value(i).asInstanceOf[CollapsibleDistribution[ref.outcome.type]]).pr(ref.outcome)
              else proportions(i) *= ref.outcomePr(i)
            }
            sum += proportions(i)
          }
          // Sample
          val newValue = Maths.nextDiscrete(proportions, sum)
          // Set the new value
          choice.setByIndex(newValue)(d)
          // Add variable' sufficient statistics to collapsed distributions
          for (ref <- choice.gateRefs; if (isCollapsed(ref.value))) {
            val collapsedMixtureComponent = collapsed(ref.value.asInstanceOf[CollapsibleDistribution[ref.outcome.type]])
            collapsedMixtureComponent.addSample(choice.outcome)(d)
          }
        }
      }
      case _: => throw new Error("CollapsedGibbsSampler: No sampling method found for "+factors)
    }
    // Return the DiffList
    d
  }

}

trait CollapsedGibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean
}

object DiscreteWithCollapsedParentGibbsSamplerHandler extends GibbsSamplerHandler {
  def sample(v:Variable, factors:List[Factor], sampler:CollapsedGibbsSampler)(implicit d:DiffList): Boolean = {
    v match {
      v: Discrete => {
        factors match {
          case List(factor:GeneratedVariableTemplate#Factor) => {
            val parent = v.proportions
            if (sampler.isCollapsed(parent)) {
              // parent has been collapsed, notify it before and after the sampling change
              val collapsedParent = collapsed(parent.asInstanceOf[CollapsibleProportions])
          assert(src != collapsedSrc)
          collapsedSrc.removeSample(v)(d)
          v.sampleFrom(collapsedSrc)(d) // Note that v.generativeSource is still the original uncollapsed distribution
          collapsedSrc.addSample(v)(d)
            } else {
            }
        val parents = factor.v1.parents
        val src = v.generativeSource.value
        if (isCollapsed(src)) {
        } else {
          // src has not been collapsed, just sample normally.
          v.sampleFrom(src)(d)
        }
        d
      }

      case List(factor:GeneratedVariableTemplate#Factor) => {
        // TODO Consider doing factor.v1.generativeSource.set(null) and then resetting
        factor.v1.sample(d)
        true
      }
      case _ => false
    }
  }
}
*/




/** Defines a substitution mapping from variable to variable.  
    Used in approximate inference, where collapsed versions of variables take the place of the originals. */
/*
trait VariableMap extends Map[Variable,Variable] {
  // If the argument is in the map, return its mapped value; otherwise return the argument itself. 
  override def apply[V<:Variable](in:V): V = getOrElse(in,in).asInstanceOf[V]
}
*/
