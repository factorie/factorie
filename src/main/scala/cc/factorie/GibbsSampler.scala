package cc.factorie
import scala.reflect.Manifest 
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue}

import cc.factorie.util.Implicits._

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
    

/** Tries each one of the settings in the Iterator provided by the abstract method "settings(V1), 
 * scores each, builds a distribution from the scores, and samples from it. */
abstract class GibbsSamplerOverSettings1[V1<:Variable](val model:Model)(implicit m1:Manifest[V1]) extends Sampler1[V1](m1) {
  var temperature = 1.0
  var useQueue = false
  var maxQueueSize = 500
  lazy val queue = new PriorityQueue[Factor]
	def settings(v:V1) : Iterator[{def set(d:DiffList):Unit}];
  def sample(variable:V1) : DiffList = {
    val queueDiff : Seq[Diff] = if (useQueue && Global.random.nextDouble < 0.3 && queue.size > 1)
      sampleFromQueue else Nil
		val vsettings = settings(variable) 
		variable match {
		  // For cases in which changing this variable value does not change any other variables' values
		  case v:NoVariableCoordination => {
		  	case class Proposal(setting:{def set(d:DiffList):Unit}, score:Double)
		  	val proposals = vsettings.map(s => {s.set(null); new Proposal(s, temperature * model.score(variable))}).toList		  
		  	val proposal = proposals.sampleExpProportionally(_.score)
		  	val d = new DiffList
		  	proposal.setting.set(d)
		  	if (useQueue && !queueDiff.isEmpty) d prependAll queueDiff
		  	d
		  }
		  // For cases in which changing this variable value might change some other variables' values, 
		  // but all the changes can be properly evaluating using only the factors that touch this variable
		  case v:NoFactorCoordination => {
		  	case class Proposal(diff:DiffList, score:Double)
		  	val proposals = vsettings.map(s => {val d = new DiffList; s.set(d); val p = new Proposal(d, temperature * model.score(variable)); d.undo; p}).toList
		  	val proposal = proposals.sampleExpProportionally(_.score)
		  	proposal.diff.redo
		  	if (useQueue && !queueDiff.isEmpty) proposal.diff prependAll queueDiff
		  	proposal.diff
		  }
		  // For cases in which there are no guarantees about variable factor- and change-locality
		  case _ => {
		  	case class Proposal(diff:DiffList, score:Double)
		  	val proposals = vsettings.map(s => {val d = new DiffList; s.set(d); new Proposal(d, temperature * d.scoreAndUndo(model))}).toList
		  	//proposals.foreach(p => println("GibbsSampler1 proposal score="+p.score)); println
		  	val proposal = proposals.sampleExpProportionally(_.score)
		  	proposal.diff.redo
		  	if (useQueue && !queueDiff.isEmpty) proposal.diff prependAll queueDiff
		  	proposal.diff
		  }
    }
  }
  def sampleFromQueue : DiffList = {
    if (queue.size < 1) return null
    val factor = queue.dequeue
    val variables = factor.variables.filter(v => m1.erasure.isAssignableFrom(v.getClass)).toSeq
    sample(variables(Global.random.nextInt(variables.size)).asInstanceOf[V1])
  }
}


class GibbsSampler1[V1<:Variable with IterableSettings](model:Model)(implicit m1:Manifest[V1]) extends GibbsSamplerOverSettings1[V1](model)(m1) {
  def this()(implicit m1:Manifest[V1]) = this(Global.defaultModel)(m1)
	def settings(v:V1) = v.settings
}
