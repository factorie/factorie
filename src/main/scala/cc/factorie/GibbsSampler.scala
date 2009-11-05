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
    


/** GibbsSampler for a subclass of Variable with IterableSettings */
// TODO implement Block2 sampling with nested iterators over settings
class GibbsSampler1[V1<:Variable with IterableSettings](model:Model, objective:Model)(implicit m1:Manifest[V1]) extends SamplerOverSettings[V1](model, objective) {
  def this(m:Model)(implicit man:Manifest[V1]) = this(m, null)
  def this()(implicit m1:Manifest[V1]) = this(Global.defaultModel)(m1)
  
	def settings(v:V1) : SettingIterator = v.settings

	// Manage and use a queue to revisit low-scoring factors more often; by default not used.
  var useQueue = false
  var maxQueueSize = 1000
  lazy val queue = new PriorityQueue[Factor]
  override def proposalHook(p:Proposal): Unit = {
    super.proposalHook(p)
    if (useQueue && !queue.isEmpty && Global.random.nextDouble < 0.3) 
      p.diff appendAll sampleFromQueue
  }
  def sampleFromQueue : DiffList = {
    if (queue.size < 1) return null
    val factor = queue.dequeue
    val variables = factor.variables.filter(v => m1.erasure.isAssignableFrom(v.getClass)).toSeq
    process1(variables(Global.random.nextInt(variables.size)).asInstanceOf[V1])
  }

}

/** GibbsSampler for generic "Variable with IterableSettings" */
class GibbsSampler(model:Model, objective:Model) extends GibbsSampler1[Variable with IterableSettings](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
}

