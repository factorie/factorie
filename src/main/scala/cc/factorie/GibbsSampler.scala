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
abstract class GibbsSamplerOverSettings1[V1<:Variable](val model:Model, val objective:Model)(implicit m1:Manifest[V1]) extends ProposalSampler[V1] {
  def this(m:Model)(implicit man:Manifest[V1]) = this(m, null)
	//println("GibbsSamplerOverSettings V1="+m1+"  objective="+objective)

  // This method must be implemented in sub-classes
  def settings(v:V1) : Iterator[{def set(d:DiffList):Unit}];

  // Meta-parameters
  var temperature = 1.0
  var useQueue = false
  var maxQueueSize = 1000
  lazy val queue = new PriorityQueue[Factor]
  
  // Diagnostic information
  val numChanges = 0

	// Methods override in learning methods
  /** If you want the Proposals to actually contain the objectiveScore, override this method appropriately.  Used for training.	*/
  //def objective : Model = null
  
  def proposals(variable:V1) : Seq[Proposal] = {
		val vsettings = settings(variable)
		vsettings.map(s => {val d = new DiffList; s.set(d); val (m,o) = d.scoreAndUndo(model,objective); new Proposal(d, m/temperature, o)}).toList
  }
  
  override def proposalHook(p:Proposal): Unit = {
    super.proposalHook(p)
    if (useQueue && !queue.isEmpty && Global.random.nextDouble < 0.3) 
    	p.diff prependAll sampleFromQueue
  }

  def sampleFromQueue : DiffList = {
    if (queue.size < 1) return null
    val factor = queue.dequeue
    val variables = factor.variables.filter(v => m1.erasure.isAssignableFrom(v.getClass)).toSeq
    process1(variables(Global.random.nextInt(variables.size)).asInstanceOf[V1])
  }
}


/** GibbsSampler for a subclass of Variable with IterableSettings */
class GibbsSampler1[V1<:Variable with IterableSettings](model:Model, objective:Model)(implicit m1:Manifest[V1]) extends GibbsSamplerOverSettings1[V1](model, objective) {
  def this(m:Model)(implicit man:Manifest[V1]) = this(m, null)
	//println("GibbsSampler1 V1="+m1)
  def this()(implicit m1:Manifest[V1]) = this(Global.defaultModel)(m1)
	def settings(v:V1) = v.settings
}

/** GibbsSampler for generic "Variable with IterableSettings" */
class GibbsSampler(model:Model, objective:Model) extends GibbsSampler1[Variable with IterableSettings](model, objective) {
  def this(m:Model) = this(m, null)
  def this() = this(Global.defaultModel)
}

