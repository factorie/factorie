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
abstract class GibbsSamplerOverSettings1[V1<:Variable](val model:Model)(implicit m1:Manifest[V1]) extends VariableSampler1[V1](m1) {
	//println("GibbsSamplerOverSettings V1="+m1)

  // This method must be implemented in sub-classes
  def settings(v:V1) : Iterator[{def set(d:DiffList):Unit}];

  // Meta-parameters
  var temperature = 1.0
  var useQueue = false
  var maxQueueSize = 500
  lazy val queue = new PriorityQueue[Factor]
  
  // Diagnostic information
  val numChanges = 0

  /** For storing one of the proposals considered.  Note that objectiveScore may not be truly set, in which case it will have value Math.NaN_DOUBLE. */
	//case class Proposal(diff:DiffList, modelScore:Double, objectiveScore:Double)
 
	// Methods override in learning methods
	def proposalsHook(proposals:Seq[Proposal]) : Unit = {}
  /** If you want the Proposals to actually contain the objectiveScore, override this method appropriately.  Used for training.	*/
  def objective : Model = null

  /** Consider all settings(variable), and select one proportionally to the distribution over model scores. */
  def sample(variable:V1) : DiffList = {
    val queueDiff : Seq[Diff] = if (useQueue && Global.random.nextDouble < 0.3 && queue.size > 1)
      sampleFromQueue else Nil
		val vsettings = settings(variable) 
		variable match {
		  /* // TODO Consider re-adding something like this, but using the above case class Proposal, and populating the DiffList 
		  // For cases in which changing this variable value does not change any other variables' values
		  case v:NoVariableCoordination => {
		  	case class Proposal(setting:{def set(d:DiffList):Unit}, score:Double)
		  	val proposals = vsettings.map(s => {s.set(null); new Proposal(s, model.score(variable)/temperature)}).toList
		  	val proposal = proposals.sampleExpProportionally(_.score)
		  	val d = new DiffList
		  	proposal.setting.set(d)
		  	if (useQueue && !queueDiff.isEmpty) d prependAll queueDiff
		  	d
		  }*/
		  /* // TODO Not sure this is valid
		  // For cases in which changing this variable value might change some other variables' values, 
		  // but all the changes can be properly evaluating using only the factors that touch this variable
		  case v:NoFactorCoordination => {
		    // TODO Note that the modelScore in Proposal will now not be a diff, but a raw score.  Is this OK??? !!!;
		  	val proposals = vsettings.map(s => {val d = new DiffList; s.set(d); val p = new Proposal(d, model.score(variable)/temperature, objectiveScore(d)); d.undo; p}).toList
		  	proposalsHook(proposals)
		  	val proposal = proposals.sampleExpProportionally(_.modelScore)
		  	proposal.diff.redo
		  	if (useQueue && !queue.isEmpty) proposal.diff prependAll queueDiff
		  	proposal.diff
     } */
		  // For cases in which there are no guarantees about variable factor- and change-locality
		  case _ => {
		    // TODO Find a way to make more efficient by avoiding toList below
		  	val proposals = 
		  		if (objective == null)
		  			vsettings.map(s => {val d = new DiffList; s.set(d); val m = d.scoreAndUndo(model); new Proposal(d, m/temperature, Math.NaN_DOUBLE)}).toList
		  		else
		  			vsettings.map(s => {val d = new DiffList; s.set(d); val (m,o) = d.scoreAndUndo(model,objective); new Proposal(d, m/temperature, o)}).toList
		  	//proposals.foreach(p => println("GibbsSampler1 proposal score="+p.score)); println
		  	proposalsHook(proposals)
		  	val proposal = proposals.sampleExpProportionally(_.modelScore)
		  	proposal.diff.redo
		  	if (useQueue && !queue.isEmpty) proposal.diff prependAll queueDiff
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


/** GibbsSampler for a subclass of Variable with IterableSettings */
class GibbsSampler1[V1<:Variable with IterableSettings](model:Model)(implicit m1:Manifest[V1]) extends GibbsSamplerOverSettings1[V1](model)(m1) {
	//println("GibbsSampler1 V1="+m1)
  def this()(implicit m1:Manifest[V1]) = this(Global.defaultModel)(m1)
	def settings(v:V1) = v.settings
}

/** GibbsSampler for generic Variable with IterableSettings */
class GibbsSampler(model:Model) extends GibbsSampler1[Variable with IterableSettings](model) {
  def this() = this(Global.defaultModel)
}

