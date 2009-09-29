package cc.factorie

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
    


class GibbsSampler1(model:Model) {
  def this() = this(Global.defaultModel)
	def sample(variable:Variable with IterableSettings) : DiffList = {
		val settings = variable.settings 
		case class Proposal(diff:DiffList, score:Double)
		val proposals = settings.map(s => {val d = new DiffList; s.set(d); new Proposal(d, d.scoreAndUndo(model))}).toList
		//proposals.foreach(p => println("Coordination "+p.score))
		val proposal = proposals.sampleExpProportionally(_.score)
		proposal.diff.redo
		proposal.diff
	}
	def sampleNFC(variable:Variable with IterableSettings with NoFactorCoordination) : DiffList = {
		val settings = variable.settings 
		case class Proposal(diff:DiffList, score:Double)
		//val factors = model.factors(variable)
		val proposals = settings.map(s => {val d = new DiffList; s.set(d); val p = new Proposal(d, model.score(variable)); d.undo; p}).toList
		//proposals.foreach(p => println("Coordination "+p.score))
		val proposal = proposals.sampleExpProportionally(_.score)
		proposal.diff.redo
		proposal.diff
	}
	def sampleNVC(variable:Variable with IterableSettings with NoVariableCoordination) : Unit = {
	  //println(variable)
		val settings = variable.settings
		case class Proposal(setting:{def set(d:DiffList):Unit}, score:Double)
		val proposals = settings.map(s => {s.set(null); new Proposal(s, model.score(variable))}).toList		  
		//proposals.foreach(p => println("NoCoordination "+p.score))
		val proposal = proposals.sampleExpProportionally(_.score)
		proposal.setting.set(null)
	}
	def sample[V<:Variable with IterableSettings](variables:Iterable[V], numIterations:Int) : Unit = {
	  for (i <- 1 to numIterations)	variables.map(sample(_))
	}
}

/*
  new Sampler2[Label,Label](model) {
    def unroll1(label:Label) = ...
    def unroll2(label:Label) = ...
    def sample(label1:Label, label2:Label) : Double
  }	
  
  new ProposalTemplate1[Label](model) with SampleRank {
    
  }
  
  new BPTemplate[Label,Label]
*/