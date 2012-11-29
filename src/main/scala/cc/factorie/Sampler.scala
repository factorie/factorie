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



package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{ListBuffer,ArrayBuffer,HashMap,PriorityQueue}
import cc.factorie.util.{Hooks0,Hooks1}

// How to think about Proposals and MCMC:
// Variables know their own range of values.  This needs to be coded on a per-variable basis
// Scored preferences about different values are known only by using the model.
// Sometimes we want to sample more than one variable together.  One variable cannot know how to do this on its own.
// Sometimes we want to sample conditioned on other fixed values.  One variable cannot know about this either.  It must be something like a Template
// Sometimes we want sampling to chain: sample v1, then v2 conditioned on the value of v1, etc.
// Making proposals is sometimes keyed by a single variable, a list of variables, or nothing (proposer itself maintains context of what to change next)
// Perhaps proposers should be in a list of Template-like objects; given a variable, first Template in the list to claim it gets to make the change.
// To facilitate ease of use, perhaps variable classes could provide something like:
//   class Label[T] { def defaultSampler = LabelSampler; def sample(model:Model) = defaultSampler.sample(this,model) }
//   object LabelSampler extends Sampler1[Label]
    

/** Samplers that key off of particular contexts.  Subclasses implement "process1(context:C)" */
trait Sampler[C] {
  type ContextType = C
  //if (contextClass == classOf[Nothing]) throw new Error("Constructor for class "+this.getClass.getName+" must be given type argument");
  /** The number of calls to process(numIterations:Int) or process(contexts:C,numIterations:Int). */
  var iterationCount = 0
  /** The number of calls to process(context:C) */
  var processCount = 0
  /** The number of calls to process that resulted in a change (a non-empty DiffList) */
  var changeCount = 0
  // TODO Consider renaming this to "processContext"? -akm
  /** Do one step of sampling.  This is a method intended to be called by users.  It manages hooks and processCount. */
  final def process(context:C): DiffList = {
    val processingWithoutContext = (null == context)
    val c = preProcessHook(context)
    // The preProcessHook might return null to indicate it doesn't want to sample this context, so check for it:
    if (c == null && !processingWithoutContext) return null // TODO should we return newDiffList here instead?
    val d = process1(c)
    processCount += 1
    postProcessHook(c, d)
    //diffHook(d)
    if (null != d && d.size > 0) changeCount += 1
    d
  }
  /** If true, calls to "newDiffList" will create a new DiffList to describe the changes they made, otherwise "newDiffList" will return null. */
  var makeNewDiffList = true
  /** Convenient method for setting makeNewDiffList to false, and returning this. */
  def noDiffList: this.type = { makeNewDiffList = false; this }
  /** In your implementation of "process1" use this method to optionally create a new DiffList, obeying "makeNewDiffList". */
  def newDiffList = if (makeNewDiffList) new DiffList else null
  /** The underlying protected method that actually does the work.  Use this.newDiffList to optionally create returned DiffList.
      Needs to be defined in subclasses. */
  def process1(context:C): DiffList // TODO Why isn't this 'protected'?  It should be... Oh, I see, GenericSampler needs to call this, but perhaps it should be removed.
  final def processAll(contexts:Iterable[C]): Unit = { 
    contexts.foreach(process(_))
    iterationCount += 1
    postIterationHooks
    if (!postIterationHook) return 
  }
  // TODO Consider renaming this processContexts or sampleFromContext.  See also Trainer.processExamples.
  final def processAll(contexts:Iterable[C], numIterations:Int): Unit = for (i <- 0 until numIterations) processAll(contexts)
  // TODO Consider renaming this processContext
  final def process(context:C, repeat:Int): Unit = for (i <- 0 until repeat) process(context)
  //private var processingWithoutContext = false
  final def process(count:Int): Unit = {
    //processingWithoutContext = true // examined in process()
    for (i <- 0 to count) process(null.asInstanceOf[C]) // TODO Why is this cast necessary?;
    //processingWithoutContext = false
  }
 
  // Hooks
  /** Called just before each step of sampling.  Return an alternative variable if you want that one sampled instead.  
      Return null if you want to abort sampling of this context. */
  def preProcessHook(context:C): C = context 
  /** Call just after each step of sampling. */
  def postProcessHook(context:C, difflist:DiffList): Unit = {}
  /** An alternative to postProcessHook that does not require the type C. */ // TODO Really have both?  Remove 'context:C' from postProcessHook?
  //def diffHook(difflist:DiffList): Unit = {}
  /** Called after each iteration of sampling the full list of variables.  Return false if you want sampling to stop early. */
  def postIterationHook: Boolean = true
  def postIterationHooks = new Hooks0
}




/** Samplers that generate a list of Proposal objects, and select one log-proportionally to their modelScore.
    Proposal objects come from abstract method "proposals". 
    @author Andrew McCallum */
trait ProposalSampler[C] extends Sampler[C] {
  def model: Model
  var temperature = 1.0 // Not used here, but used in subclasses; here for uniformity // TODO Consider moving use from SettingsSampler to this.process1
  def proposals(context:C): Seq[Proposal]
  def skipEmptyProposals = true
  def process1(context:C): DiffList = processProposals(proposals(context))
  def processProposals(props: Seq[Proposal]): DiffList = {
    if (props.size == 0 && skipEmptyProposals) return newDiffList
    proposalsHook(props)
    val proposal = props.size match {
      case 0 => throw new Error("No proposals created.")
      case 1 => props.head 
      case _ => pickProposal(props)
    }
    proposal.diff.redo
    proposalHook(proposal)
    proposal.diff
  }
  def pickProposal(proposals:Seq[Proposal]): Proposal = proposals.sampleExpProportionally((p:Proposal) => p.acceptanceScore)
  val proposalsHooks = new Hooks1[Seq[Proposal]] // Allows non-overriders to add hooks
  def proposalsHook(proposals:Seq[Proposal]): Unit = proposalsHooks(proposals)
  val proposalHooks = new Hooks1[Proposal]
  def proposalHook(proposal:Proposal): Unit = proposalHooks(proposal)
}


/** Tries each one of the settings in the Iterator provided by the abstract method "settings(C)", 
    scores each, builds a distribution from the scores, and samples from it.
    @author Andrew McCallum */
abstract class SettingsSampler[C](theModel:Model, theObjective:Model = null) extends ProposalSampler[C] {
  //def this(m:Model) = this(m, null)
  def model: Model = theModel
  def objective = theObjective 
  /** Abstract method must be implemented in sub-classes.  
      Provides access to all different possible worlds we will evaluate for each call to 'process' */ 
  def settings(context:C) : SettingIterator

  //val proposalsCache = collection.mutable.ArrayBuffer[Proposal]() // TODO This is not thread-safe; remove it
  def proposals(context:C): Seq[Proposal] = {
    val result = new ArrayBuffer[Proposal]
    // the call to 'next' is actually what causes the change in state to happen
    var i = 0
    val si = settings(context)
    while (si.hasNext) {
      val d = si.next
      assert(model ne null) // TODO!!! Clean up and delete this
      val (m,o) = d.scoreAndUndo(model, objective)
      //if (proposalsCache.length == i) proposalsCache.append(null)
      result += new Proposal(d, m, o, m/temperature)
      i += 1
    }
    //if (proposalsCache.length > i) proposalsCache.trimEnd(proposalsCache.length - i)
    //assert(proposalsCache.length == i)
    //val s = settings(context).map(d => {val (m,o) = d.scoreAndUndo(model,objective); new Proposal(d, m, o, m/temperature)}).toList
    //if (s.exists(p=>p.modelScore > 0.0)) { s.foreach(p => println(p.modelScore+" "+model)); println("SettingsSampler^") }
    //proposalsCache.toSeq
    result
  } 
}

/** Instead of randomly sampling according to the distribution, always pick the setting with the maximum acceptanceScore. */
abstract class SettingsMaximizer[C](theModel:Model, theObjective:Model = null) extends SettingsSampler[C](theModel, theObjective) {
  override def pickProposal(proposals:Seq[Proposal]): Proposal = proposals.maxByDouble(_.acceptanceScore)
}

/** Tries each one of the settings of the given variable, 
    scores each, builds a distribution from the scores, and samples from it.
    This is exactly Gibbs sampling over a finite number of possible values of the variable.
    Note:  This differs from cc.factorie.generative.GibbsSampler in that GibbsSampler may not iterate over settings, but instead samples from a closed-form distribution.
    Because SampleRank requires Proposal objects, we use this intsead of GibbsSampler.
    @see generative.GibbsSampler
    @author Andrew McCallum */
class VariableSettingsSampler[V<:Variable with IterableSettings](model:Model, objective:Model = null) extends SettingsSampler[V](model, objective) {
  def settings(v:V): SettingIterator = v.settings
}

// TODO Remove and recommend GibbsSampler instead
class VariablesSettingsSampler[V<:Variable with IterableSettings](model:Model, objective:Model = null) extends SettingsSampler[Seq[V]](model, objective) {
  def settings(variables:Seq[V]): SettingIterator = new SettingIterator {
    val vs = variables.map(_.settings).toList
    val vds = variables.map(v => new DiffList).toList // maintains a list of changes for each variable
    var initialized = false
    //println("VariablesSettingsSampler.settings "+variables)
    var _hasNext = true
    var prevDiffList: DiffList = null
    /**Iterate through all combinations of values in Variables given their `SettingIterators */
    def nextValues(vs: List[IterableSettings#SettingIterator], vds: List[DiffList]): Boolean = {
      if (vs == Nil) false
      else if (vs.head.hasNext) {
        val vd = vs.head.next(vds.head); vds.head.clear(); vds.head ++= vd; // update the changelist for the variable
        //println("nextValues changed "+vs.map(_.variable));
        true /*(vs.head.hasNext || vs.tail != Nil)*/
      } else if (vs.tail != Nil) {
        vs.head.reset; val vd = vs.head.next(vds.head); vds.head.clear(); vds.head ++= vd; // update the changelist for the variable
        //println("nextValues changed "+vs.map(_.variable));
        nextValues(vs.tail, vds.tail)
      }
      else false
    }
    def next(d:DiffList): DiffList = {
      // TODO Should we instead let result = d ?  But what if it is null?
      val result = newDiffList
      if (!initialized) {
        vs.foreach(setting => { setting.reset; setting.next(result) })
        initialized = true
        _hasNext = true
      } else {
        //if (prevDiffList ne null) { prevDiffList.redo; prevDiffList.done = false } // TODO  Ug!  Ugly hack that will not generalize!
        _hasNext = nextValues(vs, vds)
        // copy over the difflist for each variable to the result
        vds.foreach(vd => { vd.done = false; vd.redo; result ++= vd })
      }
      //println("VariablesSettingsSampler.next "+vs.map(_.variable)+" hasNext="+this.hasNext)
      result
    }
    def reset: Unit = { vs.foreach(_.reset); prevDiffList = null }
    def hasNext: Boolean = vs.exists(_.hasNext)
  }
}

/** Besag's Iterated Conditional Modes.  Visit a variable, and set it to its highest scoring value (based on current value of its factors' neighbors). */
class IteratedConditionalModes[V<:Variable with IterableSettings](model:Model, objective:Model = null) extends SettingsMaximizer[V](model, objective) {
  def settings(v:V): SettingIterator = v.settings
}


/** Manage and use a queue to more often revisit low-scoring factors and re-sample their variables. */
// Consider FactorQueue { this: Sampler[_] => ... abstract override postProcessHook(C,DiffList).  But what to do about C?  
trait FactorQueue[C] extends Sampler[C] {
  var useQueue = true
  var maxQueueSize = 1000
  lazy val queue = new PriorityQueue[Factor]
  /** The proportion of sampling process steps to take from the queue, versus from the standard source of contexts. */
  var queueProportion = 0.5
  
  /** Override to provide the generic sampler that can potentially deal with arbitrary variables coming from Factors */
  def process0(x:AnyRef): DiffList
  def model: Model
  
  override def postProcessHook(context:C, diff:DiffList): Unit = {
    super.postProcessHook(context, diff)
    if (useQueue) {
      var queueDiff: DiffList = new DiffList
      if (queueProportion > 1.0 && !queue.isEmpty) {
        for (i <- 0 until (queueProportion.toInt)) if (!queue.isEmpty) {
          val qd = sampleFromQueue
          if (qd != null) queueDiff ++= qd
        }
      } else if (!queue.isEmpty && cc.factorie.random.nextDouble < queueProportion) {
        val qd = sampleFromQueue
        if (qd != null) queueDiff ++= qd
      }
      if (maxQueueSize > 0) {
        queue ++= model.factors(diff)
        if (queue.size > maxQueueSize) throw new Error // TODO find alternative, reduceToSize is missing from Scala 2.8; queue.reduceToSize(maxQueueSize)
      }
      diff appendAll queueDiff
    }
  }
  def sampleFromQueue : DiffList = {
    val factor = queue.dequeue // TODO consider proportionally sampling from the queue instead
    for (variable <- factor.variables.toSeq.shuffle; if (!variable.isConstant)) {
      val difflist = process0(variable)
      if (difflist != null && difflist.size > 0) return difflist
    }
    null
  }
}

// TODO But I haven't been able to make this existential typing work in practice yet.
/*
trait AlternativeFactorQueue extends Sampler[C forSome {type C <: Variable}] {
  override def diffHook(diff:DiffList): Unit = {
   println("foo") 
  }
}
*/


