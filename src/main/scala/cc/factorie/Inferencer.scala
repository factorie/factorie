package cc.factorie
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie.util.Implicits._

// Preliminary steps toward generic interfaces to inference
// Eventually we will have marginals over factors instead of variables


// Generic over sampling-based inference and variational inference
trait Marginal
trait Lattice

trait Inferencer[V<:Variable,C] {
  type LatticeType <: Lattice
  /** Infer the target 'variables' using 'varying' to drive the exploration of changes to the configuration.  
      For example, in SamplingInference, 'varying' are Sampler 'contexts' which do not necessarily have to be Variables;
      they are arbitrary keys to the sampler that may drive exploration, which will eventually result in changes to the 'target' variables.
      If for you this separation of target variables and sampling contexts is unnecessary or confusing, consider VariableInferencer instead. */
  def infer(variables:Collection[V], varying:Collection[C]): LatticeType
  //def infer(factors:TemplateList[VectorTemplate], variables:Collection[V]): LatticeType
}

trait VariableInferencer[V<:Variable] extends Inferencer[V,V] {
  /** Infer the 'variables', changing their values, but not other variables' (except perhaps through variable-value coordination). */
  def infer(variables:Collection[V]): LatticeType = infer(variables, variables)
  /** Infer the 'targets' variables, considering not only changes their their values, but also changes to the 'marginalizing' variables. */
  def inferMarginalizing(targets:Collection[V], marginalizing:Collection[V]) = infer(targets, { val a = new ArrayBuffer[V]; a ++= targets; a ++= marginalizing; a})
}

trait Maximizer[V<:Variable] extends Inferencer[V,V] // Include something like this?
// 'infer' here would actually change state to the maximum found
// 'infer' in Inferencer would leave it in some random state, with the results really in the Marginal objects?

// TODO Something like this also??  Where will optimizers like CongugateGradient and BFGS go?  
trait Optimizer {
  def optimize: Unit
  def optimize(numIterations:Int): Unit
}


// Note that putting a [V], as in DenseCountsMultinomial[V], doesn't work here because IndexedVariable not <: MultinomialOutcome[V].  
// But as long as we don't use any methods that require [V], I think we are OK.
class IndexedMarginal[V<:IndexedVariable](val variable:V) extends DenseCountsMultinomial(variable.domain.size) with Marginal {
  keepGeneratedSamples = false
  def increment : Unit = variable match {
    case v:SingleIndexedVariable => increment(v.index, 1.0)(null)
    case v:BinaryVectorVariable[_] => v.incrementInto(this)
  }
}

// TODO This is over variables.  We want something over Factors... and perhaps also something separate over Variables
class SamplingLattice[V<:IndexedVariable](variables:Collection[V]) extends Lattice {
  val map = new HashMap[V,IndexedMarginal[V]]
  variables.foreach(v => map(v) = new IndexedMarginal(v))
  def marginal(v:V) = map(v)
  def apply(v:V) = map(v)
  def marginals: Iterator[IndexedMarginal[V]] = map.values
}

// A simple special case, to be generalized later
class SamplingInferencer[V<:SingleIndexedVariable,C](val sampler:Sampler[C]) extends Inferencer[V,C] {
  type LatticeType = SamplingLattice[V]
  var burnIn = 100 // I really want these to be default-valued parameters to infer, in Scala 2.8.
  var thinning = 20
  var iterations = 500
  def infer(targets:Collection[V], contexts:Collection[C]): SamplingLattice[V] = {
    val lat = new SamplingLattice(targets)
    sampler.process(contexts, burnIn)
    for (i <- 0 until iterations/thinning) {
      sampler.process(contexts, thinning)
      targets.foreach(v => lat.marginal(v).increment)
    }
    lat
  }
}

class VariableSamplingInferencer[V<:SingleIndexedVariable](sampler:Sampler[V]) extends SamplingInferencer[V,V](sampler) with VariableInferencer[V] {
  def this() = this(new GibbsSampler1[V])
  def this(model:Model) = this(new GibbsSampler1[V](model))
}


// Max-Product inference, finding the best scoring configuration

/** The result of inference by a SamplingMaximizer.  
    'diff' is the list the changes from the initial configuration.  It may be null if no DiffList is provided to 'infer'.
    'diffScore' is the relative change in model score from the initial configuration. */
class SamplingMaximizerLattice(val diff:DiffList, val diffScore:Double) extends Lattice

/** Provide 'infer' method that uses the 'sampler' to search for the best-scoring configuration. */
// TODO Update this for the new separated "modelScore" and "acceptScore" in Proposal.
class SamplingMaximizer[V<:Variable with IterableSettings](val sampler:ProposalSampler[V]) extends Maximizer[V] with VariableInferencer[V] {
  def this(model:Model) = this(new GibbsSampler1[V](model))
  type LatticeType = SamplingMaximizerLattice
  var iterations = 500
  var rounds = 10
  var initialTemperature = 1.0
  var finalTemperature = 0.1
  def infer(variables:Collection[V], varying:Collection[V]): LatticeType = inferd(variables, varying)(null)
  // TODO I really want Scala 2.8 default parameters: (implicit diff:DiffList = null)  !!!
  def inferd(variables:Collection[V], varying:Collection[V])(implicit diff:DiffList): LatticeType = {
  	var currentScore = 0.0
    var maxScore = currentScore
    val maxdiff = new DiffList
    sampler.temperature = initialTemperature
    def updateMaxScore(p:Proposal): Unit = {
      currentScore += p.modelScore // TODO Check proper handling of fbRatio
      //println("SamplingMaximizer modelScore="+p.modelScore+" currentScore="+currentScore)
      if (diff != null) diff appendAll p.diff
      if (currentScore > maxScore) {
        maxScore = currentScore
        maxdiff.clear
        //println("SamplingMaximizer maxScore="+maxScore)
      } else if (p.diff.size > 0) {
        maxdiff appendAll p.diff
        //println("SamplingMaximizer diff.size="+diff.size)
      }
    }
    val updateHook: Proposal=>Unit = updateMaxScore _ 
    sampler.proposalHooks += updateHook 
    //sampler.proposalsHooks += { (props:Seq[Proposal]) => { props.foreach(p => println(p.modelScore)) }}
    for (i <- 0 until rounds) {
    	sampler.process(varying, iterations/rounds)
    	sampler.temperature += (finalTemperature-initialTemperature)/rounds
    	//println("Reducing temperature to "+sampler.temperature)
    }
    maxdiff.undo // Go back to maximum scoring configuration
    sampler.proposalHooks -= updateHook
    new SamplingMaximizerLattice(diff, maxScore)
  }
}


