/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import scala.collection.mutable.IndexedSeqLike

// A collection abstract Variables for the distributions of generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).
                                                                               
// TODO Create factors for all generative distributions, so that BP will be supported.
// Make sure there is a factor between a Gaussian, its Real mean parameter, and the data generated from the Gaussian.
// So GeneratedVariables should have children also.
// Then factors could have gate numbers, and we could try harder to implement Minka and Winn's "Gates".

// TODO Consider also making a GenerativeFunction for deterministic relations between parent and child.


// TODO  Consider something like this.  A trait for factory objects
//trait GenerativeFactory[Source<:AbstractDistribution] { def apply(Source#OutcomeType#ValueType): Source#OutcomeType }

/** A Variable representing a probability distribution that generates other variable values with type O.
    It provides methods returning the probability of given values, and for (re-)sampling a new value into an existing variable. 
    Optionally, it keeps track of all the variables generated, for use in estimating the parameters of this distribution,
    and for use in finding neighbors for factor Templates. 
    @author Andrew McCallum */
// TODO Change this to O<:TypedVariable so that we can add def sampleValue:O#ValueType
//   but then make sure this still works for IntValue and RealValue.
// TODO Consider changing this name to simply "Distribution"
trait Distribution[O<:Variable] extends Variable {
  // Note that 'O' is not *required* to be a GenerativeVariable.  This allows us to put any DiscreteVariable into Multinomial, for example.
  type VariableType <: Distribution[O]
  type OutcomeType = O // TODO Consider insisting the OutcomeType = GenerativeObservation[O]; For now I've simly added a noop 'setSource' method to Variable
  // TODO Consider renaming all "sample" below to "children", in order to avoid confusion with the verb 'sample'.
  private lazy val _generatedSamples: HashSet[O] = new HashSet[O]
  def generatedSamples: scala.collection.Set[O] = _generatedSamples // TODO I want this to be .readOnly, but how in Scala 2.8?
  def weightedGeneratedSamples: Iterator[(O,Double)] = new Iterator[(O,Double)] {
    val elts = _generatedSamples.iterator
    def hasNext = elts.hasNext
    def next = (elts.next,1.0)
  }
  def keepGeneratedSamples = true
  /** The next two methods should only be called from SourceRefVariable!  Never call them yourself.  */
  def _registerSample(o:O)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    if (generatedSamples.contains(o)) throw new Error("Already generated outcome "+o) 
    _generatedSamples += o
    if (d != null) d += DistributionRegisterDiff(o)
  }
  def _unregisterSample(o:O)(implicit d:DiffList): Unit = if (keepGeneratedSamples) {
    _generatedSamples -= o
    if (d != null) d += DistributionUnregisterDiff(o)
  }
  // TODO consider removing preChange/postChange, because it requires extra infrastructure/Diffs from implementers?  Can just use (un)registerSample.  
  // TODO No.  Remove them because their functionality is now in CollapsedVariable!
  // TODO Yes!!  Remove these two methods because CollapsedVariable should just rely on _registerSample and _unregisterSample; I just hope that +/- from HashTable is efficient.
  /** Notify this Distribution that the value of its associated outcome 'o' is about to change.  
      Calls to this method are always paired with (followed by) a call to postChange. */
  //def preChange(o:O)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  //def postChange(o:O)(implicit d:DiffList): Unit = if (!generatedSamples.contains(o)) throw new Error("Outcome not present")
  /** Return the probability that this Distribution would generate outcome 'o' */
  def pr(o:O): Double
  /** Return the log-probability that this Distribution would generate outcome 'o' */
  def logpr(o:O): Double = Math.log(pr(o))
  /** Estimate parameters of this distribution, by some method to be determined later, but typically using only the parent and the sample children. */
  def estimate: Unit // TODO consider removing this.  Paramter estimation for generative models should be seen as inference?  Yes, I think so.
  // But still, it is convenient to have a multinomial.estimate method... Hmmm...
  case class DistributionRegisterDiff(m:O) extends Diff {
    def variable: Distribution[O] = Distribution.this//.asInstanceOf[VariableType]
    def redo = { assert(!_generatedSamples.contains(m))); _generatedSamples += m}
    def undo = { _generatedSamples -= m }
  }
  case class DistributionUnregisterDiff(m:O) extends Diff {
    def variable: Distribution[O] = Distribution.this //.asInstanceOf[VariableType]
    def redo = { _generatedSamples -= m }
    def undo = { assert(!_generatedSamples.contains(m)); _generatedSamples += m}
  }
}

trait GeneratedDistribution[O<:Variable,This<:GeneratedDistribution[O,This]] extends Distribution[O] with GeneratedVariable[This]


// TODO Consider something like this?  Would be needed in "def sample"
//trait RealParameter { val children ... }

// TODO Yes!!! Separate parameters from the distribution that results from them.
// Create deterministic functions.
// new Dirichlet(RealVector)
// Dirichlet -> Proportions; Proportions is a Variable
// new Multinomial(proportions), this is a Distribution, which isn't a variable, it is something that causes a factor to be created.
// Multinomial -> Discrete; Discrete is a Variable

trait Parameter {
  var children: List[ParameterizedFunction]
}
class RealParameter(x:Double) extends RealVariable(x) with Parameter {
  def +(p2:RealParameter) = new AddRealParameters(this,p2)
}
trait ParameterizedFunction { // Not a Variable
  def parameters: Seq[Parameter]
}
class AddRealParameters(p1:RealParameter, p2:RealParameter) extends ParameterizedFunction with RealValue {
  val parameters: Seq[Parameter] = List(p1, p2)
  def doubleValue = p1.doubleValue + p2.doubleValue
}
class ParameterizedDistribution[O<:Variable](params:Parameter*) extends ParameterizedFunction with Distribution[O] { // Not a Variable
  val parameters = params.toList
  def pr(o:O): Double
}
class Gaussian(val mean:RealParameter) extends ParameterizedDistribution[RealValue](mean) { // Not a Variable
  def this(m:Double) = this(new RealParameter(m))
  mean.children = mean.children :: this
}
class ProportionsVariable[O<:DiscreteValue](implicit m:Manifest[O])(ps:Seq[Double] = Array.tabulate(Domain[O](m).size)()) extends Seq[Double] with Variable
class ProportionsParameter[O<:DiscreteValue] extends ProportionsVariable[O] with Parameter
class Multinomial[O<:DiscreteValue](val proportions:ProportionsParameter[O]) extends ParameterizedDistribution[O](proportions) with GeneratedProportionsValue[O] with DiscreteDistribution[O] {
  def pr(o:O) = proportions(o.index)
}
object Multinomial {
  def apply[O<:DiscreteValue](p:ProportionsParameter[O]) = new Multinomial[O](p)
}
object ParameterTest {
  class Roll(i:Int) extends DiscreteVariable(i); Domain[Roll].size = 6
  val proportions = new ProportionsParameter[Roll]
  val rolls = for (i <- 1 to 100) yield new Roll :~ Multinomial(proportions)
}

class ParameterTemplate extends TemplateWithStatistics2[Parameter,ParameterizedFunction] {
  def unroll1(p:Parameter) = p.children.map(pf => Factor(p, pf)) // TODO This needs to make multiple hops through deterministic ParameterizedFunctions
  def unroll2(pf:ParameterizedFunction) = throw new Error("ParameterizedFunction should not change.")
  def score(s:Stat) = 0.0 // TODO Is this right?  No, perhaps a template like this should replace GeneratedVariableTemplate?
}



// Some specific cases of Distribution types

/** A Distribution that generates ordinal (non-negative Int) outcomes (perhaps even a OrdinalOutcome), for example a Poisson. */
trait OrdinalDistribution[O<:OrdinalValue] extends Distribution[O] {
  def sampleInt: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double = math.log(pr(index))
  def pr(o:O): Double = pr(o.intValue)
}


/** A Distribution that generates discrete (finite-range non-negative) outcomes (perhaps even a DiscreteOutcome), for example a Multinomial */
trait DiscreteDistribution[O<:DiscreteValue] extends OrdinalDistribution[O] {
  def sampleInt = sampleIndex
  def sampleIndex: Int
  def maxPrIndex: Int
  def proportion: Seq[Double] // TODO Consider also 'extends TypedValue[Seq[Double]]
}

/** A Distribution that generates proportions (vectors with values summing to 1.0), for example a Dirichlet*/
trait ProportionDistribution[O<:DiscreteValue] extends Distribution[GeneratedProportionValue[O]] {
  def size: Int
  def sampleProportions: Seq[Double] = sampleProportionsWithCounts(new { def apply(i:Int)=0.0; def length = size })
  def sampleProportionsWithCounts(counts:{def apply(i:Int):Double; def length:Int}): Seq[Double]
  def pr(proportions:GeneratedProportionValue[O]): Double
}

trait RealDistribution[O<:RealValue] extends Distribution[O] {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}

/** A Distribution that generates positive real values as Double (represented by a cc.factorie.Real), for example a Gamma distribution */
//trait PositiveRealDistributio[O<:Real] extends RealDistribution[O]






