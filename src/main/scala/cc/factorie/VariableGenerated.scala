/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet

// A collection of abstract Variables (and a generic Template) for generative models (directed Bayesian networks, 
// as opposed to undirected in which there is not a DAG-shaped generative storyline).

trait GeneratedValue extends Variable {
  /** The list of random variables that control the generation of this value. */
  def parents: Seq[Parameter]
  /** Sometimes pointers to parents are kept in a ParameterRef variable; 
      if so, return them here so that we can track diffs to them; 
      if not, return Nil or null entries in sequence matching 'parents'. */
  def parentRefs: Seq[AbstractParameterRef] = Nil
  /** The probability of the current value given its parents. */
  def pr:Double
  /** The log-probability of the current value given its parents. */
  def logpr:Double = math.log(pr)
}
trait GeneratedVariable extends GeneratedValue {
  /** Sample a new value for this variable given only its parents */
  def sample(implicit d:DiffList): Unit
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit
}
trait Parameter extends Variable {
  private lazy val _children = new HashSet[GeneratedValue]
  def keepChildren = true
  def children: Iterable[GeneratedValue] = _children
  def addChild(v:GeneratedValue)(implicit d:DiffList): Unit = if (keepChildren) {
    if (_children.contains(v)) throw new Error("Parameter "+this+" already has child "+v)
    _children += v
    if (d ne null) d += ParameterAddChildDiff(v)
  }
  def removeChild(v:GeneratedValue)(implicit d:DiffList): Unit = if (keepChildren) {
    _children -= v
    if (d ne null) d += ParameterRemoveChildDiff(v)
  }
  //def weightedChildren: Iterable[(GeneratedValue,Double)]
  case class ParameterAddChildDiff(v:GeneratedValue) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { assert(!_children.contains(v)); _children += v }
    def undo = { _children -= v }
  }
  case class ParameterRemoveChildDiff(v:GeneratedValue) extends Diff {
    def variable: Parameter = Parameter.this
    def redo = { _children -= v }
    def undo = { assert(!_children.contains(v)); _children += v }
  }
}

trait DeterministicFunction extends Parameter
trait RealFunction extends DeterministicFunction with RealValue
abstract class RealOpConstant(val real:RealValueParameter) extends RealFunction with GeneratedValue {
  real.addChild(this)(null) // But now might not garbage collect this when we want to
  def parents = List(real)
  def pr = 1.0 // Deterministic value given parent
}
class RealPlusConstant(override val real:RealValueParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue + constant
}
class RealTimesConstant(override val real:RealValueParameter, val constant:Double) extends RealOpConstant(real) {
  def doubleValue = real.doubleValue * constant
}

trait AbstractParameterRef extends Variable {
  def abstractValue: Parameter
  def child: GeneratedValue
}
class ParameterRef[P<:Parameter,C<:GeneratedValue](p:P, override val child:C) extends RefVariable(p) with AbstractParameterRef {
  def abstractValue = this.value
  // This 'set' method will be called in initialization of RefVariable
  override def set(newValue:P)(implicit d:DiffList): Unit = if (newValue != value) {
    if (value ne null) value.removeChild(child)
    super.set(newValue)
    if (value ne null) value.addChild(child)
  }
}
class GatedParameterRef[P<:Parameter,C<:MixtureOutcome](val parameters:Seq[P], g:Gate, child:C) extends ParameterRef[P,C](parameters.apply(g.intValue), child) with GatedRefVariable[P] {
  gate = g
  g += this
  assert(parameters.length == gate.domainSize)
  def valueForIndex(index:Int) = parameters(index)
  def domainSize = parameters.length
}

trait RealGenerating {
  def sampleDouble: Double
  def pr(x:Double): Double
  def logpr(x:Double): Double
}
trait DiscreteGenerating {
  def length: Int
  def sampleInt: Int
  def pr(index:Int): Double
  def logpr(index:Int): Double
}
trait ProportionGenerating {
  def sampleProportions: Proportions
  def pr(p:Proportions): Double
  def logpr(p:Proportions): Double
}


trait RealValueParameter extends RealValue with Parameter
class RealVariableParameter(value:Double) extends RealVariable(value) with RealValueParameter // TODO Awkward name?
trait IntegerValueParameter extends IntegerValue with Parameter
class IntegerVariableParameter(value:Int) extends IntegerVariable(value) with IntegerValueParameter
trait Proportions extends Parameter with IndexedSeq[Double] with DiscreteGenerating {
  def sampleInt = Maths.nextDiscrete(this)(Global.random)
  def pr(index:Int) = apply(index)
  def logpr(index:Int) = math.log(apply(index))
  def maxPrIndex: Int = { var maxIndex = 0; var i = 1; while (i < size) { if (this(i) > this(maxIndex)) maxIndex =i; i += 1 }; maxIndex }
}
trait TypedProportions[A<:DiscreteValue] extends Proportions
class DenseProportions(p:Seq[Double]) extends Proportions {
  private var _p = new Array[Double](p.size)
  if (p != Nil) this := p else setUniform(null)
  @inline final def length = _p.size
  @inline final def apply(index:Int) = _p(index)
  def set(p:Seq[Double])(implicit d:DiffList): Unit = {
    assert(p.size == _p.size)
    val newP = p.toArray
    if (d ne null) d += ProportionsDiff(_p, newP)
    _p = newP
  }
  def :=(p:Seq[Double]) = set(p)(null)
  def setUniform(implicit d:DiffList): Unit = set(new UniformProportions(size))
  case class ProportionsDiff(oldP:Array[Double], newP:Array[Double]) extends Diff {
    def variable = DenseProportions.this
    def undo = _p = oldP
    def redo = _p = newP
  }
}
class UniformProportions(val length:Int) extends Proportions {
  @inline final def apply(index:Int) = 1.0/size
}
class DenseCountsProportions(len:Int) extends Proportions {
  private var _counts = new Array[Double](len)
  private var _countsTotal = 0.0
  def length = _counts.size
  def counts(index:Int) = _counts(index)
  def countsTotal  = _countsTotal
  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = { _counts(index) += incr; _countsTotal += incr }
  def decrement(index:Int, incr:Double)(implicit d:DiffList): Unit = { _counts(index) -= incr; _countsTotal -= incr }
  def apply(index:Int): Double = {
    //if (parent != null) (counts(index) + parent.alpha(index)) / (countsTotal + parent.alphaSum) else 
    if (_countsTotal == 0)
      1.0 / size
    else
      _counts(index) / _countsTotal
  }
}

// A Proportions ~ Dirichlet(Proportions, Precisio)
trait Dirichlet extends Proportions with GeneratedVariable {
  /** Return the mean of the Dirichlet distribution from which this Proportions was generated.
      Note that the returned object may be a temporary one generated for the return value, 
      and may not have this Proportions as a child. */
  def mean: Proportions
  def precision: RealValue
  def pr = 0.0 // TODO implement p(Proportions) ~ Dirichlet
}
class DenseDirichlet(initialMean:Proportions, initialPrecision:RealValueParameter, p:Seq[Double] = Nil) extends DenseProportions(p) with Dirichlet {
  private val meanRef = new ParameterRef(initialMean, this)
  private val precisionRef = new ParameterRef(initialPrecision, this)
  def mean = meanRef.value
  def mean_=(newMean:Proportions)(implicit d:DiffList): Unit = meanRef.set(newMean)
  def precision = precisionRef.value
  def precision_=(newPrecision:RealValueParameter)(implicit d:DiffList): Unit = precisionRef.set(newPrecision)
  def parents = List(mean, precision)
  def ~(mean:Proportions, precision:RealValueParameter): this.type = { mean_=(mean)(null); precision_=(precision)(null); this }
  def sample(implicit d:DiffList): Unit = set(this.mean) // TODO Just setting to mean of parent for now
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList) = parents match {
    case Seq(mean:Proportions, precision:RealValueParameter) => set(mean) // TODO Just setting to mean for now
  }
}
class DenseSymmetricDirichlet(val alpha:RealValueParameter, p:Seq[Double] = Nil) extends DenseProportions(p) with Dirichlet {
  //val length = p.length
  //def apply(index:Int) = alpha.doubleValue
  lazy val mean = new UniformProportions(size)
  def precision = new RealObservation(alpha.doubleValue * size) // TODO Should we instead make a version of RealTimesConstant that does't make itself a child of 'alpha'?
  override def parents = List(alpha)
  //def pr = 0.0 // TODO
  def sample(implicit d:DiffList): Unit = set(mean) // TODO Just setting to mean of parent for now
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(alpha:RealValueParameter) => set(mean) // TODO Just setting to mean for now
  }
}
class DirichletMultinomial(val mean:Proportions, val precision:RealValueParameter) extends CollapsedParameter with Proportions {
  //def this(dirichlet:Dirichlet) = this(dirichlet.mean, dirichlet.precision)
  def parents = List(mean, precision)
  def pr = 1.0 // TODO implement.  Since this is collapsed, what should it be?  1.0?
  def length = mean.length
  mean.addChild(this)(null)
  precision.addChild(this)(null)
  def detatch: Unit = { mean.removeChild(this)(null); precision.removeChild(this)(null) }
  private val counts = new Array[Double](mean.length)
  private var countsTotal = 0.0
  def increment(index:Int, incr:Double): Unit = { counts(index) += incr; countsTotal += incr }
  //def decrement(index:Int, incr:Double): Unit = { counts(index) -= incr; countsTotal -= incr }
  def apply(index:Int) : Double = {
    //if (parent != null) (counts(index) + parent.alpha(index)) / (countsTotal + parent.alphaSum) else
    if (countsTotal == 0)
      1.0 / size
    else
      counts(index) / countsTotal
  }
  override def addChild(c:GeneratedValue)(implicit d:DiffList): Unit = {
    c match { case d:DiscreteValue => increment(d.intValue, 1.0) }
    super.addChild(c)(d)
  }
  override def removeChild(c:GeneratedValue)(implicit d:DiffList): Unit = {
    c match { case d:DiscreteValue => increment(d.intValue, -1.0) }
    super.removeChild(c)(d)
  }
  // Perhaps Proportions should not be a GeneratedVariable?
  def sample(implicit d:DiffList): Unit = new Error
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = new Error
}

/*class Binomial(p:RealValueParameter, trials:Int) extends OrdinalVariable with GeneratedVariable {
  this := 0
}*/
trait GeneratedDiscreteValue extends GeneratedValue with DiscreteValue {
  def proportions: Proportions
  def parents = List(proportions)
  def pr: Double = proportions(this.intValue)
  //def ~(proportions:Proportions): this.type = { proportions_=(proportions)(null); this }
}
trait GeneratedDiscreteVariable extends GeneratedDiscreteValue {
  this: DiscreteVariable with GeneratedDiscreteValue =>
  def sample(implicit d:DiffList): Unit = setByIndex(proportions.sampleInt)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList) = parents match {
    case Seq(p:Proportions) => setByIndex(p.sampleInt)
  }
  def maximize(implicit d:DiffList): Unit = setByIndex(proportions.maxPrIndex)
}
// A Discrete ~ Multinomial(Proportions), in which we can change the parent
class Discrete(p:Proportions, value:Int = 0) extends DiscreteVariable(value) with GeneratedDiscreteVariable {
  assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  override def parentRefs = List(proportionsRef)
}
trait GeneratedCategoricalValue[A] extends GeneratedDiscreteValue with CategoricalValue[A]
trait GeneratedCategoricalVariable[A] extends GeneratedCategoricalValue[A]
class Categorical[A](p:Proportions, value:A) extends CategoricalVariable(value) with GeneratedCategoricalVariable[A] {
  assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  override def parentRefs = List(proportionsRef)
}
class ObservedDiscrete(p:Proportions, value:Int) extends DiscreteObservation(value) with GeneratedValue {
  // TODO Rename "DiscreteConstant"?
  assert(p.length <= domainSize)
  private val proportionsRef = new ParameterRef(p, this)
  def proportions = proportionsRef.value
  def proportions_=(p2:Proportions)(implicit d:DiffList) = { assert(p2.length <= domainSize); proportionsRef.set(p2) }
  def parents = List(proportionsRef.value)
  override def parentRefs = List(proportionsRef)
  def pr: Double = proportions(this.intValue)
}
class ObservedDiscretes(val proportions:Proportions, values:Traversable[Int] = Nil) extends DiscreteValues with GeneratedValue with ConstantValue {
  assert(proportions.length <= domainSize)
  proportions.addChild(this)(null)
  def parents = List(proportions)
  private val _values = values.toArray
  override def logpr: Double = { var result = 0.0; forIndex(_values.size)(index => result += math.log(proportions(index))); result }
  def pr: Double = math.exp(logpr)
  def vector: scalala.tensor.Vector = throw new Error
}

class MixtureChoice(p:Proportions, value:Int = 0) extends Discrete(p, value) with Gate
trait MixtureOutcome extends GeneratedValue {
  def prFromMixtureComponent(index:Int): Double
}
class MixtureComponentRef[P<:Parameter,C<:MixtureOutcome](p:P, override val child:C) extends ParameterRef(p, child)
class DiscreteMixture(val components:Seq[Proportions], val choice:MixtureChoice, value:Int = 0) extends DiscreteVariable(value) with GeneratedVariable with MixtureOutcome {
  private val proportionsRef = new GatedParameterRef(components, choice, this)
  def proportions = proportionsRef.value
  // proportions.addChild(this)(null) // This is done in GatedParameterRef initialization
  def parents = List(proportions) // Note that 'choice' is not included here because it is not a Parameter
  override def parentRefs = List(proportionsRef, null)
  def pr: Double = proportions.pr(this.intValue)
  def prFromMixtureComponent(index:Int): Double = proportions.pr(index)
  def sampleFrom(p:Proportions)(implicit d:DiffList) = setByIndex(p.sampleInt)
  def sample(implicit d:DiffList): Unit = sampleFrom(proportions)
  def sampleFrom(parents:Seq[Variable])(implicit d:DiffList): Unit = parents match {
    case Seq(p:Proportions) => sampleFrom(p)
  }
}



// Templates
class GeneratedValueTemplate extends TemplateWithStatistics3ss[GeneratedValue,AbstractParameterRef,Parameter] {
  def unroll1(v:GeneratedValue) = Factor(v, v.parentRefs, v.parents)
  def unroll2(r:AbstractParameterRef) = Factor(r.child, r.child.parentRefs, r.child.parents)
  def unroll3(p:Parameter) = p.children.map(v => Factor(v, v.parentRefs, v.parents))
  def score(s:Stat) = s.s1.logpr
}
class MixtureChoiceTemplate extends TemplateWithStatistics1[MixtureChoice] {
  def score(s:Stat) = 0 // s.s1.logpr comes from GeneratedVariableTemplate; gateRefs similarly
  //def score(s:Stat) = { val mc = s.s1; mc.gateRefs.reduceLeft((sum,ref) => sum + mc.value.logpr(ref.outcome)) }
}



object DenseDirichletMAP {
  def estimate(p:Dirichlet, model:Model): Unit = {
    val factors = model.factors(p)
    
  }
}


// LDA, PyMC style
/*
class Word(s:String, ps:Seq[Proportions] = Nil, z:MixtureChoice = null) extends CategoricalMixtureObservation(ps, z, s)
class Document extends ArrayList[Word] { val theta: Proportions }
class Z(p:Proportions[Z]) extends MixtureChoice[Z](p)
// Read data
val nTopics = 10
val topics = repeat(nTopics) new SymmetricDirichlet(Domain[Word].size)(0.01)
for (i <- 0 until document.length) {
  document.theta = new SymmetricDirichlet[Z](1.0)
  for (word <- document) {
    val z = new MixtureChoice(document.theta)
    word ~ (topics, z) // val word = new Word(string, topics, z)
    //word ~ new Word(string, topics, z)
  }
}
*/
