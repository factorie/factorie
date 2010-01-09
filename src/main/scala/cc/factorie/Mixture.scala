/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.{HashSet,HashMap}
import scala.util.Random
import cc.factorie.util.Implicits._


  
/** Trait for any distribution that might be selected as as part of a mixture.
    Since it inherits from ItemizedObservation, the variables themselves are entered into the Domain,
    and this.index is a densely-packed numbering of the distribution instances themselves.
    The number of components in the mixture is the size of the domain.  
    Values of the domain are these MixtureComponent objects.
    Note that this is not a GeneratedCategoricalValue, it is the *categorical value* of a GeneratedCategoricalValue.
    (This is one of those times that the "Value" name is confusing, in that "Value" really means variable.)
    A GeneratedCategoricalValue that has this value is a MixtureChoice. 
    @author Andrew McCallum
*/
trait MixtureComponent[This<:MixtureComponent[This,O] with GenerativeDistribution[O] with ItemizedObservation[This],O<:GeneratedValue[O]] extends GenerativeDistribution[O] with ItemizedObservation[This] {
  this : This =>
  /** Gather together the generated samples from all the MixtureComponents, returning each with its weight associated with this MixtureChoice's MixtureComponent. */
  override def weightedGeneratedSamples: Iterator[(O,Double)] = new Iterator[(O,Double)] {
    val components = domain.elements
    var currentComponent = components.next // TODO perhaps check to make sure that components hasNext
    var elts = currentComponent.generatedSamples.elements
    var nextElt: O = if (elts.hasNext) elts.next else null.asInstanceOf[O]
    def hasNext = nextElt != null
    def next = { 
      val result = nextElt
      if (elts.hasNext) nextElt = elts.next 
      else if (components.hasNext) {
        currentComponent = components.next
        elts = currentComponent.generatedSamples.elements
        if (elts.hasNext) nextElt = elts.next else nextElt = null.asInstanceOf[O]
      } else nextElt = null.asInstanceOf[O]
      (result,weight(result))
    }
    private def weight(o:O) = {
      o.generativeSource match {
        case mc:MarginalizedMixtureChoice[_,O,_] => mc.multinomial(index)
        case mc:MixtureChoice[_,O,_] => if (index == mc.index) 1.0 else 0.0
        case _ => throw new Error("MixtureComponent generatedSample does not have a MixtureChoice generativeSource; it was instead "+o.generativeSource)
      }
    }      
  }
  // The generatedSamples of MixtureComponents are all stored in a central location 
  // shared by all MixtureComponents in the mixture.  Here return just those whose MixtureChoice
  // points at this MixtureComponent
  /*
  override def generatedSamples: Collection[O] = 
    MixtureComponent.generatedSamples[O](this.domain).filter(o => 
      o.generativeSource.asInstanceOf[MixtureChoice[_,O,_]].index == this.index)
  override def weightedGeneratedSamples: Iterator[(O,Double)] = new Iterator[(O,Double)] {
    val elts = MixtureComponent.generatedSamples[O](domain).elements
    var nextElt: O = null.asInstanceOf[O]
    def hasNext = elts.hasNext
    def next = { nextElt = elts.next; (nextElt,weight) }
    private def weight = {
      nextElt.generativeSource match {
        case mc:MarginalizedMixtureChoice[_,O,_] => mc.multinomial(index)
        case mc:MixtureChoice[_,O,_] => if (index == mc.index) 1.0 else 0.0
        case _ => throw new Error("MixtureComponent generatedSample does not have a MixtureChoice generativeSource.")
      }
    }      
  }*/
}

/*object MixtureComponent {
  private val _generatedSamples = new HashMap[Domain[_],HashSet[Variable]] {
    override def default(d:Domain[_]) =  { val result = new HashSet[Variable]; this(d) = result; result }
  }
  def generatedSamples[O](d:Domain[_]) = _generatedSamples(d).asInstanceOf[HashSet[O]]
}*/



/** A multinomial outcome that is an indicator of which mixture component in a MixtureChoice is chosen.
    Its categorical value is a MixtureComponent. 
    The "Z" in Latent Dirichlet Allocation is an example. 
    @author Andrew McCallum
 */  
// Example usage, in LDA: 
// class Topic extends Multinomial[Word] with MixtureComponent[Topic]
// class Z extends MixtureChoice[Topic,Z]; Domain.alias[Z,Topic]
// class Theta extends Multinomial[Z];
// TODO Can I make this:
// MixtureChoice[M<:MixtureComponent[M,_],This<:MixtureChoice[M,M#O,This]]
@DomainInSubclasses
class MixtureChoice[M<:MixtureComponent[M,O],O<:Variable,This<:MixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends GeneratedCategoricalVariable[This] with GenerativeDistributionProxy[M,O] {
  this : This =>
  type VariableType = This
  type ValueType = M
  //def asOutcome = this
  override def asGenerativeDistribution = choice
  def choice: M = domain.get(index)
  // We do the following line in super because "_outcome" will not be set at initialization time.
  Domain.alias[This,M](mt,mm) // We must share the same domain as M; this aliasing might have been done already, but just make sure its done.
  if (domain.size == 0) throw new Error("You must create all the MixtureComponents before creating a MixtureChoice.")
  super.setByIndex(Global.random.nextInt(domain.size))(null) // TODO is this how _index should be initialized?
  // Make sure the defaultModel is prepared to handle this
  if (!Global.defaultModel.contains(MixtureChoiceTemplate)) Global.defaultModel += MixtureChoiceTemplate
  private var _outcome : O = _
  @inline final def outcome : O = _outcome // The particular outcome that was generated from this choice of mixture component
  /** Acting as a GenerativeDistributionProxy, register a sample with the distribution pointed to by this MixtureChoice. 
      This is done in GenerativeDistributionProxy (hence the call to super).  
      Here we also take the opportunity to remember the sample with setOutcome. */
  override def _registerSample(o:O)(implicit d:DiffList): Unit = {
    setOutcome(o)
    super._registerSample(o)
  }
  private def setOutcome(o:O): Unit = if (_outcome == null) _outcome = o else if (o != _outcome) throw new Error("Outcome already set")
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (_outcome == null) throw new Error("No outcome yet set.")
    choice.unregisterSample(outcome)
    super.setByIndex(newIndex) // this changes the value of 'choice'
    choice.registerSample(outcome)
  }
  // example.LDA on 127 documents with 185129 tokens and 17032 types (Users/mccallum/research/data/text/nipstxt/nips05)
  // 9 iterations, printing topics 4 times: 
  // this.sample with this.setByIndex = 118.4 seconds (old version)
  // this.sample with local super.setByIndex = 114.5 seconds
  // this.sample with above and "val dom" = 109.0 seconds
  // this.sample with above and "DirichletMultinomial.pre/postChange" = 108.0 seconds // TODO Didn't help much; consider removing pre/postChange?
  // NOT DEFAULT this.sample with above and keepGeneratedSamples = false = 103.3 seconds // in DirichletMultinomial?
  // NOT DEFAULT this.sample with above and noDiffList = 503.0 seconds.  Fishy!!!!  // TODO Why?????  Investigate!
  // this.sample with above, after Generative infrastructure overhaul = 115.4 seconds
  // this.sample with above, and _setByIndex instead of setByIndex = 111.4 seconds
  // this.sample with above, after Indexed=>Categorical naming overhaul = 451.5 seconds.  Yipes!  What happened?
  // this.sample with above, after caching results of Manifest <:< in GenericSampler = 34 seconds.  Wow!  Much better!! :-)
  // GibbsSampler = 368.3 seconds
  def temperature = 1.0
  override def sample(implicit d:DiffList): Unit = {
    //println("MixtureChoice.sample "+index+" diff "+d)
    //|**("MixtureChoice.sample.prep")
    val src = generativeSource
    // Remove this variable and its sufficient statistics from the model
    //assert(outcome.generativeSource == this)
    choice._unregisterSample(outcome) // We don't call unregisterSample() because it would outcome._setSource(); we want outcome.generativeSource to remain this MixtureChoice 
    src.preChange(this)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    //**|
    //|**("MixtureChoice.sample.sample")
    //val distribution = Array.fromFunction[Double]((i:Int) => src.pr(i) * dom.get(i).unsafePr(outcome))(dom.size)
    //val i = Maths.nextDiscrete(distribution, distribution.foldLeft(0.0)(_+_))(Global.random) // TODO Yipes, I'm seeing BoxedDoubleArray here!
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0
    while (i < size) { 
      distribution(i) = src.asGenerativeDistribution.pr(i) * dom.get(i).pr(outcome)
      if (temperature != 1.0) distribution(i) = Math.pow(distribution(i), 1.0/temperature)
      sum += distribution(i)
      i += 1
    }
    assert(sum > 0.0) // This can happen is the temperature is too low
    // If we are actually a MultinomialDiscrete distribution (integrating out our discrete value) then save the distribution
    this match { case md:MultinomialDiscrete[This] => md.multinomial.set(distribution); case _ => {} }
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    //choice.unsafeGenerate(outcome) // Put outcome back, although, inefficiently, the next line moves it again
    //setByIndex(i - 1) // So, instead we do it ourselves.  But then subclassers cannot meaningfully override setByIndex!! // TODO Consider alternatives
    //**|
    //|**("MixtureChoice.sample.post")
    this._setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    src.postChange(this)
    choice._registerSample(outcome)
    //**|
  }
}


class MarginalizedMixtureChoice[M<:MixtureComponent[M,O],O<:GeneratedVariable[O],This<:MarginalizedMixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends MixtureChoice[M,O,This] with MultinomialCategorical[This] {
  this : This =>
  // Next two methods removed because we will actually register and unregister samples, even if it means we will most of the time simply remove/add to the same MixtureChoice.generatedSample HashSet
  // This way is will be safe to move a sample from a MixtureChoice to a different source. 
  def disabled_registerSample(o:O)(implicit d:DiffList): Unit = {
    if (!(o.generativeSource == null || o.generativeSource == this)) throw new Error("Trying to move a sample to a MixtureChoice not part of its original Domain")
    // If 'o' is already registered assume that it has been registered in all mixture components
    if (choice.generatedSamples.contains(o)) return // TODO remove this so that we throw error if we try to register twice.
    // Register this 'o' with this mixture component's choice
    super._registerSample(o)
  }
  def disabled_unregisterSample(o:O)(implicit d:DiffList): Unit = {
    if (o.generativeSource != this) throw new Error("Trying to remove a sample from a MixtureComponent of which is was not a part.")
    // Do nothing.  The degree of membership of an 'o' in each mixture component is determined by this.multinomial.
    // TODO Note that this does not allow us to change the generativeSource to a GenerativeDistribution outside the mixture once it has been set in the mixture!
  }
  // TODO !!! I'm not sure the next method is correct.
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (outcome == null) throw new Error("No outcome yet set.")
    val src = generativeSource
    choice._unregisterSample(outcome)
    src.preChange(this)
    super._setByIndex(newIndex) // this changes the value of 'choice'
    src.postChange(this)
    choice._registerSample(outcome)
  }
  override def setRandomly(random:Random, d:DiffList): Unit = {
    super.setRandomly(random, d)
    this match { case md:MultinomialDiscrete[This] => md.multinomial.setDirac(this.index); case _ => {} }
  }
  override def setRandomly:Unit = super.setRandomly
  // TODO Are these next two methods correct?
  override def preChange(o:O)(implicit d:DiffList): Unit = Domain[M](mm).foreach(mc => mc.preChange(o))
  override def postChange(o:O)(implicit d:DiffList): Unit = Domain[M](mm).foreach(mc => mc.postChange(o))
  /** Return the weighted average of the pr from each mixture component. */
  override def pr(o:O): Double = { val d = Domain[M](mm); var result = 0.0; for (i <- 0 until d.size) result += multinomial(i) * d.get(i).pr(o); result } 
  override def logpr(o:O): Double = Math.log(pr(o))
  def unused_sample(implicit d:DiffList): Unit = {
    val src = generativeSource
    // Remove this variable and its sufficient statistics from the model
    choice.unregisterSample(outcome)
    //choice.preChange(outcome)
    src.preChange(this)
    val dom = domain // Avoid 'domain' HashMap lookup in inner loop
    val size = dom.size
    val distribution = new Array[Double](size)
    var sum = 0.0
    var i = 0; while (i < size) { distribution(i) = src.asGenerativeDistribution.pr(i) * dom.get(i).pr(outcome); sum += distribution(i); i += 1 }
    // If we are actually a MultinomialDiscrete distribution (integrating out our discrete value) then save the distribution
    this match { case md:MultinomialDiscrete[This] => md.multinomial.set(distribution); case _ => {} }
    i = 0; val r = Global.random.nextDouble * sum; var s = 0.0
    while (s < r && i < size) { s += distribution(i); i += 1 }; i -= 1
    this._setByIndex(i) // change the value of choice
    // Add the variable back into the model, with its new value
    //src.postChange(this) // Could be postChange(this) instead of registerSample(this)
    choice.registerSample(outcome)
    choice.postChange(outcome)
  }
}

//@DomainInSubclasses class MultinomialMixtureChoice[M<:MixtureComponent[AbstractMultinomial[O],O],O<:DiscreteOutcome[O],This<:MixtureChoice[M,O,This]](implicit mm:Manifest[M], mt:Manifest[This]) extends MixtureChoice[M,O,This] { this: This => }
//class DirichletMixtureChoice
//class GaussianMixtureChoice


  
/** A Template for scoring changes to a MixtureChoice. */ 
object MixtureChoiceTemplate extends TemplateWithStatistics1[GenericMixtureChoice] {
  def score(s:Stat) = { val mc = s.s1; mc.logpr + mc.choice.logpr(mc.outcome) } 
}
abstract class GenericGeneratedDiscreteValue extends GeneratedDiscreteValue[GenericGeneratedDiscreteValue] { def intValue = -1 }
// The "2" below is arbitrary, but since this constructor is never called, it shouldn't make any difference
abstract class GenericMixtureComponent extends DenseCountsMultinomial[GenericGeneratedDiscreteValue](2) with MixtureComponent[GenericMixtureComponent,GenericGeneratedDiscreteValue]
abstract class GenericMixtureChoice extends MixtureChoice[GenericMixtureComponent,GenericGeneratedDiscreteValue,GenericMixtureChoice]
