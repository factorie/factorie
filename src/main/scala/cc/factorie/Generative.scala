package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._

// A collection of Variables and Factors relevant to generative models

// TODO Think more carefully about the hierarchy of Variables that are parameters 
trait GenerativeDistribution extends Variable {
  type OutcomeType <: Variable
	def estimate : Unit
	// This odd arg type is due to:
	// http://www.nabble.com/Fwd:--lift--Lift-with-Scala-2.6.1--td14571698.html	
	def generate[O<:OutcomeType](o:O)(implicit d:DiffList)
	def ungenerate[O<:OutcomeType](o:O)(implicit d:DiffList)
}

// Consider using DenseVector instead of Array[Double] everywhere in this file

trait AbstractDirichlet[O<:MultinomialOutcome[O]] extends GenerativeDistribution {
  type OutcomeType = Multinomial[O]
	def alpha(index:Int) : Double
	def sum : Double
	def mean(index:Int) : Double
  def apply(index:Int) = alpha(index)
	def generate(o:Multinomial[O])(implicit d:DiffList) : Unit
	def ungenerate(o:Multinomial[O])(implicit d:DiffList) : Unit
	def sample : Multinomial[O];
  //def sample(n:Int) : Seq[Multinomial[O]] = for (i <- 0 until n) yield sample
	def sampleInto(ret:Multinomial[O]) : Unit = {
	  //println("sampleInto")
	  var norm = 0.0
		for (val i <- 0 until ret.counts.length) {
			ret.counts(i) = Maths.nextGamma(alpha(i), 1)(Global.random)
			if (ret.counts(i) <= 0) ret.counts(i) = 0.0001
			norm += ret.counts(i)
		}
	  norm /= sum
		for (i <- 0 until ret.counts.length) {
		  ret.counts(i) /= norm
		  //if (ret.source != null) ret.counts(i) -= ret.source.alpha(i)
    }
		ret.total = sum
	}
}


// Make a general class Dirichlet to cover both constant and estimated varieties
abstract class Dirichlet[O<:MultinomialOutcome[O]](initialAlpha:Seq[Double])(implicit m:Manifest[O]) extends AbstractDirichlet[O] with Parameter {
	//println("Dirichlet")
  def this(initialAlpha:Double)(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).allocSize, initialAlpha))(m)
	type VariableType <: Dirichlet[O];
	class DomainInSubclasses
	val outcomeDomain = Domain[O](m) // TODO unfortunately this gets repeatedly set
	assert(initialAlpha.length == outcomeDomain.allocSize)
 	val generatedSamples = new HashSet[Multinomial[O]];
	def generate(o:Multinomial[O])(implicit d:DiffList) = 
   if (generatedSamples.contains(o)) throw new Error("Already generated Multinomial "+o) else generatedSamples += o
   // TODO Add Handling of DiffList!!!!
	def ungenerate(o:Multinomial[O])(implicit d:DiffList) = generatedSamples -= o
   // TODO Add Handling of DiffList!!!!
	protected val _mean: Array[Double] = { 
	  val a = new Array[Double](initialAlpha.length); val sum = initialAlpha.foldLeft(0.0)(_+_) 
	  for (i <- 0 until a.length) a(i) = initialAlpha(i) / sum
	  a
	} 
	def mean(i:Int) = _mean(i)
	def alpha(i:Int) = _mean(i) * _sum
	var _sum = initialAlpha.foldLeft(0.0)(_+_)
	def sum = _sum
 	def sample = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
}
  
trait DirichletMomentMatchingEstimator[O<:MultinomialOutcome[O]] {
  this : Dirichlet[O] =>
	def estimate : Unit = {
		// TODO this would be much easier if Multinomial and Dirichlet used scalala; I should convert eventually
	  for (i <- 0 until _mean.length) _mean(i) = 0.0
	  val variance = new Array[Double](_mean.length)
	  for (m <- generatedSamples; i <- 0 until _mean.length) _mean(i) += m.counts(i)/m.total
	  for (i <- 0 until _mean.length) _mean(i) /= generatedSamples.size
		assert (Maths.almostEquals(1.0, _mean.foldLeft(0.0)(_+_)))
		println("mean "+_mean.take(10).toList)
		// Calculate variance = E[x^2] - E[x]^2 for each dimension
	  for (m <- generatedSamples; i <- 0 until _mean.length) {
	    val p = m.counts(i)/m.total
	    variance(i) += p * p
	  }
		for (i <- 0 until _mean.length) {
		  val a = _mean(i)
		  variance(i) = (variance(i) / (generatedSamples.size - 1.0)) - a*a
		}
		//for (i <- 0 until _mean.length) variance(i) /= (generatedSamples.size - 1)
		println("variance "+variance.take(10).toList)
	  _sum = 0.0
	  for (i <- 0 until _mean.length) 
	  	if (_mean(i) != 0.0) _sum += Math.log((_mean(i) * (1.0 - _mean(i)) / variance(i)) - 1.0)
	  	// sum += Math.log(( partition[bin] * ( 1 - partition[bin] ) / variances[bin] ) - 1);
	  _sum = Math.exp(_sum / (_mean.length - 1))
	}
}

// Does not have its own Domain.  Size of alpha is Domain of O
class SymmetricDirichlet[O<:MultinomialOutcome[O]](initialAlpha:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] {
	type VariableType <: Dirichlet[O];
	class DomainInSubclasses
	type OutcomeDomainType = O
	val outcomeDomain = Domain[O](m)
	protected val meanAlpha = 1.0 / outcomeDomain.size 
	def alpha(index:Int) = initialAlpha
	def mean(index:Int) = meanAlpha
	lazy val sum = initialAlpha * outcomeDomain.size
	def generate(o:Multinomial[O])(implicit d:DiffList) = {}
	def ungenerate(o:Multinomial[O])(implicit d:DiffList) = {}
 	def sample = { val mul = new Multinomial[O]()(m); sampleInto(mul); mul }
}

// Does not have its own Domain.  Size of pr is Domain of O
// TODO should this Iterate over [O] or over [O#VariableType#ValueType] ???
class Multinomial[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) extends Variable with GenerativeDistribution with Iterable[O#VariableType#ValueType] {
	type OutcomeType = O
  type VariableType <: Multinomial[O];
	class DomainInSubclasses
	type OutcomeDomainType = O // TODO No longer necessary, I think
	val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance
	lazy val counts = new Array[Double](outcomeDomain.allocSize)
	var total : Double = 0.0
	def size = counts.length
	var source : AbstractDirichlet[O] = _
	def setSource(dir:AbstractDirichlet[O])(implicit d:DiffList) : Unit = {
		if (d != null) d += MultinomialSetSourceDiff(source, dir)
		if (source != null) source.ungenerate(this) 
		source = dir
		source.generate(this)
	}
	def ~(d:AbstractDirichlet[O]) : this.type /*Multinomial[O]*/ = { setSource(d)(null); this }
	def :~(d:AbstractDirichlet[O]) : this.type = { setSource(d)(null); d.sampleInto(this); this }
	def :=~(d:AbstractDirichlet[O]) : this.type = { 
	  setSource(d)(null)
	  for (i <- 0 until counts.length) counts(i) = d.alpha(i)
	  total = d.sum
	  this
	}
	def generate(o:O)(implicit d:DiffList) = { 
		//println("Multinomial.outcomeDomain.size="+outcomeDomain.size+" generate "+o+" size="+size); Console.flush; 
		counts(o.index) += 1.0; total += 1.0
		if (d != null) d += MultinomialGenerateDiff(o.index)
	}
	def ungenerate(o:O)(implicit d:DiffList) = { 
		counts(o.index) -= 1.0; assert(counts(o.index) >= 0.0)
		total -= 1.0; assert(total >= 0.0)
		if (d != null) d += MultinomialUngenerateDiff(o.index)
	}
	def estimate : Unit = {} // Nothing to be done; constantly keeps itself estimated
	def nextOutcomeValue : O#VariableType#ValueType = outcomeDomain.get(nextSample) 
	def nextSample : Int = {
		val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = outcomeDomain.size
		while (i < size) {
			sum += pr(i)
			if (sum >= s) return i
		}
		return size - 1
	}
	def elements : Iterator[O#VariableType#ValueType] = new Iterator[O#VariableType#ValueType] {
		def hasNext = true
		def next = nextOutcomeValue
	}
	def pr(index:Int) : Double = {
		//println("Multinomial.pr "+counts(index)+" "+source(index)+" "+total+" "+source.sum)
		if (/*false &&*/ source != null)
			(counts(index) + source.alpha(index)) / (total + source.sum)
		else
			counts(index) / total
		}
	def pr(o:O#VariableType) : Double = pr(o.index)
	def logpr(index:Int) = Math.log(pr(index))
	def logpr(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
	def top(n:Int) = counts.zipWithIndex.sortReverse({case (c,i)=>c}).take(n).toList.map({case (c,i)=>(outcomeDomain.get(i),i,pr(i),c)})
	override def toString = "Multinomial(count="+total+")"
	case class MultinomialGenerateDiff(i:Int) extends Diff {
		def variable = Multinomial.this
		def redo = { counts(i) += 1.0; total += 1.0 }
		def undo = { counts(i) -= 1.0; total -= 1.0 }
	}
	case class MultinomialUngenerateDiff(i:Int) extends Diff {
		def variable = Multinomial.this
		def redo = { counts(i) -= 1.0; total -= 1.0 }
		def undo = { counts(i) += 1.0; total += 1.0 }
	}
	case class MultinomialSetSourceDiff(oldSource:AbstractDirichlet[O], newSource:AbstractDirichlet[O]) extends Diff {
		def variable = Multinomial.this
		def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
		def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
	}
}

  
// Trait for any distribution that might be picked as part of a Multinomial mixture.
// Creates is own Domain.  Number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponents.
// Note that this is not a MultinomialOutcome, it is the *value* of MultinomialOutcome.
// The MultinomialOutcome is MixtureChoice
// class Theta extends Multinomial[Z];
// class Topic extends Multinomial[Word] with MixtureComponent[Topic];
// class Z extends MultinomialMixtureChoice[Topic,Theta,Word];
// class Z extends MultinomialMixtureChoice[Topic,Z];
// class Word(s:String) extends EnumVariable(s) with MultinomialOutcome[Word] with MultinomialSource[Topic] // optionally to know the type of the generating Multinomial
trait MixtureComponent[This<:MixtureComponent[This] with GenerativeDistribution] extends TypedSingleIndexedVariable[This] with GenerativeDistribution {
	this : This =>
	type VariableType = This  // can we get away with this = ?
	//type DomainType = IndexedDomain[VariableType] // TODO Why is this necessary?
	//override type ValueType = This
	//override final def domain : Domain[This] = super.domain // TODO I think this line can be commented out???
	//def domain : Domain[This] = Domain[This].get(this.getClass)
	_index = domain.asInstanceOf[IndexedDomain[This]].index(this) // TODO can we avoid this cast?
	//println("Creating MixtureComponent this.getClass = "+this.getClass.toString+" index="+_index+" domain size="+domain.asInstanceOf[IndexedDomain[This]].size)
	override final def setByIndex(index:Int)(implicit d:DiffList) : Unit = new Error
}


// Usage: 
// class Topic extends Multinomial[Word] with MixtureComponent[Topic]
// class Z extends MixtureChoice[Topic,Z]
// class Theta extends Multinomial[Z]
trait MixtureChoice[M<:MixtureComponent[M],This<:MixtureChoice[M,This]] extends MultinomialOutcome[This] {
  this : This =>
  type VariableType = This
  type ValueType = M
  class DomainInSubclasses
  def choice : M = domain.get(index)
  _index = Global.random.nextInt(domain.size) // TODO is this how _index should be initialized?
	private var _outcome : M#OutcomeType = _
	def outcome : M#OutcomeType = _outcome // The particular outcome that was generated from this choice of mixture component
	def setOutcome(o:M#OutcomeType) = 
		if (_outcome == null) _outcome = o
		else throw new Error
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
	  if (_outcome == null) throw new Error("No outcome yet set.")
	  choice.ungenerate(outcome)
	  super.setByIndex(newIndex)
	  choice.generate(outcome)
	}
}
  
  
// Order of type arguments: M==The distribution being selected, M==The multinomial distribution from which the selection is made
class MultinomialMixtureChoice[M<:Multinomial[O] with MixtureComponent[M],O<:MultinomialOutcome[O],This<:MultinomialMixtureChoice[M,O,This]](implicit mm:Manifest[M/*#OutcomeDomainType*/]) extends /* TODO Typed... so def value works */ MultinomialOutcome[This] {
	//type VariableType = MultinomialMixtureChoice[C,This];
	this : This =>
	type VariableType = This // TODO is this right?
	type ValueType = M
	class DomainInSubclasses
	//override type OutcomeDomainType = M
	//println("new MultinomialMixtureChoice "+this.getClass.getName+" and manifold "+mm+" domain.size="+domain.size)
	//println("new MultinomialMixtureChoice "+this.getClass.getName+" Domain[Z].size="+Domain[Z].size)
	def multinomial : M = domain.get(index)
	_index = Global.random.nextInt(domain.size) // TODO is this how _index should be initialized?
	if (!Global.defaultModel.contains(MultinomialMixtureChoiceTemplate))
		Global.defaultModel += MultinomialMixtureChoiceTemplate
	private var _outcome : O = _
	def outcome : O = _outcome
	def setOutcome(o:O) = 
		if (_outcome != null) throw new Error
		else { _outcome = o } 
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
	  if (_outcome == null) throw new Error("No outcome yet set.")
	  multinomial.ungenerate(outcome)
	  super.setByIndex(newIndex)
	  multinomial.generate(outcome)
	}
	def ~~~(m:Multinomial[This]) : this.type = { // TODO Remove this
		//_index = m.nextSample
		/*super.*/setSource(m)(null)
		this // this.asInstanceOf[MultinomialChoice[M,O]]
	}
}
    
// TODO Consider renaming this MultinomialSample, because the instances of this class are idividual samples (e.g. token).  "outcome" may indicate the value (e.g. type) 
trait MultinomialOutcome[This<:MultinomialOutcome[This]] extends SingleIndexedVariable {
	this : This =>
	class DomainInSubclasses
	type OutcomeDomainType = This // TODO No longer necessary, I think
	var source : Multinomial[This] = _
	def setSource(m:Multinomial[This])(implicit d:DiffList) : Unit = {
		if (m == null) throw new IllegalArgumentException("MultinomialOutcome cannot have null source")
		if (d != null) d += new MultinomialOutcomeSourceChangeDiff(source, m)
		if (source != null) source.ungenerate(this)
		source = m
		//println("Multinomial Outcome setSource on outcome "+this+" index="+index)
		source.generate(this)
	}
	def ~(m:Multinomial[This]) : this.type = {
		setSource(m)(null); 
     this 
	}
	def ~[M<:Multinomial[This]](mmc:MultinomialMixtureChoice[M,This,_]) : this.type = {
		mmc.setOutcome(this); 
		this.~(mmc.multinomial) // either here or in mmc.setOutcome; not sure which is more natural
	}
	/** Attach to my generator, and also set my value to something sampled from the generator. */
	def :~(m:Multinomial[This]) : this.type = {
		_index = m.nextSample // TODO Do this instead by setByIndex()()?  But causes some problems...
		this.~(m)
	}
	def :~[M<:Multinomial[This]](mmc:MultinomialMixtureChoice[M,This,_]) : this.type = {
		_index = mmc.multinomial.nextSample // TODO Do this instead by setByIndex()()?  But causes some problems...
		this.~(mmc)
	}
	//override def toString = "MultinomialOutcome(" + domain.get(_index).toString + "=" + _index + ")"
	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
		if (source != null) source.ungenerate(this)
		super.setByIndex(newIndex)
		if (source != null) source.generate(this)
	}
	case class MultinomialOutcomeSourceChangeDiff(oldSource:Multinomial[This], newSource:Multinomial[This]) extends Diff {
		def variable = MultinomialOutcome.this
		def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
		def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
	}
}
  
/** The outcome of a coin flip, with boolean value.  this.value:Boolean */
case class Flip extends Bool with MultinomialOutcome[Flip]
case class Coin(p:Double, dirichletVariance:Double) extends Multinomial[Flip] {
	def this(p:Double) = this(p:Double, 1.0)
	assert (p >= 0.0 && p <= 1.0)
	this.counts(0) = (1.0-p)/dirichletVariance
	this.counts(1) = p/dirichletVariance
}

class GenericMultinomialOutcome extends MultinomialOutcome[GenericMultinomialOutcome]
class GenericMultinomial extends Multinomial[GenericMultinomialOutcome] with MixtureComponent[GenericMultinomial]
class GenericMultinomialMixtureComponent extends MultinomialMixtureChoice[GenericMultinomial,GenericMultinomialOutcome,GenericMultinomialMixtureComponent]
//class MultinomialMixtureChoiceTemplate[This<:MultinomialMixtureChoice[_<:Multinomial[_<:MultinomialOutcome[_]],_<:MultinomialOutcome[_],_]](implicit m:Manifest[This]) extends TemplateWithStatistics1[This]()(m) {
//class MultinomialMixtureChoiceTemplate extends TemplateWithStatistics1[MultinomialMixtureChoice[GenericMultinomial,GenericOutcome,MultinomialMixtureChoice[_,_,_]]]()(Manifest.classType[MultinomialMixtureChoice[GenericMultinomial,GenericOutcome,_]]) {
object MultinomialMixtureChoiceTemplate extends TemplateWithStatistics1[MultinomialMixtureChoice[GenericMultinomial,GenericMultinomialOutcome,GenericMultinomialMixtureComponent]] {
  //val mmcc = classOf[MultinomialMixtureChoice[_,_,_]]; println("Template class="+nc1)
  def score(s:Stat) = {
		val mmc = s.s1
		mmc.multinomial.logpr(s.s1.outcome.index) + mmc.source.logpr(mmc.index)
	}
}

