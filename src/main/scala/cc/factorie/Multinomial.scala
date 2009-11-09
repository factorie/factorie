package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

/** Base of the Multinomial class hierarchy, needing only methods length, pr, and set. */
trait AbstractMultinomial[O<:SingleIndexed] extends GenerativeVariable[AbstractMultinomial[O]] with GenerativeDistribution[O] with RandomAccessSeq[Double] {
  type VariableType <: AbstractMultinomial[O];
  //type OutcomeType = O
  type SourceType = AbstractDirichlet[O];
  class DomainInSubclasses
  //type DomainType <: IndexedDomain[OutcomeType]; // No! This might be a IndexedVariable, and have a domain different than its OutcomeType
  //def this(initCounts:Seq[Double]) = { this(initCounts.size); setCounts(initCounts) }
  def length: Int
  def apply(index:Int) = pr(index)
  def set(proportions:Seq[Double]): Unit // TODO include a DiffList here?
  /** Attach and set to the mean of the Dirichlet source 'd'. */ // TODO Too weird?  Remove?  Replace by m ~ d; m.mazimize
  def pr(index:Int) : Double
  //def pr_=(index:Int, p:Double): Unit = _count(index) = countTotal * p
  final def pr(o:OutcomeType) : Double = pr(o.index)
  // I want 'this' to provide this methods; then method 'prs' should be removed.
  //def prs : RandomAccessSeq[Double] = new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = count.size }
  def logpr(index:Int) = Math.log(pr(index))
  def logprIndices(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  def prIndices(indices:Seq[Int]) : Double = indices.foldLeft(1.0)(_*pr(_))
  def pr(outcomes:Seq[OutcomeType]) : Double = prIndices(outcomes.map(_.index))
  def counts: { def apply(i:Int):Double; def size:Int } = {
    val c = new Array[Double](length)
    generatedSamples.foreach(s => c(s.index) += 1.0)
    c
  }
  def sampleInto(o:OutcomeType): Unit = o match { // TODO Consider including implicit DiffList here? 
    case v:SingleIndexedVariable => v.setByIndex(sampleIndex)(null)
    //case _ => throw new Error("Trying to sample into immutable Variable") // TODO Is this OK to just do nothing here?;
    // if so, then we really don't need the separation between GenerativeObservation and GenerativeVariable!...  Or perhaps not?
  }
  class DiscretePr(val index:Int, val pr:Double)
  def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p)})
  def sample(implicit d:DiffList): Unit = { if (source != null) source.sampleInto(this, counts) else throw new Error("Source not set") } 
  def sampleIndex: Int = {
    val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += pr(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  def sampleIndices(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples force) yield sampleIndex
  def estimate: Unit = { throw new Error("Not yet implemented")} // TODO What to put here?
}

/** Compact representation of immutable Multinomial with equal probability for all outcomes. */
class UniformMultinomial[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) extends AbstractMultinomial[O] {
  val length: Int = Domain[O](m).size
  val pr1 = 1.0/length
  def pr(index:Int) = pr1
  def set(proportions:Seq[Double]): Unit = throw new Error("UniformMultinomial cannot be changed.")
}

/** Simple Multinomial that represents p(x) directly. */
class DenseMultinomial[O<:MultinomialOutcome[O]](proportions:Seq[Double])(implicit m:Manifest[O]) extends AbstractMultinomial[O] {
  val length: Int = Domain[O](m).size
  val _pr = new DenseVector(length)
  if (proportions != null) set(proportions)
  /** Will normalize for you, if not already normalized. */
  def set(props:Seq[Double]): Unit = {
    assert (proportions.length == length)
    val sum = proportions.foldLeft(0.0)(_+_)
    for (i <- 0 until length) _pr(i) = proportions(i)/sum
  }
  final def pr(index:Int) = _pr(index)
}

/** A Multinomial that stores its parameters as a collection of "outcome counts" and their total. */
trait CountsMultinomial[O<:SingleIndexed] extends AbstractMultinomial[O] {
  type VariableType <: CountsMultinomial[O];
  class DomainInSubclasses
  def length: Int = counts.size 
  protected var total : Double = 0.0
  def countsTotal = total
  protected val _counts : Vector
  override def counts: { def apply(i:Int):Double; def update(i:Int,v:Double):Unit; def size:Int } = _counts // TODO Want Seq[Double], but Vector doesn't mixin Seq?!!
  def increment(index:Int, incr:Double)(implicit d:DiffList) = { // TODO Scala 2.8 add incr:Double=1.0
    _counts(index) += incr; total += incr
    if (_counts(index) < 0.0) println("CountsMultinomial counts<0 "+this.getClass.getName)
    assert(_counts(index) >= 0.0)
    assert(total >= 0.0)
    if (d != null) d += new CountsMultinomialIncrementDiff(index, incr)
  }
  def increment(o:O, incr:Double)(implicit d:DiffList): Unit = increment(o.index, incr)
  def set(c:Seq[Double]) : Unit = { // TODO Add DiffList here?
    assert(c.length == _counts.size)
    total = 0.0
    var i = 0; c.foreach(x => {_counts(i) = x; total += x; i += 1}) 
  }
  def pr(index:Int) : Double = {
    //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
    if (/*false &&*/ source != null)
      (counts(index) + source.alpha(index)) / (countsTotal + source.sum)
    else if (countsTotal == 0)
      1.0 / size
    else
      counts(index) / countsTotal
  }
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * countsTotal; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += counts(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  override def estimate: Unit = { // TODO Think about how this might make SparseCountsMultinomial unsparse, and how to improve
    if (generatedSamples.isEmpty) throw new Error("No generated samples from which to estimate")
    if (source == null) { _counts.zero; total = 0.0 }
    else { _counts := source.alphas; total = source.sum }
    generatedSamples.foreach(s => { _counts(s.index) += 1.0; total += 1.0 })
  }
  class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))})
  case class CountsMultinomialIncrementDiff(index:Int, incr:Double) extends Diff {
    def variable = CountsMultinomial.this
    def undo = { _counts(index) -= incr; total -= incr }
    def redo = { _counts(index) += incr; total += incr }
  }
}

class SparseCountsMultinomial[O<:SingleIndexed](dim:Int) extends CountsMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: SparseCountsMultinomial[O]
  class DomainInSubclasses
  protected val _counts = new SparseVector(dim)
  def default = _counts.default
  def default_=(d:Double) = _counts.default = d
}

class DenseCountsMultinomial[O<:SingleIndexed](dim:Int) extends CountsMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); set(initCounts) }
  type VariableType <: DenseCountsMultinomial[O]
  class DomainInSubclasses
  protected val _counts = new DenseVector(dim)
}

//class Multinomial[O<:MultinomialOutcome[O]](initCounts:Seq[Double])(implicit m:Manifest[O]) extends DenseMultinomial[O](initCounts) with Iterable[O#VariableType#ValueType]
class DirichletMultinomial[O<:SingleIndexed](dirichlet:AbstractDirichlet[O])(implicit m:Manifest[O]) extends DenseCountsMultinomial[O](Domain[O](m).size) {
  def this()(implicit m:Manifest[O]) = this(null.asInstanceOf[AbstractDirichlet[O]])(m)
  def this(dirichlet:AbstractDirichlet[O], initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(dirichlet)(m); set(initCounts) }
  def this(initCounts:Seq[Double])(implicit m:Manifest[O]) = { this(null.asInstanceOf[AbstractDirichlet[O]])(m); set(initCounts) }
  type VariableType <: DirichletMultinomial[O];
  class DomainInSubclasses
  val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance; but otherwise 'm' would be stored for each instance anyway?
  override def pr(index:Int) : Double = {
    //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
    if (source != null)
      (counts(index) + source.alpha(index)) / (countsTotal + source.sum)
    else if (countsTotal == 0)
      1.0 / size
    else
      counts(index) / countsTotal
  }
  override def sampleIndex: Int = { // TODO More efficient because it avoids the normalization in this.pr
    val s = Global.random.nextDouble * (countsTotal + source.sum); var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += counts(i) + source.alpha(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  // Probability of a collection of counts; see http://en.wikipedia.org/wiki/Multivariate_Polya_distribution
  def pr(ocounts:Vector) : Double = {
    import Maths.{gamma => g}
    assert (ocounts.size == length)
    val n = norm(ocounts,1)
    val normalizer1 = g(n) / ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2))
    val normalizer2 = g(source.sum) / g(n+source.sum)
    val ratio = ocounts.elements.foldLeft(1.0)((p,e) => p * g(e._2+source.alpha(e._1)/g(source.alpha(e._1))))
    normalizer1 * normalizer2 * ratio
  }
  def logpr(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  /** Set source, and incrementally update the parameters of this Multinomial */
  //def ~:(o:O) : this.type = { o.setSource(this)(null); increment(o); this }
  override def generate[O2<:O](o:O2)(implicit d:DiffList) = { 
    //println("Multinomial.outcomeDomain.size="+outcomeDomain.size+" generate "+o+" size="+size); Console.flush;
    super.generate(o)
    increment(o.index, 1.0)
  }
  override def ungenerate[O2<:O](o:O2)(implicit d:DiffList) = {
    super.ungenerate(o)
    increment(o.index, -1.0)
  }
  override def preChange[O2<:O](o:O2)(implicit d:DiffList) = {
    increment(o.index, -1.0)
  }
  override def postChange[O2<:O](o:O2)(implicit d:DiffList) = {
    increment(o.index, 1.0)
  }
  def sampleValue: O#VariableType#ValueType = outcomeDomain.get(sampleIndex)
  override def estimate: Unit = {} // Nothing to do because estimated on the fly
	class DiscretePr(override val index:Int, override val pr:Double, override val count:Double, val value:O#VariableType#ValueType) extends super.DiscretePr(index,pr,count)
  override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i),outcomeDomain.get(i))})
  def topValues(n:Int) = top(n).toList.map(_.value) // TODO change name to topValues
  override def toString = "Multinomial(count="+total+")"
}

    
// TODO Consider renaming this MultinomialSample, because the instances of this class are individual samples (e.g. token)?
// "outcome" may indicate the value (e.g. type)
// No, I think I like "MultinomialOutcome"
// Consider merging MultinomialOutcome and MultinomialOutcomeVariable?  No, because not everything we want to generate has a "setByIndex" to override
trait MultinomialOutcome[This<:MultinomialOutcome[This] with SingleIndexed with GenerativeObservation[This]] extends SingleIndexed with GenerativeObservation[This] {
	this: This =>  
  type SourceType = AbstractMultinomial[This]
  class DomainInSubclasses
}

/** A MultinomialOutcome that is also a SingleIndexedVariable */
trait MultinomialOutcomeVariable[This<:MultinomialOutcomeVariable[This] with SingleIndexedVariable] extends SingleIndexedVariable with MultinomialOutcome[This] with GenerativeVariable[This] {
  this : This =>
  override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
    if (source != null) source.ungenerate(this)
    super.setByIndex(newIndex)
    if (source != null) source.generate(this)
  }
  def distribution: Array[Double]= {
    val buffer = new Array[Double](domain.size);
    for (i <- 0 until buffer.length) buffer(i) = source.pr(i); 
    buffer
  }
  def sample(implicit d:DiffList): Unit = {
    val isDM = classOf[DirichletMultinomial[This]].isAssignableFrom(getClass)
    if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, -1)
    setByIndex(source.sampleIndex)
    if (isDM) source.asInstanceOf[DirichletMultinomial[This]].increment(this, 1)
  }
}
  
/** The outcome of a coin flip, with boolean value.  this.value:Boolean */
class Flip extends CoordinatedBool with MultinomialOutcomeVariable[Flip]
case class Coin(p:Double, totalCount:Double) extends DenseCountsMultinomial[Flip](Array((1-p)*totalCount,p*totalCount)) {
  def this(p:Double) = this(p:Double, 1.0)
  def this() = this(0.5)
  assert (p >= 0.0 && p <= 1.0)
  def flip : Flip = { val f = new Flip; f.setByIndex(this.sampleIndex)(null); f }
  def flip(n:Int) : Seq[Flip] = for (i <- 0 until n force) yield flip
  def pr(f:Boolean) : Double = if (f) pr(1) else pr(0)
}
object Coin { 
  def apply(p:Double) = new Coin(p)
}
