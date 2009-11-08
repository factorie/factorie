package cc.factorie

import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}


// Simplify this; change AbstractMultinomial to remove counts
class UniformMultinomial[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) extends DenseMultinomial[O](Domain[O](m).size)


// AnyRef avoids type overlap between pr(Seq[Int]) and pr(Seq[OutcomeType])
trait AbstractMultinomial[O<:SingleIndexed] extends GenerativeVariable[AbstractMultinomial[O]] with GenerativeDistribution[O] with RandomAccessSeq[Double] {
  type VariableType <: AbstractMultinomial[O];
  //type OutcomeType = O
  type SourceType = AbstractDirichlet[O];
  class DomainInSubclasses
  //type DomainType <: IndexedDomain[OutcomeType]; // No! This might be a IndexedVariable, and have a domain different than its OutcomeType
  //def this(initCounts:Seq[Double]) = { this(initCounts.size); setCounts(initCounts) }
  def countTotal : Double
  def count : { def apply(i:Int):Double; def update(i:Int,v:Double):Unit; def size:Int } // Seq[Double]
  def setCounts(c:Seq[Double]) : Unit 
  def increment(index:Int): Unit
  def unincrement(index:Int): Unit
  def increment(o:O): Unit = increment(o.index)
  def unincrement(o:O): Unit = unincrement(o.index)
  def length = count.size
  def apply(index:Int) = pr(index)
  def ~[D<:AbstractDirichlet[O]](mc:MixtureChoice[D,_]) : this.type = {
    mc.setOutcome(this); 
    this.~(mc.choice) // either here or in mmc.setOutcome; not sure which is more natural
  }
  def :=~(d:AbstractDirichlet[O]) : this.type = {
    this.~(d)
    setCounts(d.alphas)
    this
  }
  def pr: Double = if (source != null) source.pr(this) else throw new Error("Multinomial source not set.")
  def pr(index:Int) : Double = {
    //println("Multinomial.pr "+count(index)+" "+source(index)+" "+total+" "+source.sum)
    if (/*false &&*/ source != null)
      (count(index) + source.alpha(index)) / (countTotal + source.sum)
    else if (countTotal == 0)
      1.0 / size
    else
      count(index) / countTotal
  }
  final def pr(o:OutcomeType) : Double = pr(o.index)
  // I want 'this' to provide this methods; then method 'prs' should be removed.
  def prs : RandomAccessSeq[Double] = new RandomAccessSeq[Double] { def apply(i:Int) = pr(i); def length = count.size }
  final def logpr(o:OutcomeType) : Double = Math.log(pr(o))
  def logpr(index:Int) = Math.log(pr(index))
  def logpr(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
  def pr(indices:Seq[Int]) : Double = indices.foldLeft(1.0)(_*pr(_))
  def prOutcomes(outcomes:Seq[OutcomeType]) : Double = outcomes.foldLeft(1.0)(_*pr(_))
  class DiscretePr(val index:Int, val pr:Double, val count:Double)
  def top(n:Int): Seq[DiscretePr] = prs.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,count(i))})
  def sample(implicit d:DiffList): Unit = { if (source != null) source.sampleInto(this) else throw new Error("Source not set"); return null } 
  def sampleValue : Int = {
    val s = Global.random.nextDouble; var sum = 0.0; var i = 0; val size = this.size
    while (i < size) {
      sum += pr(i)
      if (sum >= s) return i
      i += 1
    }
    return size - 1
  }
  def estimate = {}
}

class DenseMultinomial[O<:SingleIndexed](dim:Int) extends AbstractMultinomial[O] {
  def this(initCounts:Seq[Double]) = { this(initCounts.size); setCounts(initCounts) }
  type VariableType <: DenseMultinomial[O]
  class DomainInSubclasses
  protected val _counts = new DenseVector(dim)
  def count = _counts // TODO But doesn't protect others from changing _counts' values
  protected var total : Double = 0.0
  def countTotal = total
  def setCounts(c:Seq[Double]) : Unit = {
    assert(c.length == _counts.size)
    total = 0.0
    var i = 0
    c.foreach(x => {_counts(i) = x; total += x; i += 1}) 
  }
  def increment(index:Int) = { _counts(index) += 1.0; total += 1.0 }
  def unincrement(index:Int) = { _counts(index) -= 1.0; total -= 1.0 }
}


// Does not have its own Domain.  Size of pr is Domain of O
// TODO should this Iterate over [O] or over [O#VariableType#ValueType] ???  Neither.  It should provide Seq[pr] not Iterable[samples]
//class Multinomial[O<:MultinomialOutcome[O]](initCounts:Seq[Double])(implicit m:Manifest[O]) extends DenseMultinomial[O](initCounts) with Iterable[O#VariableType#ValueType]
class Multinomial[O<:SingleIndexed](initCounts:Seq[Double])(implicit m:Manifest[O]) extends DenseMultinomial[O](initCounts) {
  def this()(implicit m:Manifest[O]) = this(null)(m)
  //override type OutcomeType = O
  type VariableType <: Multinomial[O];
  class DomainInSubclasses
  val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance; but otherwise 'm' would be stored for each instance anyway?

  /** Set source, and incrementally update the parameters of this Multinomial */
  //def ~:(o:O) : this.type = { o.setSource(this)(null); increment(o); this }
  override def generate(o:O)(implicit d:DiffList) = { 
    //println("Multinomial.outcomeDomain.size="+outcomeDomain.size+" generate "+o+" size="+size); Console.flush; 
    _counts(o.index) += 1.0; total += 1.0
    if (d != null) d += MultinomialGenerateDiff(o.index)
  }
  override def ungenerate(o:O)(implicit d:DiffList) = { 
    _counts(o.index) -= 1.0; assert(_counts(o.index) >= 0.0)
    total -= 1.0; assert(total >= 0.0)
    if (d != null) d += MultinomialUngenerateDiff(o.index)
  }
  def nextOutcomeValue : O#VariableType#ValueType = outcomeDomain.get(sampleValue) 
  def sample(numSamples:Int) : Seq[Int] = for (i <- 0 until numSamples force) yield sampleValue
	class DiscretePr(override val index:Int, override val pr:Double, override val count:Double, val value:O#VariableType#ValueType) extends super.DiscretePr(index,pr,count)
  override def top(n:Int): Seq[DiscretePr] = prs.toArray.zipWithIndex.sortReverse({case (p,i)=>p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,count(i),outcomeDomain.get(i))})
  def topValues(n:Int) = top(n).toList.map(_.value) // TODO change name to topValues

  override def toString = "Multinomial(count="+total+")"
  case class MultinomialGenerateDiff(i:Int) extends Diff {
    def variable = Multinomial.this
    def redo = { _counts(i) += 1.0; total += 1.0 }
    def undo = { _counts(i) -= 1.0; total -= 1.0 }
  }
  case class MultinomialUngenerateDiff(i:Int) extends Diff {
    def variable = Multinomial.this
    def redo = { _counts(i) -= 1.0; total -= 1.0 }
    def undo = { _counts(i) += 1.0; total += 1.0 }
  }
}
