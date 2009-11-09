package cc.factorie
import scala.reflect.Manifest
import scala.collection.mutable.HashSet
import cc.factorie.util.Implicits._
import scalala.Scalala._
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}

// Consider using DenseVector instead of Array[Double] everywhere in this file

//trait AbstractDirichlet[O<:MultinomialOutcome[O]] extends GenerativeDistribution/*[Multinomial[O]]*/
trait AbstractDirichlet[O<:SingleIndexed] extends GenerativeDistribution[AbstractMultinomial[O]] {
  //type OutcomeType = AbstractMultinomial[O]
  def size : Int
  def alpha(index:Int) : Double
  def alphas : Seq[Double] = for (i <- 0 until size) yield alpha(i)
  def sum : Double // TODO Consider renaming to alphaSum!
  def mean(index:Int) : Double
  def apply(index:Int) = alpha(index)
  def sampleOutcome : OutcomeType
  def estimate : Unit = throw new Error("Method estimate is not implemented in this class.  You must add a trait for estimation.")
  def sampleOutcomes(n:Int) : Seq[OutcomeType] = for (i <- 0 until n force) yield sampleOutcome
  def sampleInto(m:OutcomeType): Unit = sampleInto(m:OutcomeType, SparseVector(size)(0.0)) // Waiting Scala 2.8 default args 
  def sampleInto(m:OutcomeType, counts:{def apply(i:Int):Double; def size:Int}): Unit = {
    //println("sampleInto")
    var norm = 0.0
    val c = new Array[Double](m.size)
    for (val i <- 0 until c.length) {
      //println("sampleInto alpha(i)="+alpha(i))
      c(i) = Maths.nextGamma(alpha(i)+counts(i), 1)(Global.random)
      if (c(i) <= 0.0) c(i) = 0.0001
      norm += c(i)
    }
    norm /= sum
    for (i <- 0 until c.length) {
      c(i) /= norm
      //println("sampleInto c(i)="+c(i))
      //if (ret.source != null) ret.counts(i) -= ret.source.alpha(i) // TODO consider putting this back!  Because now ret.pr(i) != c(i) !!!
      // coordinate with Dirichlet parameter estimation
    }
    m.set(c)
  }
  //def pr[O2<:OutcomeType](m:O2) : Double = throw new Error("Not yet implemented")
  def pr(m:OutcomeType) : Double = Math.exp(logpr(m)) // was O2
  //def logpr[O2<:OutcomeType](m:O2) : Double = Math.log(pr(m))
  override def logpr(m:OutcomeType) : Double = { // was O2
    var ret = Maths.logGamma(sum)
    for (i <- 1 until size) ret -= Maths.logGamma(alpha(i))
    for (i <- 1 until size) ret += alpha(i) * Math.log(m.pr(i))
    assert(ret == ret) // check for NaN
    ret
  }
}


// Make a general class Dirichlet to cover both constant and estimated varieties
// TODO Consider implementing this as a Multinomial plus a concentration parameter
abstract class Dirichlet[O<:MultinomialOutcome[O]](initialAlpha:Seq[Double], pseudoEvidence:Double)(implicit m:Manifest[O]) extends AbstractDirichlet[O] {
  //println("Dirichlet")
  def this(initialAlpha:Double)(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).allocSize, initialAlpha), 0.1)(m)
  def this(initialAlpha:Seq[Double])(implicit m:Manifest[O]) = this(initialAlpha, 0.1)(m)
  type VariableType <: Dirichlet[O];
  class DomainInSubclasses
  protected val _mean: Array[Double] = new Array[Double](initialAlpha.length)
  protected var _meanNormalizer = pseudoEvidence
  protected var _sum = initialAlpha.foldLeft(0.0)(_+_)
  for (i <- 0 until _mean.length) _mean(i) = initialAlpha(i) * _meanNormalizer / _sum 
  def size = _mean.size
  val outcomeDomain = Domain[O](m)
  assert(initialAlpha.length == outcomeDomain.allocSize)
  /** Just for convenient access as a sequence; allocates a new Seq.  For per-index access use mean(i:Int).
    This method name is slightly awkward, but I want to be sure that no one uses this when they really want mean(i:Int) */
  def means : RandomAccessSeq[Double] = new RandomAccessSeq[Double] { def apply(i:Int) = mean(i); def length = _mean.size } 
  @scala.inline final def mean(i:Int) = _mean(i) / _meanNormalizer
  @scala.inline final def alpha(i:Int) = mean(i) * _sum
  @scala.inline final def sum = _sum
  def sampleOutcome = { val mul = new DenseCountsMultinomial[O](outcomeDomain.size); sampleInto(mul); mul }
  override def generate[O2<:OutcomeType](m:O2)(implicit d:DiffList) = {
    DirichletGenerateDiff
    super.generate(m)
  }
  override def ungenerate[O2<:OutcomeType](m:O2)(implicit d:DiffList) = {
    DirichletUngenerateDiff
    super.ungenerate(m)
  }
  case class DirichletGenerateDiff(m:OutcomeType)(implicit d:DiffList) extends AutoDiff {
    def variable = Dirichlet.this
    def redo = { for (i <- 0 until size) _mean(i) += m.pr(i); _meanNormalizer += 1.0 }
    def undo = { for (i <- 0 until size) _mean(i) -= m.pr(i); _meanNormalizer -= 1.0 }
  }
  case class DirichletUngenerateDiff(override val m:OutcomeType)(implicit d:DiffList) extends DirichletGenerateDiff(m) {
    override def redo = super.undo
    override def undo = super.redo
  }
}
object Dirichlet {
  def apply[O<:MultinomialOutcome[O]](initialAlpha:Double)(implicit m:Manifest[O]) = new SymmetricDirichlet[O](initialAlpha)
  def apply[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) = new SymmetricDirichlet[O](1.0)
}
  
trait DirichletMomentMatchingEstimator[O<:MultinomialOutcome[O]] extends AbstractDirichlet[O] {
  this : Dirichlet[O] =>
  private def setUniform : Unit = {
    _sum = 1.0
    _meanNormalizer = uniformPseudoEvidence
    for (i <- 0 until _mean.length) _mean(i) = _meanNormalizer
  }
  def minSamplesForVarianceEstimate = 10
  /** Add a uniform pseudo-multinomial to estimation, with this weight relative to real multinomials */
  def uniformPseudoEvidence = 0.1 // Set to non-zero to avoid logGamma(0.0), leading to NaN
  override def estimate : Unit = {
    if (generatedSamples.size == 0) { setUniform; return }
    // TODO this would be much easier if Multinomial and Dirichlet used scalala; I should convert eventually
    val smoothing = uniformPseudoEvidence / _mean.length
    for (i <- 0 until _mean.length) _mean(i) = smoothing
    val variance = new Array[Double](_mean.length)
    for (m <- generatedSamples; i <- 0 until _mean.length) _mean(i) += m.pr(i) // TODO just make sure that this Dirichlet's alphas are not part of m.pr!
    _meanNormalizer = generatedSamples.size + uniformPseudoEvidence
    //for (i <- 0 until _mean.length) _mean(i) /= (generatedSamples.size + uniformPseudoEvidence)
    //println("unnormalized mean "+_mean.take(20).toList)
    //println("normalized mean "+(new Range(0,_mean.size,1).map(mean(_)).toList))
    assert (Maths.almostEquals(1.0, new Range(0,size,1).foldLeft(0.0)(_+mean(_))))
    if (generatedSamples.size <= minSamplesForVarianceEstimate) { // Not enough samples for a reliable variance estimate
      _sum = _mean.length
      return
    }
    // Calculate variance = E[x^2] - E[x]^2 for each dimension
    for (m <- generatedSamples; i <- 0 until _mean.length) {
      val p = m.pr(i) // TODO Was "count(i)/m.countTotal", trying to ensure this Dirichlet's alphas are not part of the m.pr
      variance(i) += p * p
    }
    for (i <- 0 until _mean.length) {
      val a = mean(i)
      variance(i) = (variance(i) / (generatedSamples.size - 1.0)) - a*a
    }
    //for (i <- 0 until _mean.length) variance(i) /= (generatedSamples.size - 1)
    //println("variance "+variance.take(10).toList)
    _sum = 0.0
    for (i <- 0 until _mean.length) 
      if (_mean(i) != 0.0) _sum += Math.log((mean(i) * (1.0 - mean(i)) / variance(i)) - 1.0)
      assert (_sum == _sum)
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
  keepGeneratedSamples = false
  def size = outcomeDomain.size
  protected val meanAlpha = 1.0 / outcomeDomain.size 
  def alpha(index:Int) = initialAlpha
  def mean(index:Int) = meanAlpha
  lazy val sum = initialAlpha * outcomeDomain.size
  def sampleOutcome = { val mul = new DenseCountsMultinomial[O](outcomeDomain.size); sampleInto(mul); mul }
}
