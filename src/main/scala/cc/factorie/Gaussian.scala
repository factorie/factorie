package cc.factorie
import cc.factorie.util.Implicits._

// TODO I am now storing the mean and variance as Real variables, so that they can, in turn, be generated from other distributions.
// Perhaps we need to do this for all other GenerativeDistributions also?

/** A one-dimensional Gaussian distribution, generating Real (valued) variables.  Default estimation by moment-matching. */
class Gaussian1(initialMean:Real, initialVariance:Real) extends GenerativeDistribution[Real] {
  def this(initMean:Double, initVariance:Double) = this(new Real(initMean), new Real(initVariance))
  private var mean = initialMean
  private var variance = initialVariance
  def sample : Double = Maths.nextGaussian(mean, variance)(Global.random)
  def sampleInto(o:Real) : Unit = o.set(sample)(null) // TODO should we put a difflist here?
  def logpr(x:Double) : Double = {
    val diff = x - mean.doubleValue
    return - diff * diff / (2 * variance) - 0.5 * Math.log(2 * Math.Pi * variance)
  }
  override def logpr(o:Real):Double = logpr(o.doubleValue)
  def pr(x:Double):Double = Math.exp(logpr(x))
  def pr(o:Real):Double = pr(o)
  def minSamplesForVarianceEstimate = 5
  /** This implements a moment-matching estimator. */
  def estimate : Unit = {
    if (generatedSamples.size == 0) { mean := 0.0; variance := 1.0; return }
    mean := 0.0
    generatedSamples.foreach(s => mean += s)
    mean /= generatedSamples.size
    if (generatedSamples.size < minSamplesForVarianceEstimate) { variance := 1.0; return }
    variance := 0.0
    generatedSamples.foreach(s => { val diff = mean - s; variance += diff * diff })
    variance := Math.sqrt(variance / (generatedSamples.size - 1))
  }
}
