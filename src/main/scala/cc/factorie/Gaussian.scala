package cc.factorie

// TODO I am now storing the mean and variance as Real variables, so that they can, in turn, be generated from other distributions.
// Perhaps we need to do this for all other GenerativeDistributions also?

/** A one-dimensional Gaussian distribution, generating Real (valued) variables.  Default estimation by moment-matching. */
class Gaussian1(initialMean:Real, initialVariance:Real) extends GenerativeDistribution[Real] {
  def this(initMean:Double, initVariance:Double) = this(new Real(initMean), new Real(initVariance))
  private var mean = initialMean
  private var variance = initialVariance
  def sample : Double = Maths.nextGaussian(mean.value, variance.value)(Global.random)
  def sampleInto(o:Real) : Unit = o.set(sample)(null) // TODO should we put a difflist here?
  def logpr(x:Double) : Double = {
    val diff = x - mean.value
    return - diff * diff / (2 * variance.value) - 0.5 * Math.log(2 * Math.Pi * variance.value)
  }
  override def logpr(o:Real):Double = logpr(o.value)
  def pr(x:Double):Double = Math.exp(logpr(x))
  def pr(o:Real):Double = pr(o.value)
  def minSamplesForVarianceEstimate = 5
  /** This implements a moment-matching estimator. */
  def estimate : Unit = {
    if (generatedSamples.size == 0) { mean := 0.0; variance := 1.0; return }
    mean := 0.0
    generatedSamples.foreach(s => mean += s.value)
    mean /= generatedSamples.size
    if (generatedSamples.size < minSamplesForVarianceEstimate) { variance := 1.0; return }
    variance := 0.0
    generatedSamples.foreach(s => { val diff = mean.value - s.value; variance += diff * diff })
    variance := Math.sqrt(variance.value / (generatedSamples.size - 1))
  }
}
