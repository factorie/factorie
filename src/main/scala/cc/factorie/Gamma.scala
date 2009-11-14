package cc.factorie

// TODO Consider renaming Real -> PositiveRealValue ??
// TODO Consider Real.value -> Real.toDouble and/or Real.doubleValue GenerativeDistribution[PositiveReal]

// TODO Consider creating PostiveReal, and then Gamma extends 

/** The Gamma distribution generating real values with parameters alpha and beta. */
class Gamma(alpha:Real, beta:Real) extends GenerativeDistribution[Real] {
  def this(alpha:Double, beta:Double) = this(new Real(alpha), new Real(beta))
  def pr(x:Double) = {
    assert (x > 0)
    Math.pow(beta.value, alpha.value) / Maths.gamma(alpha.value) * Math.pow(x, alpha.value - 1) * Math.exp(- beta.value * x)
  }
  def pr(o:Real): Double = pr(o.value)
  // TODO def logpr(x:Double) = 
  def sample: Double = Maths.nextGamma(alpha.value, beta.value)(Global.random)
  def estimate: Unit = {
    throw new Error("Not yet implemented")
  }
}

// TODO Finish this.
abstract class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma) extends GenerativeDistribution[Real]
