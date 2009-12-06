package cc.factorie
import cc.factorie.util.Implicits._

// TODO Consider renaming Real -> PositiveRealValue ??
// TODO Consider Real.value -> Real.toDouble and/or Real.doubleValue GenerativeDistribution[PositiveReal]

// TODO Consider creating PostiveReal, and then Gamma extends 

/** The Gamma distribution generating real values with parameters alpha and beta. 
    @author Andrew McCallum. */
class Gamma(alpha:Real, beta:Real) extends GenerativeDistribution[Real] {
  def this(alpha:Double, beta:Double) = this(new Real(alpha), new Real(beta))
  // Note that there is an implicit conversion from RealValue to Double, which we leverage below
  def pr(x:Double) = {
    assert (x > 0)
    Math.pow(beta, alpha) / Maths.gamma(alpha) * Math.pow(x, alpha - 1) * Math.exp(- beta * x)
  }
  def pr(o:Real): Double = pr(o.doubleValue)
  // TODO def logpr(x:Double) = 
  def sample: Double = Maths.nextGamma(alpha, beta)(Global.random)
  def estimate: Unit = {
    throw new Error("Not yet implemented")
  }
}

// TODO Finish this.
abstract class GammaGamma(alphaGamma:Gamma, betaGamma:Gamma) extends GenerativeDistribution[Real]
