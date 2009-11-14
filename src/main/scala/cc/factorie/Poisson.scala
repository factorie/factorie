package cc.factorie
import cc.factorie.util.Implicits._

/** The Poisson distribution generating integer values with parameter lambda. */
class Poisson(val lambda:Real) extends GenerativeDistribution[IntValue] {
  def this(lambda:Double) = this(new Real(lambda))
  def mean: Double = lambda.value
  def variable: Double = lambda.value
  def pr(k:Int) = Math.pow(lambda.value, k) * Math.exp(-lambda.value) / Maths.factorial(k)
  def pr(o:IntValue): Double = pr(o.intValue)
  def sample: Int = Maths.nextPoisson(lambda.value)(Global.random).toInt
  /** This implements the maximum likelihood estimator */
  def estimate: Unit = {
    if (generatedSamples.size == 0) throw new Error("No samles from which to estimate")
    val sum = generatedSamples.sum(_.intValue)
    lambda.set(sum/generatedSamples.size)(null) // TODO Should we put a DiffList here?
  }
}

abstract class GammaPoisson(gamma:Gamma) extends GenerativeDistribution[IntValue] {
  throw new Error("Not yet implemented")
}

