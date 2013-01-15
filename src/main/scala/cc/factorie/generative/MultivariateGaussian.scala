package cc.factorie.generative

import cc.factorie._
import cc.factorie.DiscreteValue
import cc.factorie.DiscreteVariable
import cc.factorie.la._
import cc.factorie.maths
import cc.factorie.MutableTensorVar
import cern.colt.matrix.tdouble.{DoubleFactory2D, DoubleMatrix2D}
import cern.colt.matrix.tdouble.algo.decomposition._
import scala.util.Random
import cern.colt.matrix.tdouble.impl.DenseDoubleMatrix2D

object MultivariateGaussian extends GenerativeFamily3[MutableTensorVar[Tensor1], MutableTensorVar[Tensor1], MutableTensorVar[Tensor2]] {
  self =>
  def logpr(value: Tensor1, mean: Tensor1, variance: Tensor2): Double = {
    val k = mean.length
    val centered = value - mean
    -0.5 * (k * math.log(2 * math.Pi) + math.log(determinant(variance)) + (invert(variance) * centered dot centered))
  }
  def pr(value: Tensor1, mean: Tensor1, variance: Tensor2): Double = math.exp(logpr(value, mean, variance))
  def sampledValue(mean: Tensor1, variance: Tensor2): Tensor1 = nextGaussian(mean, variance)(cc.factorie.random)
  case class Factor(override val _1: MutableTensorVar[Tensor1], override val _2: MutableTensorVar[Tensor1], override val _3: MutableTensorVar[Tensor2])
    extends super.Factor(_1, _2, _3) {
    override def logpr(child: Tensor1, mean: Tensor1, variance: Tensor2): Double = self.logpr(child, mean, variance)
    override def logpr: Double = self.logpr(_1.value, _2.value, _3.value)
    def pr(child: Tensor1, mean: Tensor1, variance: Tensor2) = math.exp(logpr(child, mean, variance))
    override def pr: Double = self.pr(_1.value, _2.value, _3.value)
    def sampledValue(mean: Tensor1, variance: Tensor2): Tensor1 = self.sampledValue(mean, variance)
  }
  def newFactor(a: MutableTensorVar[Tensor1], b: MutableTensorVar[Tensor1], c: MutableTensorVar[Tensor2]) = Factor(a, b, c)
  def nextGaussian(mean: Tensor1, covariance: Tensor2)(implicit r: Random): Tensor1 = {
    val uncorrelated = new DenseTensor1(Array.fill(mean.length)(maths.nextGaussian(0.0, 1.0)(r)))
    choleskyT(covariance) * uncorrelated + mean
  }
  def matrix2Tensor(matrix: DoubleMatrix2D): DenseTensor2 = new DenseTensor2(matrix.toArray)
  def tensor2Matrix(matrix: Tensor2) = { val n = matrix.dim1; new DenseDoubleMatrix2D(matrix.asArray.grouped(n).toArray) }
  def invert(matrix: Tensor2): DenseTensor2 =
    matrix2Tensor(new DenseDoubleLUDecomposition(tensor2Matrix(matrix)).solve(DoubleFactory2D.dense.identity(matrix.dim1)))
  def choleskyT(matrix: Tensor2): DenseTensor2 =
    matrix2Tensor(new DenseDoubleCholeskyDecomposition(tensor2Matrix(matrix)).getLtranspose)
  def determinant(matrix: Tensor2): Double = new DenseDoubleLUDecomposition(tensor2Matrix(matrix)).det()
}

object MultivariateGaussianMixture
  extends GenerativeFamily4[MutableTensorVar[Tensor1], Mixture[MutableTensorVar[Tensor1]], Mixture[MutableTensorVar[Tensor2]], DiscreteVariable] {
  case class Factor(
    override val _1: MutableTensorVar[Tensor1],
    override val _2: Mixture[MutableTensorVar[Tensor1]],
    override val _3: Mixture[MutableTensorVar[Tensor2]],
    override val _4: DiscreteVariable) extends super.Factor(_1, _2, _3, _4) {
    def gate = _4
    override def logpr(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue) =
      MultivariateGaussian.logpr(child, means(z.intValue), variances(z.intValue))
    def pr(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue) =
      MultivariateGaussian.pr(child, means(z.intValue), variances(z.intValue))
    def sampledValue(means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue): Tensor1 =
      MultivariateGaussian.sampledValue(means(z.intValue), variances(z.intValue))
    def prChoosing(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], mixtureIndex: Int): Double =
      MultivariateGaussian.pr(child, means(mixtureIndex), variances(mixtureIndex))
    def sampledValueChoosing(means: Seq[Tensor1], variances: Seq[Tensor2], mixtureIndex: Int): Tensor1 =
      MultivariateGaussian.sampledValue(means(mixtureIndex), variances(mixtureIndex))
  }
  def newFactor(a: MutableTensorVar[Tensor1], b: Mixture[MutableTensorVar[Tensor1]], c: Mixture[MutableTensorVar[Tensor2]], d: DiscreteVariable) =
    Factor(a, b, c, d)
}

object MaximizeMultivariateGaussianMean extends Maximize {
  def maxMean(meanVar: MutableTensorVar[Tensor1], model: GenerativeModel, summary: DiscreteSummary1[DiscreteVar]): Option[Tensor1] =
    getMeanFromFactors(model.extendedChildFactors(meanVar), _._2 == meanVar, _._2.indexOf(meanVar), summary)
  def apply(meanVar: MutableTensorVar[Tensor1], model: GenerativeModel, summary: DiscreteSummary1[DiscreteVar] = null): Unit =
    maxMean(meanVar, model, summary).foreach(meanVar.set(_)(null))
  override def infer(variables: Iterable[Var], model: Model, summary: Summary[Marginal] = null): Option[AssignmentSummary] = {
    val gModel = model match { case m: GenerativeModel => m; case _ => return None }
    val dSummary = summary match { case s: DiscreteSummary1[DiscreteVar] => s; case null => null; case _ => return None }
    lazy val assignment = new HashMapAssignment
    for (v <- variables) v match {
      // Ugh, erasure makes this not at all type-safe
      case r: MutableTensorVar[Tensor1] if r.tensor.isInstanceOf[Tensor1] =>
        val m = maxMean(r, gModel, dSummary)
        if (m.isEmpty) return None
        else assignment(r) = m.get
      case _ => return None
    }
    Some(new AssignmentSummary(assignment))
  }
  def getMeanFromFactors(
    factors: Iterable[GenerativeFactor],
    pred1: MultivariateGaussian.Factor => Boolean,
    pred2: MultivariateGaussianMixture.Factor => Int,
    summary: DiscreteSummary1[DiscreteVar]): Option[Tensor1] = {
    val factorIter = factors.iterator
    var sum = null: Tensor1; var count = 0.0
    while (factorIter.hasNext)
      factorIter.next() match {
        case f@MultivariateGaussian.Factor(fvalue, _, _) if pred1(f) =>
          if (sum == null) sum = new DenseTensor1(fvalue.value.length, 0.0)
          sum += fvalue.value
          count += 1
        case f@MultivariateGaussianMixture.Factor(fvalue, fmeans, _, gate) if pred2(f) != -1 =>
          if (sum == null) sum = new DenseTensor1(fvalue.value.length, 0.0)
          val gateMarginal: DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal1(gate)
           val mixtureIndex = pred2(f)
           if (gateMarginal eq null) {
             if (gate.intValue == mixtureIndex) { sum += fvalue.value; count += 1.0 }
           } else {
             val p = gateMarginal.proportions(mixtureIndex)
             assert(p == p); assert(p >= 0.0); assert(p <= 1.0)
             sum += (fvalue.value, p); count += p
           }
        case m: Mixture.Factor => { }
        case f => println("MaximizeMultivariateGaussianMean can't handle factor " + f.getClass.getName + "=" + f); return None
      }
    sum /= count
    Some(sum)
  }
}

object MaximizeMultivariateGaussianCovariance extends Maximize {
  def maxCovariance(covarianceVar: MutableTensorVar[Tensor2], model: GenerativeModel, summary: DiscreteSummary1[DiscreteVar]): Option[Tensor2] = {
    val factorIter = model.extendedChildFactors(covarianceVar).iterator
    val n = covarianceVar.value.dim1
    val meanOpt = MaximizeMultivariateGaussianMean.getMeanFromFactors(
      model.extendedChildFactors(covarianceVar),
      _._3 == covarianceVar, _._3.indexOf(covarianceVar), summary)
    if (meanOpt.isEmpty) return None
    val mean = meanOpt.get
    var sum = new DenseTensor2(n, n, 0.0); var count = 0.0
    while (factorIter.hasNext)
      factorIter.next() match {
        case MultivariateGaussian.Factor(fvalue, _, fcovariance) if fcovariance == covarianceVar =>
          val centered = (fvalue.value - mean).asInstanceOf[Tensor1]
          sum += new Outer1Tensor2(centered, centered)
          count += 1.0
        case MultivariateGaussianMixture.Factor(fvalue, _, fcovariances, gate) if fcovariances.contains(covarianceVar) =>
          val gateMarginal: DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal1(gate)
          val mixtureIndex = fcovariances.indexOf(covarianceVar)
          if (gateMarginal eq null) {
            if (gate.intValue == mixtureIndex) {
              val centered = (fvalue.value - mean).asInstanceOf[Tensor1]
              sum += new Outer1Tensor2(centered, centered)
              count += 1.0
            }
          } else {
            val p = gateMarginal.proportions(mixtureIndex)
            val centered = (fvalue.value - mean).asInstanceOf[Tensor1]
            centered *= math.sqrt(p)
            sum += new Outer1Tensor2(centered, centered)
            count += p
          }
        case m: Mixture.Factor => { }
        case _ => return None
      }
    if (count < 2) return None
    sum /= (count - 1.0)
    Some(sum)
  }
  def apply(covarianceVar: MutableTensorVar[Tensor2], model: GenerativeModel, summary: DiscreteSummary1[DiscreteVar] = null): Unit =
    maxCovariance(covarianceVar, model, summary).foreach(covarianceVar.set(_)(null))
  override def infer(variables: Iterable[Var], model: Model, summary: Summary[Marginal] = null): Option[AssignmentSummary] = {
    val gModel = model match { case m: GenerativeModel => m; case _ => return None }
    val dSummary = summary match { case s: DiscreteSummary1[DiscreteVar] => s; case null => null; case _ => return None }
    lazy val assignment = new HashMapAssignment
    for (v <- variables) v match {
      // Ugh, erasure makes this not at all type-safe
      case r: MutableTensorVar[Tensor2] if r.tensor.isInstanceOf[Tensor2] =>
        val m = maxCovariance(r, gModel, dSummary)
        if (m.isEmpty) return None
        else assignment(r) = m.get
      case f => println("MaximizeMultivariateGaussianCovariance can't handle factor " + f.getClass.getName + "=" + f); return None
    }
    Some(new AssignmentSummary(assignment))
  }
}
