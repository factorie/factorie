package cc.factorie.directed

import cc.factorie._
import cc.factorie.la._
import cc.factorie.maths
import scala.util.Random
import org.jblas.DoubleMatrix
import cc.factorie.variable._
import scala.Some
import cc.factorie.infer._
import scala.Some

object DirectedTypeHelpers {
  type MutableTensorVarTensor1 = MutableTensorVar { type Value = Tensor1 }
  type MutableTensorVarTensor2 = MutableTensorVar { type Value = Tensor2 }
}
import DirectedTypeHelpers._

object MultivariateGaussian extends DirectedFamily3[MutableTensorVarTensor1, MutableTensorVarTensor1, MutableTensorVarTensor2] {
  self =>
  def logpr(value: Tensor1, mean: Tensor1, variance: Tensor2): Double = {
    val k = mean.length
    val centered = value - mean
    -0.5 * (k * math.log(2 * math.Pi) + math.log(determinant(variance)) + (invert(variance) * centered dot centered))
  }
  def pr(value: Tensor1, mean: Tensor1, variance: Tensor2): Double = math.exp(logpr(value, mean, variance))
  def sampledValue(mean: Tensor1, variance: Tensor2)(implicit random: scala.util.Random): Tensor1 = nextGaussian(mean, variance)(random)
  case class Factor(override val _1: MutableTensorVarTensor1, override val _2: MutableTensorVarTensor1, override val _3: MutableTensorVarTensor2)
    extends super.Factor(_1, _2, _3) {
    override def logpr(child: Tensor1, mean: Tensor1, variance: Tensor2): Double = self.logpr(child, mean, variance)
    override def logpr: Double = self.logpr(_1.value, _2.value, _3.value)
    def pr(child: Tensor1, mean: Tensor1, variance: Tensor2) = math.exp(logpr(child, mean, variance))
    override def pr: Double = self.pr(_1.value, _2.value, _3.value)
    def sampledValue(mean: Tensor1, variance: Tensor2)(implicit random: scala.util.Random): Tensor1 = self.sampledValue(mean, variance)
  }
  def newFactor(a: MutableTensorVarTensor1, b: MutableTensorVarTensor1, c: MutableTensorVarTensor2) = Factor(a, b, c)
  def nextGaussian(mean: Tensor1, covariance: Tensor2)(implicit r: Random): Tensor1 = {
    val uncorrelated = new DenseTensor1(Array.fill(mean.length)(maths.nextGaussian(0.0, 1.0)(r)))
    uncorrelated * cholesky(covariance) + mean
  }
  def matrix2Tensor(matrix: DoubleMatrix): DenseTensor2 = new DenseTensor2(matrix.toArray2)
  def tensor2Matrix(matrix: Tensor2) = { val m = matrix.dim1; new DoubleMatrix(matrix.asArray.grouped(m).toArray) }
  def invert(matrix: Tensor2): DenseTensor2 = matrix2Tensor(org.jblas.Solve.solve(tensor2Matrix(matrix), org.jblas.DoubleMatrix.eye(matrix.dim1)))
  def cholesky(matrix: Tensor2): DenseTensor2 =
    matrix2Tensor(org.jblas.Decompose.cholesky(tensor2Matrix(matrix)))
  def determinant(matrix: Tensor2): Double = org.jblas.Decompose.lu(tensor2Matrix(matrix)).u.diag().prod()
}

object MultivariateGaussianMixture
  extends DirectedFamily4[MutableTensorVarTensor1, Mixture[MutableTensorVarTensor1], Mixture[MutableTensorVarTensor2], DiscreteVariable] {
  case class Factor(
    override val _1: MutableTensorVarTensor1,
    override val _2: Mixture[MutableTensorVarTensor1],
    override val _3: Mixture[MutableTensorVarTensor2],
    override val _4: DiscreteVariable) extends super.Factor(_1, _2, _3, _4) {
    def gate = _4
    override def logpr(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue) =
      MultivariateGaussian.logpr(child, means(z.intValue), variances(z.intValue))
    def pr(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue) =
      MultivariateGaussian.pr(child, means(z.intValue), variances(z.intValue))
    def sampledValue(means: Seq[Tensor1], variances: Seq[Tensor2], z: DiscreteValue)(implicit random: scala.util.Random): Tensor1 =
      MultivariateGaussian.sampledValue(means(z.intValue), variances(z.intValue))
    def prChoosing(child: Tensor1, means: Seq[Tensor1], variances: Seq[Tensor2], mixtureIndex: Int): Double =
      MultivariateGaussian.pr(child, means(mixtureIndex), variances(mixtureIndex))
    def sampledValueChoosing(means: Seq[Tensor1], variances: Seq[Tensor2], mixtureIndex: Int)(implicit random: scala.util.Random): Tensor1 =
      MultivariateGaussian.sampledValue(means(mixtureIndex), variances(mixtureIndex))
  }
  def newFactor(a: MutableTensorVarTensor1, b: Mixture[MutableTensorVarTensor1], c: Mixture[MutableTensorVarTensor2], d: DiscreteVariable) =
    Factor(a, b, c, d)
}

object MaximizeMultivariateGaussianMean extends Maximize[Iterable[MutableTensorVarTensor1],DirectedModel] {
  def maxMean(meanVar: MutableTensorVarTensor1, model: DirectedModel, summary: Summary): Option[Tensor1] =
    getMeanFromFactors(model.extendedChildFactors(meanVar), _._2 == meanVar, _._2.indexOf(meanVar), summary)
  def apply(meanVar: MutableTensorVarTensor1, model: DirectedModel, summary: DiscreteSummary1[DiscreteVar] = null): Unit =
    maxMean(meanVar, model, summary).foreach(meanVar.set(_)(null))
  def infer(variables: Iterable[MutableTensorVarTensor1], model:DirectedModel, marginalizing:Summary): AssignmentSummary = {
    val assignment = new HashMapAssignment
    for (v <- variables) {
      val m = maxMean(v, model, marginalizing)
      m.foreach(i => assignment.update[MutableTensorVarTensor1](v, i))
    }
    new AssignmentSummary(assignment)
  }
  def getMeanFromFactors(
    factors: Iterable[DirectedFactor],
    pred1: MultivariateGaussian.Factor => Boolean,
    pred2: MultivariateGaussianMixture.Factor => Int,
    summary: Summary): Option[Tensor1] = {
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
          val gateMarginal: DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(gate).asInstanceOf[DiscreteMarginal1[DiscreteVar]]
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

object MaximizeMultivariateGaussianCovariance extends Maximize[Iterable[MutableTensorVarTensor2],DirectedModel] {
  def maxCovariance(covarianceVar: MutableTensorVarTensor2, model: DirectedModel, summary: DiscreteSummary1[DiscreteVar]): Option[Tensor2] = {
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
          val centered = (fvalue.value - mean)
          sum += new Outer1Tensor2(centered, centered)
          count += 1.0
        case MultivariateGaussianMixture.Factor(fvalue, _, fcovariances, gate) if fcovariances.contains(covarianceVar) =>
          val gateMarginal: DiscreteMarginal1[DiscreteVar] = if (summary eq null) null else summary.marginal(gate)
          val mixtureIndex = fcovariances.indexOf(covarianceVar)
          if (gateMarginal eq null) {
            if (gate.intValue == mixtureIndex) {
              val centered = (fvalue.value - mean)
              sum += new Outer1Tensor2(centered, centered)
              count += 1.0
            }
          } else {
            val p = gateMarginal.proportions(mixtureIndex)
            val centered = (fvalue.value - mean)
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
  def apply(covarianceVar: MutableTensorVarTensor2, model: DirectedModel, summary: DiscreteSummary1[DiscreteVar] = null): Unit =
    maxCovariance(covarianceVar, model, summary).foreach(covarianceVar.set(_)(null))
  def infer(variables: Iterable[MutableTensorVarTensor2], model:DirectedModel, marginalizing:Summary): AssignmentSummary = {
    lazy val assignment = new HashMapAssignment
    for (v <- variables) {
      val m = maxCovariance(v, model, marginalizing.asInstanceOf[DiscreteSummary1[DiscreteVar]])
      m.foreach(assignment.update[MutableTensorVarTensor2](v, _))
    }
    new AssignmentSummary(assignment)
  }
}
