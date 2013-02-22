package cc.factorie.app.regress

import cc.factorie._
import app.classify.LabelList
import cc.factorie.la._
import optimize._
import java.io.File
import util.DoubleAccumulator

// Infrastructure for regression.  The architecture is somewhat parallel to app.classify. 

// Explanatory variables are not listed here because we don't know how many there are, or what their types are.  These may be provided in subclasses.
/** Container for the result of running a Regressor. */
trait Regression[A<:TensorVar] {
  def dependant: A
  def dependantValue: A#Value
  def regressor: Regressor[A]
}

/** Perform regression, with output of type A.  Input type is not specified here.
 0   @author Andrew McCallum */
trait Regressor[A<:TensorVar] {
  //def regressionValue(x:A#Value): Tensor // May not be possible if a Model.factors(x) is needed. 

  def regression(x:A): Regression[A]
  def regressions(xs:Iterable[A]): Seq[Regression[A]] = xs.toSeq.map(label => regression(label))
  //def regressionValues(xs:Iterable[A#Value]): Seq[Tensor] = xs.toSeq.map(x => regressionValue(x))
  /** Set the RealVariable to regressor-predicted value and return a Regression object summarizing the outcome. */

  def regress(x:A): Regression[A] = { val r = regression(x); /*r.globalize(null);*/ r }

 def regress(xs:Iterable[A]): Seq[Regression[A]] = { val r = regressions(xs); /*r.foreach(_.globalize(null));*/ r }
}

class LinearRegressor[E<:TensorVar,A<:TensorVar](val dependant2Explanatory: A=>E, val weights: Tensor2) extends Regressor[A]{
  linearRegressor =>

  def regression(x: A) = {
    val result = (weights * dependant2Explanatory(x).value.asInstanceOf[Tensor1]).reshape(x.value.dimensions)
    new Regression[A] {
      def dependant = x
      def dependantValue = result.asInstanceOf[A#Value]
      def regressor = linearRegressor
    }
  }
}

class LinearRegressionModel(nFeatures: Int, nLabel: Int) extends Model {
  /** Return all Factors in this Model that touch the given "variable".  The result will not have any duplicate Factors. */
  def factors(variable: Var) = Nil
  val family = new DotFamily {
    lazy val weights = new DenseTensor2(nLabel, nFeatures)
  }
  override def families = Seq(family)
  def weights = family.weights.asInstanceOf[DenseTensor2]
}

object LinearRegressionObjectiveFunctions {
  type ObjectiveFunctionType = (Tensor1, Tensor1) => (Double, Tensor1)
  val squaredObjective : ObjectiveFunctionType = (prediction, label) => {
    for (i <- prediction.activeDomain) {
      prediction(i) -= label(i)
    }
    val value = - (prediction dot prediction)
    prediction *= -2
    (value,prediction)
  }
  def epsilonInsensitiveObjectiveSq(epsilon: Double): ObjectiveFunctionType = (prediction, label) => {
    var objective = 0.0
    for (i <- prediction.activeDomain) {
      prediction(i) -= label(i)
      val o = math.max(0, math.abs(prediction(i)) - epsilon)
      objective -= o*o
      prediction(i) = -2*prediction(i)
    }
    (objective, prediction)
  }
}


class LinearRegressionExample(features: TensorVar, label: TensorVar, objective: LinearRegressionObjectiveFunctions.ObjectiveFunctionType = LinearRegressionObjectiveFunctions.squaredObjective) extends Example[LinearRegressionModel] {
  // gradient or value or margin can be null if they don't need to be computed.
  def accumulateExampleInto(model: LinearRegressionModel, gradient: WeightsTensorAccumulator, value: DoubleAccumulator, margin: DoubleAccumulator) {
    val weights = model.weights
    val prediction = weights * features.value.asInstanceOf[Tensor1]
    val (objValue,objGradient) = objective(prediction, label.value.asInstanceOf[Tensor1])
    if (value != null) value.accumulate(objValue)
    // add the gradients
    if (gradient != null) {
      gradient.accumulate(model.family, new cc.factorie.la.Outer1Tensor2(objGradient, features.value.asInstanceOf[Tensor1]))
    }
  }
}

object LinearRegressionTrainer {
  // Assumes variables are set to their target values
  def train[E <: TensorVar, A <: TensorVar](examples: Iterable[A], dependant2Explanatory: A => E, l2: Double, objective: LinearRegressionObjectiveFunctions.ObjectiveFunctionType = LinearRegressionObjectiveFunctions.squaredObjective): LinearRegressor[E, A] = {
    val optimizer = new cc.factorie.optimize.LBFGS() with cc.factorie.optimize.L2Regularization
    optimizer.variance = 1.0/l2
    val trainerMaker : LinearRegressionModel => Trainer[LinearRegressionModel] = m => new BatchTrainer(m, optimizer)
    trainCustom(examples, dependant2Explanatory, trainerMaker, objective)
  }

  def trainCustom[E <: TensorVar, A <: TensorVar](examples: Iterable[A], dependant2Explanatory: A => E, trainerMaker: LinearRegressionModel => Trainer[LinearRegressionModel], objective: LinearRegressionObjectiveFunctions.ObjectiveFunctionType = LinearRegressionObjectiveFunctions.squaredObjective): LinearRegressor[E, A] = {
    val exampleDependent = examples.head
    val exampleExplanatory = dependant2Explanatory(exampleDependent)
    val dependentSize = exampleDependent.value.dimensions.product
    val explanatorySize = exampleExplanatory.value.dimensions.product
    val model = new LinearRegressionModel(explanatorySize, dependentSize)
    val trainer = trainerMaker(model)
    val trainingExamples = examples.map(e => new LinearRegressionExample(dependant2Explanatory(e), e, objective))
    while (!trainer.isConverged) {
      trainer.processExamples(trainingExamples)
    }
    new LinearRegressor(dependant2Explanatory, model.weights)
  }
}

