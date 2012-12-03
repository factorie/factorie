package cc.factorie.app.regress

import cc.factorie._
import app.classify.LabelList
import cc.factorie.la._
import cc.factorie.optimize.LBFGS
import java.io.File

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

object LinearRegressionTrainer {
  // Assumes variables are set to their target values
  def train[E <: TensorVar, A <: TensorVar](examples: Iterable[A], dependant2Explanatory: A => E, l2: Double): LinearRegressor[E, A] = {
    val exampleDependent = examples.head
    val exampleExplanatory = dependant2Explanatory(exampleDependent)
    val dependentSize = exampleDependent.value.dimensions.product
    val explanatorySize = exampleExplanatory.value.dimensions.product
    val weights = new DenseTensor2(dependentSize, explanatorySize)
    //val optimizer = new cc.factorie.optimize.ConjugateGradient()
    val optimizer = new cc.factorie.optimize.LBFGS()
    val gradient = new DenseTensor2(weights.dim1, weights.dim2)
    while (!optimizer.isConverged) {
      gradient.zero()
      var value = 0.0
      for (e <- examples) {
        val features = dependant2Explanatory(e).value.asInstanceOf[Tensor1]
        val prediction = weights * features
        for (i <- prediction.activeDomain) prediction(i) -= e.value(i)
        value -= prediction dot prediction
        // add the gradients
        for (i <- prediction.activeDomain; j <- features.activeDomain)
            gradient(i, j) -= 2 * prediction(i) * features(j)
      }
      gradient += (weights, l2)
      optimizer.step(weights, gradient, value, 0.0)
    }
    new LinearRegressor(dependant2Explanatory, weights)
  }
}

