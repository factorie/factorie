package cc.factorie.app.regress

import cc.factorie._
import app.classify.LabelList
import cc.factorie.la._
import cc.factorie.optimize.LimitedMemoryBFGS
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
    val result = weights.matrixVector(dependant2Explanatory(x).value).reshape(x.value.dimensions)
    new Regression[A] {
      def dependant = x
      def dependantValue = result.asInstanceOf[A#Value]
      def regressor = linearRegressor
    }
  }
}

object LinearRegressionTrainer {
  // Assumes variables are set to their target values
  def train[E<:TensorVar, A<:TensorVar](examples: Iterable[A], dependant2Explanatory: A=>E, l2: Double): LinearRegressor[E,A] = {
    val exampleDependent = examples.head
    val exampleExplanatory = dependant2Explanatory(exampleDependent)
    val dependentSize = exampleDependent.value.dimensions.reduce((a,b)=>a*b)
    val explanatorySize = exampleExplanatory.value.dimensions.reduce((a,b)=>a*b)
    val weights = new DenseTensor2(dependentSize, explanatorySize)
    //val optimizer = new cc.factorie.optimize.ConjugateGradient()
    val optimizer = new cc.factorie.optimize.LimitedMemoryBFGS()
    val gradient = new DenseTensor2(weights.dim1, weights.dim2)
    while(!optimizer.isConverged) {
      gradient.zero()
      var value = 0.0
      examples.foreach(e => {
        val features = dependant2Explanatory(e).value
        val prediction = weights.matrixVector(features)
        prediction.activeDomain.foreach(i => prediction(i) -= e.value(i))
        value -= prediction.dot(prediction)
        // add the gradients
        prediction.activeDomain.foreach(i => {
          features.activeDomain.foreach(j => {
            gradient(i,j) -= 2*prediction(i)*features(j)
          })
        })
      })

      (0 to weights.dim1-1).foreach(i => {
        (0 to weights.dim2-1).foreach(j => {
          gradient(i,j) += l2 * weights(i,j)
        })
      })

      optimizer.step(weights, gradient, value, 0.0)
    }
    new LinearRegressor(dependant2Explanatory, weights)
  }
}

