package cc.factorie.app.regress

import cc.factorie._
import cc.factorie.la._

// Infrastructure for regression.  The architecture is somewhat parallel to app.classify. 

// Explanatory variables are not listed here because we don't know how many there are, or what their types are.  These may be provided in subclasses.
/** Container for the result of running a Regressor. */
trait Regression[A<:TensorVar] {
  def dependant: A
  def dependantValue: A#Value
  def regressor: Regressor[A]
}

class UnivariateRegression[D<:RealVar](theDependant:D, val regressor:Regressor[D], theValue:Double) extends RealSpikeMarginal1[D](theDependant, theValue) with Regression[D] {
  val dependant = theDependant
  val dependantValue = new RealValue(theValue)
}


class SimpleRegression[E<:RealVar,D<:RealVar](val explanatory:E, theDependant:D, val regressor:SimpleRegressor[E,D], theValue:Double) extends RealSpikeMarginal1[D](theDependant, theValue) with Regression[D] {
  val dependant = theDependant
  val dependantValue = new RealValue(theValue)
}


/** Perform regression, with output of type A.  Input type is not specified here.
    @author Andrew McCallum */
trait Regressor[A<:TensorVar] {
  //def regressionValue(x:A#Value): Tensor // May not be possible if a Model.factors(x) is needed. 
  def regression(x:A): Regression[A]
  def regressions(xs:Iterable[A]): Seq[Regression[A]] = xs.toSeq.map(label => regression(label))
  //def regressionValues(xs:Iterable[A#Value]): Seq[Tensor] = xs.toSeq.map(x => regressionValue(x))
  /** Set the RealVariable to regressor-predicted value and return a Regression object summarizing the outcome. */
  def regress(x:A): Regression[A] = { val r = regression(x); /*r.globalize(null);*/ r }
  def regress(xs:Iterable[A]): Seq[Regression[A]] = { val r = regressions(xs); /*r.foreach(_.globalize(null));*/ r }
}

/** A regression from a single real-valued input to a single real-valued output.
    @author Andrew McCallum */
trait SimpleRegressor[E<:RealVar,A<:RealVar] extends Regressor[A] {
  def regressionValue(x:Double): Double
  //def regressionValue(x:A#Value): RealValue = new RealValue(regressionValue(x.doubleValue)) 
  override def regression(x:A): SimpleRegression[E,A] // = new SimpleRegression(x, this, regressionValue(x.doubleValue))
  override def regressions(xs:Iterable[A]): Seq[SimpleRegression[E,A]] = xs.toSeq.map(label => regression(label))
  /** Set the RealVariable to regressor-predicted value and return a Regression object summarizing the outcome. */
  override def regress(x:A): SimpleRegression[E,A] = { val r = regression(x); r.globalize(null); r }
  override def regress(xs:Iterable[A]): Seq[SimpleRegression[E,A]] = { val r = regressions(xs); r.foreach(_.globalize(null)); r }
}


/** Basic linear regressor with one real-valued input and one real-valued output.
    @author Andrew McCallum */
class SimpleLinearRegressor[E<:RealVar,D<:RealVar](val dependant2Explanatory:D=>E) extends SimpleRegressor[E,D] {
  var offset = 0.0
  var slope = 0.0
  def regressionValue(e:Double): Double = e * slope + offset
  def regression(x:D): SimpleRegression[E,D] = {
    val explanatory: E = dependant2Explanatory(x)
    new SimpleRegression[E,D](explanatory, x, this, regressionValue(explanatory.doubleValue))
  }
}
class SimpleLinearRegressorTrainer[E<:RealVar,D<:RealVar](val dependant2Explanatory:D=>E) {
  def train[E<:RealVar,D<:RealVar](ds:Iterable[D], dependant2Explanatory:D=>E): SimpleLinearRegressor[E,D] = {
    val xs = ds.toSeq
    val ys = xs.map(dependant2Explanatory(_))
    //val xsum = xs.sum
    throw new Error("Not yet implemented")
  }
}

class ModelBasedUnivariateRegressor[D<:RealVar](val model:Model) extends Regressor[D] {
  def regression(d:D): Regression[D] = new UnivariateRegression(d, this, model.score(d))
}

//class MultipleLinearRegressor[E<:TensorVar,D<:RealVar](val e2d:E=>D) extends Regressor[RealVar] {
//  val weights: Tensor = null
//  def regressionValue(t:Tensor): Double = weights dot t 
//}
