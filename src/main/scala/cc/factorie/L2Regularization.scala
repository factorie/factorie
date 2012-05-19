package cc.factorie

import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 5/10/12
 * Time: 12:21 PM
 * To change this template use File | Settings | File Templates.
 */


trait L2Regularization extends WeightUpdates {

  var familiesToRegularize = ArrayBuffer[DotFamily]()
  var lambdas = ArrayBuffer[Double]()
  def learningRate: Double = 1.0

  override def updateWeights : Unit = {
    super.updateWeights
    var fi = 0
    familiesToRegularize.foreach(f => {
      val newWeights = f.weights.copy 
      newWeights *= (1.0 - learningRate*lambdas(fi))
      //f.setWeights(f.weights * (1.0 - learningRate*lambdas(fi)))
      f.weights(0) = f.weights(0) // make sure update is called, hacky
      // TODO Yipes.  Clean this up.
      fi += 1
    })
  }
}