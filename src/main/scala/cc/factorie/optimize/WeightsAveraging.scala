/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.optimize
import cc.factorie._
import cc.factorie.la._

/** Keeps an average of all weight settings throughout steps. 
    To get "average perceptron" use "new WeightsAveraging(new StepwiseGradientAscent)" */
class WeightsAveraging(inner:GradientOptimizer) extends WeightsAveragingImpl2(inner)
class WeightsAveragingImpl1(val inner:GradientOptimizer) extends GradientOptimizer {
  def currentTimeStepT:Long = _stepCount //TODO: ideally the inner optimizer would maintain the relevant time scale.
  var _stepCount = 0L
  var _updateCount = 0L
  private var _weightsUpdatedOn = 0L
  var weightsSum: WeightsTensor = null
  protected var lastUpdateIteration:WeightsTensor = null
  def reset(): Unit = {
    weightsSum = null
    lastUpdateIteration = null
    _stepCount = 0L
    _updateCount = 0L
    inner.reset()
  }
  def averageWeights(weights:Tensor= null) = {
    if(currentTimeStepT > _weightsUpdatedOn)updateWeightsSum(weights)
    weightsSum/currentTimeStepT.toDouble
  }
  def setWeightsToAverage(weights:Tensor) : Unit ={
    weights match{
      case weightsWT:WeightsTensor =>{
        updateWeightsSum(weights)
        for(template <- weightsWT.families){
          template.weights := weightsSum(template)/currentTimeStepT.toDouble
        }
      }
      case _ => throw new Exception("WeightsAveraging only implemented for WeightsTensor.")
    }
  }
  protected def updateWeightsSum(weights:Tensor) : Unit = {
    weights match{
      case weightsWT:WeightsTensor =>{
        _weightsUpdatedOn = currentTimeStepT
        for(template <- weightsWT.families){
          val vecWeightsSum = weightsSum(template)
          val vecLastUpdateIteration = lastUpdateIteration(template)
          val vecWeights = template.weights
          vecWeights.foreachActiveElement((i,v) => {
            val iterationDiff = currentTimeStepT.toDouble - vecLastUpdateIteration(i)
            if(iterationDiff>0){
              vecWeightsSum(i) += vecWeights(i) * iterationDiff
              vecLastUpdateIteration(i) = currentTimeStepT.toDouble
            }
          })
        }
      }
      case _ => throw new Exception("WeightsAveraging only implemented for WeightsTensor.")
    }
  }
  protected def isZero(wt:WeightsTensor):Boolean ={
    for(t <- wt.families)if(t.weights.activeDomainSize>0)return false
    true
  }
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    _stepCount += 1L
    gradient match{
      case gradientWT:WeightsTensor => {
        if(!isZero(gradientWT))_updateCount += 1L //TODO: this is not correct in cases where the gradient is 0 due to a subtraction.
        weights match{
          case weightsWT:WeightsTensor =>{
            if (weightsSum eq null) {
              weightsSum = weightsWT.copy
              lastUpdateIteration = weightsWT.blankCopy
            }
            //1. record the old value of the elements of the weight vector that are about to change
            val updateReconstruction = gradientWT.blankCopy
            for(template <- gradientWT.families){
              (updateReconstruction(template), weightsWT(template)) match{
                case (vecUpdateReconstruction:SparseIndexedTensor1,vecWeights:DenseTensor) =>{
                  val wValues = vecWeights.asArray
                  val urValues = vecUpdateReconstruction._values
                  val urIndices = vecUpdateReconstruction._indices
                  var i = 0
                  while(i<vecUpdateReconstruction.activeDomainSize){
                    urValues(i) -= wValues(urIndices(i))
                    i += 1
                  }
                }
                case _ =>{updateReconstruction(template) += weightsWT(template)}
              }
            }
            //2. perform the update
            inner.step(weights, gradient, value, margin)
            //3. reconstruct the "inner" update by adding the new values
            for(template <- gradientWT.families){
              (updateReconstruction(template), weightsWT(template)) match{
                case (vecUpdateReconstruction:SparseIndexedTensor1,vecWeights:DenseTensor) =>{
                  val wValues = vecWeights.asArray
                  val urValues = vecUpdateReconstruction._values
                  val urIndices = vecUpdateReconstruction._indices
                  var i = 0
                  while(i<vecUpdateReconstruction.activeDomainSize){
                    urValues(i) += wValues(urIndices(i))
                    i += 1
                  }
                }
                case _ =>{updateReconstruction(template) += weightsWT(template)}
              }
            }
            //4. accumulate the weights
            for(template:DotFamily <- updateReconstruction.families){
              val g = gradientWT.apply(template)
              (updateReconstruction(template), weightsSum(template),lastUpdateIteration(template),template.weights) match{
                case (vecReconstruction:SparseIndexedTensor1,vecWeightsSum:DenseTensor,vecLastUpdateIteration:DenseTensor,template:DenseTensor) =>{

                  val vrValues = vecReconstruction._values
                  val vrIndices = vecReconstruction._indices
                  val wsValues = vecWeightsSum.asArray
                  val wValues = weightsWT.asArray
                  val luiValues = vecLastUpdateIteration.asArray
                  //val gValues = gradientWT(template).asInstanceOf[SparseIndexedTensor]._values
                  var i = 0
                  g match{
                    case giwt:SparseIndexedTensor => {
                      val gValues = giwt._values
                      while(i<vecReconstruction.activeDomainSize){
                        val didx = vrIndices(i)
                        val iterationDiff = currentTimeStepT.toDouble - luiValues(didx)
                        assert(iterationDiff >= 0)
                        if(iterationDiff>0){
                          //          templateWeightsSum(i) += (templateWeights(i) * iterationDiff) + vector(i)
                          wsValues(didx) += (wValues(didx) - gValues(i)) * luiValues(didx) //this works because reconstruction is a copy of gValues is a copy. The subtraction is necessary to "undo" the update by the inner optimizer
                        }
                        i += 1
                      }
                    }
                    case giwt:DenseTensor => {
                      val gValues = giwt.asArray
                      while(i<vecReconstruction.activeDomainSize){
                        val didx = vrIndices(i)
                        val iterationDiff = currentTimeStepT.toDouble - luiValues(didx)
                        assert(iterationDiff >= 0)
                        if(iterationDiff>0){
                          //          templateWeightsSum(i) += (templateWeights(i) * iterationDiff) + vector(i)
                          wsValues(didx) += (wValues(didx) - gValues(didx)) * luiValues(didx) //tThe subtraction is necessary to "undo" the update by the inner optimizer
                        }
                        i += 1
                      }
                    }
                  }
                }
                case _ => throw new Exception("WeightsAveraging is not implemented for these types of tensors.")
              }
            }
/*
              vecReconstruction.foreachActiveElement((i,v) => {
                val iterationDiff = currentTimeStepT.toDouble - vecLastUpdateIteration(i)
                assert(iterationDiff >= 0)
                if(iterationDiff>0){
                  vecWeightsSum(i) += vecWeights(i) * iterationDiff + vecReconstruction(i)
                  vecLastUpdateIteration(i) = currentTimeStepT.toDouble
                } else vecWeightsSum(i) += vecReconstruction(i)
              })
            }
            println("   finished step")
          }
          case _ => throw new Exception("Weight averaging  only implemented for weight vectors of type WeightsTensor.")
        }
        */
          }
          case _ => throw new Exception("Weight averaging only implemented for gradients of type WeightsTensor.")
        }
      }
    }
  }
    def isConverged: Boolean = inner.isConverged
}

class WeightsAveragingImpl2(val inner:GradientOptimizer) extends GradientOptimizer { //this implementation is based on one described in Hal Daume's dissertation.
  def currentTimeStepT:Long = _stepCount //TODO: ideally the inner optimizer would maintain the relevant time scale.
  var _stepCount = 0L
  var _updateCount = 0L
  var weightsSum: WeightsTensor = null
  def reset(): Unit = {
    weightsSum = null
    _stepCount = 0L
    _updateCount = 0L
    inner.reset()
  }
  protected def isZero(wt:WeightsTensor):Boolean ={
    for(t <- wt.families)if(t.weights.activeDomainSize>0)return false
    true
  }
  /*
    def averageWeights(weights:Tensor= null) = {
    if(currentTimeStepT > _weightsUpdatedOn)updateWeightsSum(weights)
    weightsSum/currentTimeStepT.toDouble
  }
    def setWeightsToAverage(weights:Tensor) : Unit ={
    weights match{
      case weightsWT:WeightsTensor =>{
        updateWeightsSum(weights)
        for(template <- weightsWT.families){
          template.weights := weightsSum(template)/currentTimeStepT.toDouble
        }
      }
      case _ => throw new Exception("WeightsAveraging only implemented for WeightsTensor.")
    }
  }
   */
  def averageWeights(weights:Tensor=null):WeightsTensor = {
    var result:WeightsTensor = null
    weights match{
      case weightsWT:WeightsTensor =>{
        result = weightsWT.copy
        for(template <- weightsWT.families)
          result(template) := weightsSum(template)/currentTimeStepT.toDouble + template.weights
      }
      case _ => throw new Exception("WeightsAveraging only implemented for WeightsTensor.")
    }
    result
  }
  def setWeightsToAverage(weights:Tensor) : Unit = {
    weights match{
      case weightsWT:WeightsTensor =>{
        for(template <- weightsWT.families)
          template.weights := template.weights+weightsSum(template)/currentTimeStepT.toDouble
      }
      case _ => throw new Exception("WeightsAveraging only implemented for WeightsTensor.")
    }
  }
  def step(weights:Tensor, gradient:Tensor, value:Double, margin:Double): Unit = {
    _stepCount += 1L
    gradient match{
      case gradientWT:WeightsTensor => {
        if(!isZero(gradientWT))_updateCount += 1L //TODO: this is not correct in cases where the gradient is 0 due to a subtraction.
        weights match{
          case weightsWT:WeightsTensor =>{
            if (weightsSum eq null) {
              weightsSum = weightsWT.copy
              weightsSum = weightsWT.blankCopy
            }
            //1. record the old value of the elements of the weight vector that are about to change
            var timer = System.currentTimeMillis
            val updateReconstruction = gradientWT.blankCopy
            timer = System.currentTimeMillis
            for(template <- gradientWT.families){
              (updateReconstruction(template), weightsWT(template)) match{
                case (vecUpdateReconstruction:SparseIndexedTensor1,vecWeights:DenseTensor) =>{
                  val wValues = vecWeights.asArray
                  val urValues = vecUpdateReconstruction._values
                  val urIndices = vecUpdateReconstruction._indices
                  var i = 0
                  while(i<vecUpdateReconstruction.activeDomainSize){
                    urValues(i) -= wValues(urIndices(i))
                    i += 1
                  }
                }
                case _ =>{updateReconstruction(template) += weightsWT(template)}
              }
            }
            //2. perform the update
            inner.step(weights, gradient, value, margin)
            //3. reconstruct the "inner" update by adding the new values
            for(template <- gradientWT.families){
              (updateReconstruction(template), weightsWT(template)) match{
                case (vecUpdateReconstruction:SparseIndexedTensor1,vecWeights:DenseTensor) =>{
                  val wValues = vecWeights.asArray
                  val urValues = vecUpdateReconstruction._values
                  val urIndices = vecUpdateReconstruction._indices
                  var i = 0
                  while(i<vecUpdateReconstruction.activeDomainSize){
                    urValues(i) += wValues(urIndices(i))
                    i += 1
                  }
                }
                case _ => throw new Exception("Only implemented for WeightsTensors (both model weights and gradient).")//{updateReconstruction(template) += weightsWT(template)}
              }
            }
            //4. accumulate the weights
            for(template <- weightsSum.families)
              template.weights += updateReconstruction(template) * -currentTimeStepT.toDouble
          }
          case _ => throw new Exception("Weight averaging  only implemented for weight vectors of type WeightsTensor.")
        }
      }
      case _ => throw new Exception("Weight averaging only implemented for gradients of type WeightsTensor.")
    }
  }
  def isConverged: Boolean = inner.isConverged
}