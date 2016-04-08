/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie

import cc.factorie.variable.TensorVar

/**
 * User: apassos
 * Date: 9/27/13
 * Time: 12:17 PM
 */
object Factorie {
  type DenseTensor1 = cc.factorie.la.DenseTensor1
  type DenseTensor2 = cc.factorie.la.DenseTensor2
  type DenseTensor3 = cc.factorie.la.DenseTensor3
  type DenseTensor4 = cc.factorie.la.DenseTensor4

  type Tensor1 = cc.factorie.la.Tensor1
  type Tensor2 = cc.factorie.la.Tensor2
  type Tensor3 = cc.factorie.la.Tensor3
  type Tensor4 = cc.factorie.la.Tensor4

  type Var = variable.Var

  type Assignment = variable.Assignment
  val TargetAssignment = variable.TargetAssignment
  val GlobalAssignment = variable.GlobalAssignment
  type HashMapAssignment = variable.HashMapAssignment
  type Assignment1[A<:Var] = variable.Assignment1[A]
  type Assignment2[A<:Var,B<:Var] = variable.Assignment2[A,B]
  type Assignment3[A<:Var,B<:Var,C<:Var] = variable.Assignment3[A,B,C]
  type Assignment4[A<:Var,B<:Var,C<:Var,D<:Var] = variable.Assignment4[A,B,C,D]

  type BooleanVariable = variable.BooleanVariable

  type CategoricalDomain[T] = variable.CategoricalDomain[T]
  type CategoricalVectorDomain[T] = variable.CategoricalVectorDomain[T]

  type CategoricalVariable[T] = variable.CategoricalVariable[T]
  type CategoricalVectorVariable[T] = variable.CategoricalVectorVariable[T]

  type Diff = variable.Diff
  type DiffList = variable.DiffList

  type DiscreteDomain = variable.DiscreteDomain

  type DiscreteVariable = variable.DiscreteVariable

  type BinaryFeatureVectorVariable[T] = variable.BinaryFeatureVectorVariable[T]
  type FeatureVectorVariable[T] = variable.FeatureVectorVariable[T]
  type HashFeatureVectorVariable = variable.HashFeatureVectorVariable

  type LabeledCategoricalVariable[T] = variable.LabeledCategoricalVariable[T]
  val HammingObjective = variable.HammingObjective

  type RealVariable = variable.RealVariable

  type Cubbie = util.Cubbie

  type GlobalLogging = util.GlobalLogging

  val BinarySerializer = util.BinarySerializer

  val Trainer = optimize.Trainer
  type Example = optimize.Example
  type LikelihoodExample[A<:Iterable[Var],B<:Model] = optimize.LikelihoodExample[A,B]
  type StructuredSVMExample[A<:Iterable[Var]] = optimize.StructuredSVMExample[A]
  val MiniBatchExample = optimize.MiniBatchExample

  type Factor = model.Factor
  type DotFamilyWithStatistics1[A<:TensorVar] = model.DotFamilyWithStatistics1[A]
  type DotFamilyWithStatistics2[A<:TensorVar, B<:TensorVar] = model.DotFamilyWithStatistics2[A, B]
  type DotFamilyWithStatistics3[A<:TensorVar, B<:TensorVar, C<:TensorVar] = model.DotFamilyWithStatistics3[A, B, C]
  type DotFamilyWithStatistics4[A<:TensorVar, B<:TensorVar, C<:TensorVar, D<:TensorVar] = model.DotFamilyWithStatistics4[A, B, C, D]

  type DotTemplateWithStatistics1[A<:TensorVar] = model.DotTemplateWithStatistics1[A]
  type DotTemplateWithStatistics2[A<:TensorVar, B<:TensorVar] = model.DotTemplateWithStatistics2[A, B]
  type DotTemplateWithStatistics3[A<:TensorVar, B<:TensorVar, C<:TensorVar] = model.DotTemplateWithStatistics3[A, B, C]
  type DotTemplateWithStatistics4[A<:TensorVar, B<:TensorVar, C<:TensorVar, D<:TensorVar] = model.DotTemplateWithStatistics4[A, B, C, D]

  type Model = model.Model
  type CombinedModel = model.CombinedModel
  type TemplateModel = model.TemplateModel
  type Parameters = model.Parameters

  val InferByBPChain = infer.InferByBPChain
  val MaximizeByBPChain = infer.MaximizeByBPChain
  val InferByBPTree = infer.InferByBPTree
  val MaximizeByBPTree = infer.MaximizeByBPTree
  val InferByBP = infer.InferByBPLoopyTreewise
  val MaximizeByBP = infer.MaximizeByBPLoopyTreewise

  type GibbsSampler = infer.GibbsSampler
  val Maximize = infer.Maximize
  type MHSampler[T] = infer.MHSampler[T]
  val InferByGibbsSampling = infer.InferByGibbsSampling
  val MaximizeByIteratedConditionalModes = infer.MaximizeByIteratedConditionalModes
} 
