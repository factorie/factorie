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

package cc.factorie

// BPInfer is a factory for BPInferencers
// BPInferencer is specific to a model and some variables

// Infer.apply() does the work and returns true on success; 
//  if the inference is complicated and may be incrementally, it may create a Inferencer 
// An Inferencer is specific to a model and some variables and may be run incrementally;
//  it may also contain a Lattice (which is usually a type of GenerativeModel?)

trait Lattice2 extends Model

trait Infer {
  /** Returns true on success, false if this recipe was unable to handle the relevant factors. */
  def apply(variables:Seq[Variable], varying:Seq[Variable], factors:Seq[Factor], qModel:Model): Lattice2 // Abstract implemented in subclasses
  def apply(variables:Seq[Variable], factors:Seq[Factor], qModel:Model): Lattice2 = apply(variables, Nil, factors, qModel)
  def apply(variables:Seq[Variable], factors:Seq[Factor]): Lattice2 = apply(variables, factors, null)
  def apply(variables:Seq[Variable], model:Model): Lattice2 = apply(variables, model.factors(variables))
  def apply(variables:Seq[Variable], model:Model, qModel:Model): Lattice2 = apply(variables, model.factors(variables), qModel)
}
