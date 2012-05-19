///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie
//import java.io.{File,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}
//
//// DiscreteVector* refers to vectors with weights over a domain of multiple "DiscreteValue"s.
//// Discrete* refers to single a DiscreteValue, which can also be seen as a singleton vector.
//
//// For variables that hold one or more discrete value weights in a vector
//
///** A value consisting of one or more discrete values, representable as a vector. 
//    Not that a single "DiscreteValue" is a subclass of this, represented as a "singleton vector",
//    with 1.0 at the value's intValue and 0.0 everywhere else. */
//trait DiscreteVectorValue extends cc.factorie.la.Vector {
//  def domain: DiscreteVectorDomain
//}
//
///** A Domain for variables whose value is a DiscreteVectorValue, which is a Vector that also has a pointer back to its domain.
//    This domain has a non-negative integer size.  The method 'size' is abstract. */
//trait DiscreteVectorDomain extends VectorDomain with ValueType[DiscreteVectorValue] {
//  /** The maximum size to which this domain will be allowed to grow.  
//      The 'size' method may return values smaller than this, however.
//      This method is used to pre-allocate a Template's parameter arrays and is useful for growing domains. */
//  //def size: Int
//  def dimensionSize: Int = dimensionDomain.size  // TODO Get rid of this?
//  def dimensionDomain: DiscreteDomain
//  def dimensionName(i:Int): String = i.toString
//  def freeze(): Unit = dimensionDomain.freeze
//
//  override def save(dirname: String, gzip: Boolean = false) {
//    // TODO: Note that if multiple vectors have same dimension domains, it will be written multiple times
//    dimensionDomain.save(dirname, gzip)
//  }
//
//  override def load(dirname: String, gzip: Boolean = false) {
//    // TODO: Note that the dimensionDomain might get read multiple times
//    if(!dimensionDomain.frozen) dimensionDomain.load(dirname, gzip)
//  }
//}
//
