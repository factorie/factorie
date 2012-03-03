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
import scala.collection.mutable.{Map,ArrayBuffer, HashMap, ListBuffer}
import scala.util.Random
import cc.factorie.la._
import java.io.{File,FileOutputStream,PrintWriter,FileReader,FileWriter,BufferedReader}

// TODO Also make a randomized-representation CategoricalDomain, with hashes?

// CategoricalVector refers to vectors with weights over a domain of multiple "CategoricalValue"s.
// Categorical refers to single a CategoricalValue, which can also be seen as a singleton vector.

/** A value in a CategoricalVectorDomain */
trait CategoricalVectorValue[T] extends DiscreteVectorValue {
  def domain:CategoricalVectorDomain[T]
}

/** Domain for CategoricalVectorVar, e.g. FeatureVectorVariable.
    @author Andrew McCallum */
trait CategoricalVectorDomain[T] extends DiscreteVectorDomain with ValueType[CategoricalVectorValue[T]] {
  thisDomain =>
  type CategoryType = T
  lazy val dimensionDomain: CategoricalDomain[T] = new CategoricalDomain[T] {
    override def filename = thisDomain.filename
  }
  // Use dimensionSize to get the "size" of the vectors belonging to this domain.
}

