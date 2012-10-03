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

package cc.factorie.app
import cc.factorie._

package object classify {
  
  /** Given a label and a model, return the BinaryFeatureVectorVariable that are the features for classifying this label. */
  def labelToFeatures(label:cc.factorie.LabeledCategoricalVariable[String])(implicit model:Model): BinaryFeatureVectorVariable[String] = {
    for (f <- model.factorsOfClass(Seq(label),classOf[Factor2[_,_]])) {
      if (f._1.isInstanceOf[BinaryFeatureVectorVariable[String]]) return f._1.asInstanceOf[BinaryFeatureVectorVariable[String]]
      else if (f._2.isInstanceOf[BinaryFeatureVectorVariable[String]]) return f._1.asInstanceOf[BinaryFeatureVectorVariable[String]]
    }
    throw new Error("No classification factor of type Factor2[LabelVariable,BinaryFeatureVectorVariable] found.")
  }


}

