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

package cc.factorie.app.classify
import cc.factorie._
import cc.factorie.er._

/**Factor between label and observed instance vector */
class LabelInstanceTemplate[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L],im:Manifest[I]) extends TemplateWithDotStatistics2[L,I]()(lm,im) {
  def unroll1(label: L) = Factor(label,label.instance)
  def unroll2(instance: I) = throw new Error("Instance BinaryFeatureVectorVariable shouldn't change")
}

/**Factor between label and observed instance vector */
class SparseLabelInstanceTemplate[L<:Label[I,L],I<:Instance[L,I]](implicit lm:Manifest[L],im:Manifest[I]) extends TemplateWithDotStatistics2[L,I]()(lm,im) with SparseWeights {
  def unroll1(label: L) = Factor(label,label.instance)
  def unroll2(instance: I) = throw new Error("Instance BinaryFeatureVectorVariable shouldn't change")
}
 
