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

package cc.factorie.app.tokenseq
import cc.factorie._

/** Predefined variables and factor templates for applying FACTORIE to sequences of Tokens, each paired with a categorical Label.
    The Token remembers its String 'word', but its variable 'value' is as a BinaryFeatureVectorVariable.
    This package also provides Getters for Tokens and Labels, enabling template building with the tools in cc.factorie.er.
    For example usage see cc.factorie.example.ChainNER1
    @author Andrew McCallum
    @since 0.8
 */
package object labeled {

  def labelEvaluation[L<:LabelVariable[String] with AbstractVarInSeq[L]](labels:Seq[L])(implicit m:Manifest[L]) = new LabelEvaluation[L](labels)
  def segmentEvaluation[L<:LabelVariable[String]](labels:Seq[L])(implicit m:Manifest[L]) = new SegmentEvaluation[L](labels)

}

