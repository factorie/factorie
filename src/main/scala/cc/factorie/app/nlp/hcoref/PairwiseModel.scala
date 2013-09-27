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

package cc.factorie.app.nlp.hcoref
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}
import cc.factorie.variable.LabeledBooleanVariable

class PairwiseLabel(val m1:PairwiseMention, val m2:PairwiseMention, b:Boolean) extends LabeledBooleanVariable(b) {
  def other(m:PairwiseMention): Option[PairwiseMention] = if(m == m1) Some(m2) else if (m == m2) Some(m1) else None
}
trait PairwiseMention extends TokenSpan with Entity {
  val edges = new ArrayBuffer[PairwiseLabel]
  var _head: Token = null
  def headToken: Token = _head
  // this is done for cases where the mention is split across multiple sentence in error (e.g. in reACE: george w. | bush)
  override def sentence = if (_head ne null) headToken.sentence else super.sentence
}
