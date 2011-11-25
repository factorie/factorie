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

package cc.factorie.app.nlp.ner
import cc.factorie._
import cc.factorie.app.nlp._

object NerLabelDomain extends CategoricalDomain[String]
class NerLabel(initialValue:String) extends LabelVariable(initialValue) {
  def domain = NerLabelDomain
}

class ChainNerLabel(val token:Token, initialValue:String) extends NerLabel(initialValue)
class SpanNerLabel(val span:NerSpan, initialValue:String) extends NerLabel(initialValue)


class NerSpan(doc:Document, labelString:String, start:Int, end:Int)(implicit d:DiffList) extends TokenSpan(doc, start, end) {
  val label = new SpanNerLabel(this, labelString)
  def isCorrect = this.forall(token => token.nerLabel.intValue == label.intValue) &&
    (!hasPredecessor(1) || predecessor(1).nerLabel.intValue != label.intValue) && 
    (!hasSuccessor(1) || successor(1).nerLabel.intValue != label.intValue)
  override def toString = "NerSpan("+length+","+label.categoryValue+":"+this.phrase+")"
}