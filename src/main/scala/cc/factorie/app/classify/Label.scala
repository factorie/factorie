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

@DomainInSubclasses
abstract class Label[I<:Instance[This,I],This<:Label[I,This]](labelString:String, val _instance:I) extends cc.factorie.LabelVariable(labelString) {
  this: This =>
  type GetterType <: LabelGetter[I,This];
  class GetterClass extends LabelGetter[I,This]
  type VariableType <: Label[I,This]
  def newGetter = new LabelGetter[I,This]
  def instance: I = _instance // Why was this necessary?  Why didn't simply (val instance:I) above work?
}

class LabelGetter[I<:Instance[ThisLabel,I],ThisLabel<:Label[I,ThisLabel]] extends Getter[ThisLabel] {
  def newInstanceGetter = new InstanceGetter[ThisLabel,I]
  def instance = initOneToOne[I](newInstanceGetter, label => label.instance, (instance:I) => instance.label)
}
