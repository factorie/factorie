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

package cc.factorie.variable


/** A Variable with a numeric value.
    Note that this does not inherit from VarWithValue[] because this trait
    does not yet commit to the Value type of this Variable.
    See also IntegerVar, RealVar and DoubleVar.
    @author Andrew McCallum */
trait ScalarVar extends Var {
  def intValue: Int
  def doubleValue: Double
}

/** A Variable with a numeric value that can be set with an Int. 
    Note that this does not inherit from MutableVar[] because this trait
    does not yet commit to the Value type of this Variable. 
    See also RealVar and DoubleVar.
    @author Andrew McCallum */
trait MutableIntScalarVar extends ScalarVar {
  def set(newValue:Int)(implicit d:DiffList): Unit
  def intValue_=(newValue:Int)(implicit d:DiffList): Unit = set(newValue)
}

/** A Variable with a numeric value that can be set with an Double. 
    Note that this does not inherit from MutableVar[] because this trait
    does not yet commit to the Value type of this Variable.
    See also RealVar and DoubleVar.
    @author Andrew McCallum */
trait MutableDoubleScalarVar extends ScalarVar {
  def set(newValue:Double)(implicit d:DiffList): Unit
  def doubleValue_=(newValue:Double)(implicit d:DiffList): Unit = set(newValue)
}
