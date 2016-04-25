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


/** A Categorical domain with enumerated values.  Provides convenient initialization to known values,
    with value members holding those known values.  For example:
    object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value }
    Each of the defined val will have Int type.  Their corresponding String category values 
    will be the name of the variable (obtained through reflection). */
class EnumDomain extends CategoricalDomain[String] {
  /* For all member variables, if its type is Int, set its value to its name, 
     and intern in the Domain.  Usage: 
     object MyLabels extends StringDomain[MyLabel] { val PER, ORG, LOC, O = Value } */
  private def stringFields = this.getClass.getDeclaredFields.filter(f => { f.getType == classOf[Int] })
  private var stringFieldsIterator: Iterator[java.lang.reflect.Field] = _
  // TODO Should this return Int or ValueType? -akm
  def Value: Int = {
    if (stringFieldsIterator == null) stringFieldsIterator = stringFields.iterator
    assert(stringFieldsIterator.hasNext)
    val field = stringFieldsIterator.next()
    //println("StringDomain Value got "+field.getName)
    //checkFields // TODO Re-add this and make sure example/DirichletDemo works
    index(field.getName) // Add it to the index
  } 
  private def checkFields(): Unit = {
    for (field <- stringFields) {
      val fieldName = field.getName
      //println("StringDomain.checkFields "+field+" fieldName="+fieldName)
      //getClass.getMethods.foreach(m => println(m.toString))
      val fieldMethod = getClass.getMethod(fieldName)
      val fieldValue = fieldMethod.invoke(this).asInstanceOf[Int]
      //println("Field "+fieldName+" has value "+fieldValue)
      if (fieldValue != 0 && this.category(fieldValue) != fieldName) throw new Error("Somehow StringDomain category "+fieldName+" got the wrong String value "+fieldValue+" ("+this.category(fieldValue)+").")
    }
  }
}

// TODO Create a TensorEnumDomain? -akm

