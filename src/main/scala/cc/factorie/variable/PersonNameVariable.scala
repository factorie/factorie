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

/**
 * @author John Sullivan
 */
class PersonName(
  val firstNames:BagOfWords,
  val middleNames:BagOfWords,
  val lastNames:BagOfWords,
  val suffixes:BagOfWords) {

  def this() = this(new BagOfWords(), new BagOfWords(), new BagOfWords(), new BagOfWords())

  def ++=(that:PersonName) {
    this.firstNames ++= that.firstNames
    this.middleNames ++= that.middleNames
    this.lastNames ++= that.lastNames
    this.suffixes ++= that.suffixes
  }

  def --=(that:PersonName) {
    this.firstNames --= that.firstNames
    this.middleNames --= that.middleNames
    this.lastNames --= that.lastNames
    this.suffixes --= that.suffixes
  }

  def primaryFirstName = firstNames.topWord
  def primaryMiddleName = middleNames.topWord
  def primaryLastName = lastNames.topWord

}


class PersonNameVariable(firstNames:Iterable[String] = Iterable.empty[String], middleNames:Iterable[String] = Iterable.empty[String], lastNames:Iterable[String] = Iterable.empty[String], suffixes:Iterable[String] = Iterable.empty[String]) extends Var {
  type Value = PersonName

  val value = new PersonName(new BagOfWords(firstNames), new BagOfWords(middleNames), new BagOfWords(lastNames), new BagOfWords(suffixes))

  def add(that:PersonName)(implicit d:DiffList) {
    if(d != null) d += new PersonNameVariableAddBagDiff(that)
    value ++= that
  }

  def remove(that:PersonName)(implicit d:DiffList) {
    if(d != null) d += new PersonNameVariableRemoveBagDiff(that)
    value --= that
  }

  def ++(that:PersonNameVariable)(implicit d:DiffList):PersonNameVariable = {
    val n = new PersonNameVariable()
    n.add(this.value)(d)
    n.add(that.value)(d)
    n
  }

  def --(that:PersonNameVariable)(implicit d:DiffList):PersonNameVariable = {
    val n = new PersonNameVariable()
    n.add(this.value)(d)
    n.remove(that.value)(d)
    n
  }

  case class PersonNameVariableAddBagDiff(added:PersonName) extends Diff {
    def undo() = value.--=(added)

    def redo() = value.++=(added)

    def variable = PersonNameVariable.this
  }

  case class PersonNameVariableRemoveBagDiff(removed:PersonName) extends Diff {
    def undo() = value.++=(removed)

    def redo() = value.--=(removed)

    def variable = PersonNameVariable.this
  }
}
