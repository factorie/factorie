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
import scala.util.Random
import cc.factorie.la._

trait DiscretesVar extends VectorVar with VarAndValueType[DiscretesVar,DiscretesValue] {
  def domain: DiscreteVectorDomain
  def contains(dimension:Int): Boolean = vector.apply(dimension) != 0.0
}

/** A vector with dimensions corresponding to a DiscreteDomain, and with Double weights for each dimension, represented by a sparse vector. */
abstract class DiscretesVariable extends VectorVariable with DiscretesVar {
  thisVariable =>
  _set(new SparseVector(domain.maxVectorLength) with DiscretesValue { def domain = thisVariable.domain })
}

/** A sparse binary vector with length determined by a DiscreteDomain */
trait SparseBinaryDiscretesVar extends DiscretesVar with VarAndValueType[SparseBinaryDiscretesVar,SparseBinaryVector with DiscretesValue] {
  //type ValueType <: cc.factorie.la.SparseBinaryVector with DiscretesValue
  def length = domain.maxVectorLength //allocSize // TODO Remove this?
  //def apply(i:Int) = vector.apply(i)
  def activeDomain = vector.activeDomain
  def zero(): Unit = value.zero()
  def +=(i:Int): Unit = { if (frozen) throw new Error("Cannot append to frozen SparseBinaryDiscretesVar."); value.include(i) }
  //def ++=(is:Iterable[Int]): Unit = is.foreach(i => vector.+=(i)) // Conflicts with ++=(Iterable[T])
  var frozen = false
  def freeze() = frozen = true
  override def isConstant = frozen
}

abstract class SparseBinaryDiscretesVariable extends VectorVariable with SparseBinaryDiscretesVar {
  thisVariable =>
  //type DomainType = DiscreteVectorDomain
  //type ValueType = cc.factorie.la.SparseBinaryVector with DiscretesValue
  _set(new cc.factorie.la.SparseBinaryVector(-1) with DiscretesValue {
    def domain = thisVariable.domain
    override def length = domain.maxVectorLength
  })
}



/** A single discrete variable */
trait DiscreteVar extends DiscretesVar with VarAndValueType[DiscreteVar,DiscreteValue] {
  def domain: DiscreteDomain
  /*@inline final*/ def intValue = value.index
  @deprecated("Will be removed in the future")
  def activeDomain = List(intValue) // TODO try to make this implementation possible: = value
}

// TODO Note that DiscreteVariable is not a subclass of VectorVariable, due to initialization awkwardness.
// Consider fixing this.
/** A Variable holding a single DiscreteValue. */
abstract class DiscreteVariable extends VectorVariable with DiscreteVar with IterableSettings with QDistribution {
  // The base constructor must take no arguments because CategoricalVariable needs to create with a temporary value and do the lookup later.
  def this(initialInt:Int) = { this(); _set(domain.getValue(initialInt)) /*.asInstanceOf[Value]*/ } // TODO Get rid of this cast?
  //private var _value: ValueType = null.asInstanceOf[ValueType]
  //def value: Value = _value
  //protected def _set(newValue:ValueType): Unit = _value = newValue
  def set(newValue:ValueType)(implicit d:DiffList): Unit = if (newValue ne value) {
    assert((newValue eq null) || newValue.domain == domain)
    if (d ne null) d += new DiscreteVariableDiff(value, newValue)
    _set(newValue)
  }
  /** You should never call this yourself.  Only used in CategoricalVariable initialization code. */
  //protected def _set(newValue:Value) = { assert(newValue ne null); _value = newValue }
  // TODO provide default value for DiffList = null
  def set(newInt:Int)(implicit d:DiffList): Unit = set(domain.getValue(newInt).asInstanceOf[ValueType])(d) // TODO Get rid of cast?
  def setRandomly(random:Random = cc.factorie.random, d:DiffList = null): Unit = set(random.nextInt(domain.size))(d)
  def settings = new SettingIterator {
    // TODO Base this on a domain.iterator instead, for efficiency
    var i = -1
    val max = domain.size - 1
    def hasNext = i < max
    def next(difflist:DiffList) = { i += 1; val d = newDiffList; set(i)(d); d }
    def reset = i = -1
    override def variable: DiscreteVariable.this.type = DiscreteVariable.this
  }
  case class DiscreteVariableDiff(oldValue: ValueType, newValue: ValueType) extends Diff {
    @inline final def variable: DiscreteVariable = DiscreteVariable.this
    @inline final def redo = _set(newValue)
    @inline final def undo = _set(oldValue)
    override def toString = "DiscreteVariableDiff("+oldValue+","+newValue+")"
      /*variable match { 
       case cv:CategoricalVar[_] if (oldIndex >= 0) => "DiscreteVariableDiff("+cv.domain.get(oldIndex)+"="+oldIndex+","+cv.domain.get(newIndex)+"="+newIndex+")"
       case _ => "IntegerVariableDiff("+oldIndex+","+newIndex+")"
      } */
  }
  // TODO But then this choice cannot be changed by subclasses :-(  Consider some implicit configuration instead.
  // So define a QType[+QT] trait!
  type QType = cc.factorie.generative.MutableProportions  // TODO Change this = to <:  Why wasn't this done before?
  def newQ = new cc.factorie.generative.DenseProportions(domain.size)
}

/** A collection of DiscreteVariables that can iterate over the cross-product of all of their values.  May be useful in the future for block-Gibbs-sampling?
    @author Andrew McCallum */
@deprecated("This will likely be removed in a future version.")
class DiscreteVariableBlock(vars:DiscreteVariable*) extends Variable with Seq[DiscreteVariable] with IterableSettings with VarAndValueGenericDomain[DiscreteVariableBlock,List[Int]] {
  def value = _vars.map(_.intValue)
  private val _vars = vars.toList
  def length = _vars.length
  def apply(index:Int) = _vars.apply(index)
  def iterator = _vars.iterator
  override def foreach[U](f:(DiscreteVariable)=>U): Unit = _vars.foreach(f)
  def settings: SettingIterator = new SettingIterator {
    var i = -1
    val n = _vars.length
    val s = _vars.map(_.domain.size).toArray
    val max = s.foldLeft(1)(_*_)
    def hasNext = i < max
    def next(difflist:DiffList) = throw new Error // TODO Implement this properly { i += 1; val d = newDiffList; _vars(i%n).set(i/n)(d); d }
    def reset = i = -1
  }
}
