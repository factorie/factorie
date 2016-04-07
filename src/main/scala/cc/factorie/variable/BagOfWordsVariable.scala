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

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

/**
 * @author John Sullivan
 */
@SerialVersionUID(1)
class BagOfWords(initialWords: Iterable[String] = null, initialBag: Map[String, Double] = null) extends Serializable{

  def longest = _bag.keysIterator.toSeq.sortBy(_.length).lastOption.getOrElse("")
  def topWord = _bag.toSeq.sortBy(_._2).lastOption.map(_._1).getOrElse("")

  def topBag(w:Int) = _bag.toSeq.sortBy(-_._2).take(w)
  def topWords(w:Int) = topBag(w).map(_._1)


  var variable: BagOfWordsVariable = null
  protected var _l2Norm = 0.0
  protected var _l1Norm = 0.0
  protected var _bag = new LinkedHashMap[String, Double]
  def clear(): Unit = {
    _l2Norm = 0.0
    _l1Norm = 0.0
    _bag = new LinkedHashMap[String, Double]
  }
  def cosineSimilarity(that: BagOfWords, deduct: BagOfWords): Double = {
    val numerator: Double = this.deductedDot(that, deduct)
    if (numerator != 0.0) {
      val thatL2Norm = Math.sqrt(deduct.l2Norm * deduct.l2Norm + that.l2Norm * that.l2Norm - 2 * (deduct * that))
      val denominator: Double = this.l2Norm * thatL2Norm
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }
  def cosineSimilarity(that: BagOfWords): Double = {
    val numerator: Double = this * that
    val denominator: Double = this.l2Norm * that.l2Norm
    if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
  }
  def ++=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this +=(s, w)
  def --=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this -=(s, w)

  def sizeHint(n: Int) = _bag.sizeHint(n)
  if (initialWords != null) for (w <- initialWords) this +=(w, 1.0)
  if (initialBag != null) for ((k, v) <- initialBag) this +=(k, v)
  def l2Norm = scala.math.sqrt(_l2Norm)
  def l1Norm = _l1Norm
  def asHashMap = _bag
  override def toString = _bag.toString()
  def apply(s: String): Double = _bag.getOrElse(s, 0.0)
  def contains(s: String): Boolean = _bag.contains(s)
  def size = _bag.size
  def iterator = _bag.iterator
  def *(that: BagOfWords): Double = {
    if (that.size < this.size) return that * this
    var result = 0.0
    for ((k, v) <- iterator) result += v * that(k)
    result
  }
  def deductedDot(that: BagOfWords, deduct: BagOfWords): Double = {
    var result = 0.0
    if (deduct eq this) for ((k, v) <- iterator) result += v * (that(k) - v)
    else for ((k, v) <- iterator) result += v * (that(k) - deduct(k))
    result
  }
  def +=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      //if(w!=1.0)println("  add: "+w)
      _l1Norm += w
      _l2Norm += w * w + 2 * this(s) * w
      _bag(s) = _bag.getOrElse(s, 0.0) + w
    }
  }
  def -=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      _l1Norm -= w
      _l2Norm += w * w - 2.0 * this(s) * w
      //if(w!=1.0)println("  remove: "+w)
      if (_bag.contains(s) && withinEpsilon(w, _bag(s))) _bag.remove(s)
      else _bag(s) = _bag.getOrElse(s, 0.0) - w
    }
  }
  @inline final def withinEpsilon(v1: Double, v2: Double, epsilon: Double = 0.000001): Boolean = if (v1 == v2) true else math.abs(v1 - v2) <= epsilon
  def addBag(that: BagOfWords) = {
    for ((k, v) <- that.iterator) this +=(k, v)
  }
  def removeBag(that: BagOfWords) = for ((k, v) <- that.iterator) this -=(k, v)
  def contains(other:BagOfWords) = this._bag.keySet.intersect(other._bag.keySet).size > 0

  def l2Normalize = _bag.mapValues(_ / l2Norm).toMap
  def l1Normalize = _bag.mapValues(_ / l2Norm).toMap
}

class BagOfWordsVariable(initialWords: Iterable[String] = Nil, initialMap: Map[String, Double] = null) extends Var with Iterable[(String, Double)] {
  // Note that the returned value is not immutable.
  type Value = BagOfWords
  def value = _members
  def clear() = _members.clear
  protected val _members: BagOfWords = {
    val result = new BagOfWords(initialWords)
    if (initialMap != null) for ((k, v) <- initialMap) result +=(k, v)
    result.variable = this
    result
  }
  def immutableMap:scala.collection.immutable.Map[String, Double] = _members.asHashMap.toMap
  def members: BagOfWords = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x: String) = _members.contains(x)
  def accept(): Unit = {}
  def add(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddStringDiff(x, w)
    _members +=(x, w)
  }
  def remove(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveStringDiff(x, w)
    _members -=(x, w)
  }
  def add(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddBagDiff(x)
    _members.addBag(x)
  }
  def remove(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveBagDiff(x)
    _members.removeBag(x)
  }
  final def +=(x: String, w: Double = 1.0): Unit = add(x, w)(null)
  final def -=(x: String, w: Double = 1.0): Unit = remove(x, w)(null)
  final def +=(x: BagOfWords): Unit = add(x)(null)
  final def -=(x: BagOfWords): Unit = remove(x)(null)
  final def ++=(xs: Iterable[String]): Unit = xs.foreach(add(_)(null))
  final def --=(xs: Iterable[String]): Unit = xs.foreach(remove(_)(null))
  final def ++=(xs: Map[String, Double]): Unit = for ((k, v) <- xs) add(k, v)(null)
  final def --=(xs: Map[String, Double]): Unit = for ((k, v) <- xs) remove(k, v)(null)

  def ++(that:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
    val n = new BagOfWordsVariable()
    this.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    that.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    n
  }

  def --(that:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
    val n = new BagOfWordsVariable()
    this.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    that.foreach{ case(word, value) =>
      n.remove(word,value)(d)
    }
    n
  }


  case class BagOfWordsVariableAddStringDiff(added: String, w: Double) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members +=(added, w)
    def undo() = _members -=(added, w)
    override def toString = "BagOfWordsVariableAddStringDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveStringDiff(removed: String, w: Double) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members -=(removed, w)
    def undo() = _members +=(removed, w)
    override def toString = "BagOfWordsVariableRemoveStringDiff of " + removed + " from " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableAddBagDiff(added: BagOfWords) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.addBag(added)
    def undo() = _members.removeBag(added)
    override def toString = "BagOfWordsVariableAddBagDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveBagDiff(removed: BagOfWords) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.removeBag(removed)
    def undo() = _members.addBag(removed)
    override def toString = "BagOfWordsVariableRemoveBagDiff of " + removed + " from " + BagOfWordsVariable.this
  }
}

object BagOfWordsVariable {
  implicit object IterStringDoubleBagBuilder extends CanBuildFrom[Iterable[(String, Double)], (String, Double), BagOfWordsVariable] {
    def apply(from: Iterable[(String, Double)]):mutable.Builder[(String, Double), BagOfWordsVariable] = apply()

    def apply() =
      new mutable.Builder[(String, Double), BagOfWordsVariable] {
        private val bag = new BagOfWordsVariable()
        def +=(elem: (String, Double)) = {
          bag.+=(elem._1, elem._2)
          this
        }

        def result() = bag

        def clear() {bag.clear()}
      }
  }
  implicit object MapBagBuilder extends CanBuildFrom[Map[String, Double], (String, Double), BagOfWordsVariable] {
    def apply(from: Map[String, Double]) = IterStringDoubleBagBuilder(from)

    def apply() = IterStringDoubleBagBuilder()
  }
  
  implicit object IterStringBagBuilder extends CanBuildFrom[Iterable[String], String, BagOfWordsVariable] {
    def apply(from: Iterable[String]) = apply()

    def apply() = new mutable.Builder[String, BagOfWordsVariable] {
      private val bag = new BagOfWordsVariable
      def +=(elem: String): this.type = {bag += elem; this}

      def result() = bag

      def clear() {bag.clear()}
    }
  }

  def toBagOfWords[A](a:A)(implicit cbf:CanBuildFrom[A, _, BagOfWordsVariable]):BagOfWordsVariable = cbf(a).result()
}