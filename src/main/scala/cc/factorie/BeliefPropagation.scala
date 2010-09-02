/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
  This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
  http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
  This software is provided under the terms of the Eclipse Public License 1.0
  as published by http://www.opensource.org.  For further information,
  see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.collection.SeqLike
import java.util.Arrays
import cc.factorie._

// Very preliminary explorations in inference by belief propagation among discrete variables.
// Not yet finished, and certainly not yet optimized for speed at all.
// This is here mostly to get some idea of what the interfaces might look like.

/**Holds some global definitions for BP.  */
object BeliefPropagation {
  type BPVariable = DiscreteVariable with NoVariableCoordination // Our BP implementation currently only handles these types of Variables
  val normalizeMessages = false
}

/**A factor in a belief propagation lattice used for inference.
Note that an instance of this class is not actually a Template#Factor itself;
but it points to a Template#Factor with its 'factor' member.
@author Andrew McCallum
@author Tim Vieira
 */
abstract class BPFactor(val factor: Factor) {
  type V = BeliefPropagation.BPVariable

  def variables = factor.variables.toList // because factor.variables is not very efficient

  /**Given a variable, return the BPFactors touching it.  This method must be provided by subclasses. */
  def factorsOf(v: Variable): Seq[BPFactor];

  /**Iterate through all combinations of values in Variables given their `SettingIterators */
  private def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.first.hasNext) {vs.first.next; true}
    else if (vs.tail != Nil) {vs.first.reset; vs.first.next; nextValues(vs.tail)}
    else false
  }

  abstract class Message(val v: V) {
    lazy protected val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message: Array[Double] = msg

    def messageCurrentValue: Double = msg(v.intValue)

    var updateCount = 0
  }

  // Message from this factor to Variable v.  Return this so we can say messageTo(v).update.message
  abstract case class MessageTo(override val v: V) extends Message(v) {
    // IterableSettings instances for each of the variables neighboring this BPFactor, except the variable 'v'
    // TODO Don't we need to remove the variables that are not among those we are inferring?
    protected val neighborSettings = variables.filter(v2 => v2 != v && v2.isInstanceOf[V]).map(v2 => v2.asInstanceOf[V].settings).toList
    protected val neighborFactors = factorsOf(v).filter(_.!=(BPFactor.this))

    def updateTreewiseFromLeaves: Unit = {
      // println("WANT TO UPDATE MESSAGE FROM FACTOR=" + BPFactor.this + " TO VAR=" + v + " (FROM LEAVES) " + updateCount)
      if (updateCount > 0) return
      for (n <- neighborSettings) {
        BPFactor.this.messageFrom(n.variable).updateTreewiseFromLeaves
      }
      update
      updateCount += 1
      // println("UPDATED MESSAGE FROM FACTOR=" + BPFactor.this + " TO VAR=" + v + " (FROM LEAVES) " + updateCount)
      // println
    }

    def updateTreewiseToLeaves: Unit = {
      // println("WANT TO UPDATE MESSAGE FROM FACTOR=" + BPFactor.this + " TO VAR=" + v + " (TO LEAVES) " + updateCount)
      if (updateCount > 0) return
      update
      updateCount += 1
      // println("UPDATED MESSAGE FROM FACTOR=" + BPFactor.this + " TO VAR=" + v + " (TO LEAVES) " + updateCount)
      // println

      for (f <- neighborFactors) {
        f.messageFrom(v).updateTreewiseToLeaves
      }
    }

    def update: Unit
  }

  // TODO: Have "SumProductMessageTo" to normalize and avoid sumLogProb, and also "SumProductLogMessageTo" which does not normalize and uses sumLogProb
  case class SumProductMessageTo(override val v: V) extends MessageTo(v) {
    /**Do one step of belief propagation for the message from this BPFactor to variable 'v' */
    def update = {
      forIndex(v.domain.size)(i => { // Consider reversing the nested ordering of this loop and the inner one
        v.set(i)(null) // Note: this is changing the value of this Variable
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          msg(i) = factor.statistic.score
        } else { // This factor has variable neighbors in addition to v itself
          // Sum over all combinations of values in neighboring variables with v's value fixed to i.
          neighborSettings.foreach(setting => {setting.reset; setting.next}) // reset iterator and advance to first setting.
          msg(i) = Math.NEG_INF_DOUBLE // i.e. log(0)
          do {
            msg(i) = Maths.sumLogProb(msg(i), factor.statistic.score + neighborSettings.sumDoubles(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue))
          } while (nextValues(neighborSettings))
        }
      })
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
    }
  }

  case class MaxProductMessageTo(override val v: V) extends MessageTo(v) {
    /*lazy protected val maxIndex = new Array[Int](v.domain.size) */ // Holds how many nextValues calls it takes to get to max value
    def update = {
      forIndex(v.domain.size)(i => { // Consider reversing the nested ordering of this loop and the inner one
        v.set(i)(null) // Note: that this is changing the Variable value
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          msg(i) = factor.statistic.score
          //maxIndex(i) = -1
        } else { // This factor has variable neighbors in addition to v itself
          neighborSettings.foreach(setting => {setting.reset; setting.next})
          msg(i) = Math.NEG_INF_DOUBLE
          //maxIndex(i) = -1
          //var settingCount = 0
          do {
            val score = factor.statistic.score + neighborSettings.sumDoubles(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue)
            if (score > msg(i)) {msg(i) = score; /*maxIndex(i) = settingCount*/ }
            //settingCount += 1
          } while (nextValues(neighborSettings))
        }
      })
    }
  }

  /**Message from Variable v to this factor. */
  case class MessageFrom(override val v: V) extends Message(v) {
    val neighborFactors = factorsOf(v).filter(_.!=(BPFactor.this))
    protected val neighborSettings = variables.filter(v2 => v2 != v && v2.isInstanceOf[V]).map(v2 => v2.asInstanceOf[V].settings).toList

    def updateTreewiseFromLeaves: Unit = {
      // println("WANT TO UPDATE MESSAGE FROM VAR=" + v + " TO FACTOR=" + BPFactor.this + " (FROM LEAVES) " + updateCount)
      if (updateCount > 0) return
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        msg2.updateTreewiseFromLeaves
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
      updateCount += 1
      // println("UPDATED MESSAGE FROM VAR=" + v + " TO FACTOR=" + BPFactor.this + " (FROM LEAVES) " + updateCount)
      // println
    }

    def updateTreewiseToLeaves: Unit = {
      // println("WANT TO UPDATE MESSAGE FROM VAR=" + v + " TO FACTOR=" + BPFactor.this + " (TO LEAVES) " + updateCount)
      if (updateCount > 0) return
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
      updateCount += 1

      for (n <- neighborSettings) {
        BPFactor.this.messageTo(n.variable).updateTreewiseToLeaves
      }

      // println("UPDATED MESSAGE FROM VAR=" + v + " TO FACTOR=" + BPFactor.this + " (TO LEAVES) " + updateCount)
      // println
    }

    def update = {
      if (neighborFactors.size > 0)
        for (i <- 0 until v.domain.size) {
          msg(i) = neighborFactors.sumDoubles(_.messageTo(v).message(i))
        }
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
    }
  }

  /* For Sum-Product */
  //lazy private val _msgTo: Array[MessageTo] = this.variables.filter(_.isInstanceOf[V]).map(v => MessageTo(v.asInstanceOf[V])).toSeq.toArray
  /* For Max-Product: */
  lazy private val _msgTo: Seq[MessageTo] = this.variables.filter(_.isInstanceOf[V]).map(v => SumProductMessageTo(v.asInstanceOf[V])).toSeq
  lazy private val _msgFrom: Seq[MessageFrom] = this.variables.filter(_.isInstanceOf[V]).map(v => MessageFrom(v.asInstanceOf[V])).toSeq

  def messageTo(v: V): MessageTo = messageTo(this.variables.toSeq.indexOf(v))

  def messageTo(vi: Int): MessageTo = _msgTo(vi)

  def messageFrom(v: V): MessageFrom = messageFrom(this.variables.toSeq.indexOf(v))

  def messageFrom(vi: Int): MessageFrom = _msgFrom(vi)

  def update: Unit = {_msgFrom.foreach(_.update); _msgTo.foreach(_.update); } // TODO swap order?

  def updateTreewise: Unit = {
    // TODO This was causing a compile error; BPFactors don't have an updateCount.
    // It seems like this isn't necessary, since we already check the message
    // update count above?
    _msgFrom.foreach(message => if (message.updateCount == 0) {message.updateTreewiseFromLeaves}) // TODO msgFrom?  msgTo?
    // println("Done FROM LEAVES")
    _msgTo.foreach(message => if (message.updateCount == 0) {message.updateTreewiseToLeaves})
    // println("Done TO LEAVES")
    //}
  }

  def resetTree: Unit = {
    _msgTo.foreach(_.updateCount = 0)
    _msgFrom.foreach(_.updateCount = 0)
  }

  /**Not the overall marginal over the variable; just this factor's marginal over the variable */
  def marginal(v: V): Array[Double] = {
    val result = new Array[Double](v.domain.size)
    Array.copy(messageTo(v).message, 0, result, 0, result.length)
    Maths.expNormalize(result)
    result
  }

  def marginal: Array[Double] = {
    val dim = this.variables.multiplyInts(_.asInstanceOf[V].domain.size)
    val variableSettings = this.variables.map(v => v.asInstanceOf[V].settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    val result = new Array[Double](dim)
    var i = 0
    do {
      result(i) = factor.statistic.score + variableSettings.sumDoubles(s => BPFactor.this.messageFrom(s.variable).messageCurrentValue)
      i += 1
    } while (nextValues(variableSettings))
    Maths.expNormalize(result)
    result
  }

  // TODO: optimize for speed; iterating over settings twice
  def marginalMap: HashMap[List[Int], Double] = {
    val result = this.marginal
    val variableSettings = this.variables.map(v => v.asInstanceOf[V].settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    val map = new HashMap[List[Int], Double]
    var i = 0
    do {
      val varSetting = variableSettings.map(s => s.variable.asInstanceOf[V].intValue).toList
      map += varSetting -> result(i)
      i += 1
    } while (nextValues(variableSettings))
    map
  }

  def logZ: Double = {
    val variableSettings = this.variables.map(v => v.asInstanceOf[V].settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    var result = Math.NEG_INF_DOUBLE
    do {
      val score = factor.statistic.score + variableSettings.sumDoubles(s => BPFactor.this.messageFrom(s.variable).messageCurrentValue)
      result = Maths.sumLogProb(result, score)
    } while (nextValues(variableSettings))
    result
  }
}

trait DiscreteMarginalN {
  /**The settings of each of the N variables that together yield the highest probability. */
  def maxEntry: Array[Int]
}
// TODO: This should really inherit from Proportions
class DiscreteMarginal1[V <: DiscreteVar](val variable: V) extends RandomAccessSeq[Double] with DiscreteMarginalN {
  def this(v: V, messages: Iterable[Array[Double]]) = {
    this (v);
    for (message <- messages) {
      assert(message.length == m.length)
      for (i <- 0 until m.length) {m(i) += message(i); sum += message(i)}
    }
    assert(length == v.domain.size)
    Maths.expNormalize(m)
  }

  private val m = new Array[Double](variable.domain.size)
  private var sum = 0.0

  def length = m.length

  def apply(i: Int) = m(i)

  def maxEntry = {
    var i = 0;
    var mv = m(i);
    var mi = i
    while (i < m.length) {if (mv < m(i)) {mv = m(i); mi = i}; i += 1}
    val result = new Array[Int](1);
    result(0) = mi;
    result
  }

  def maxIndex = {
    var i = 0;
    var mv = m(i);
    var mi = i
    while (i < m.length) {if (mv < m(i)) {mv = m(i); mi = i}; i += 1}
    mi
  }

  override def toString: String = {
    val sb = new StringBuffer
    for (i <- 0 until length) sb.append("%d=%-6f ".format(i, m(i)))
    // TODO: Make DiscreteDomain have index and lookup, with Int values, so that the above line will work nicely for CategoricalDomains also
    sb.toString
  }
}

// TODO Rename "SumProductLattice" and "MaxProductLattice"
class BPLattice(val variables: Collection[BeliefPropagation.BPVariable]) extends Lattice {
  type V = BeliefPropagation.BPVariable

  // Find all the factors touching the 'variables'
  val factors = new HashSet[Factor]

  def this(model: Model, variables: Collection[BeliefPropagation.BPVariable]) {
    this (variables)
    for (factor <- model.factorsOf[Template](variables)) addFactor(factor)
  }

  def bpFactorsOf(v: V) = v2m(v)

  // Data structure for holding mapping from Variable to the collection of BPFactors that touch it
  private val v2m = new HashMap[Variable, ArrayBuffer[BPFactor]] {override def default(v: Variable) = {this(v) = new ArrayBuffer[BPFactor]; this(v)}}
  // Create a BPFactor for each factor
  val marginals = new HashMap[Factor, BPFactor]

  def addFactor(factor: Factor) = {
    val bpFactor = new BPFactor(factor) {def factorsOf(v: Variable) = v2m(v)}
    marginals(factor) = bpFactor
    for (v <- factor.variables) v2m(v) += bpFactor
    factors += factor
  }

  /**Perform one iteration of belief propagation. */
  def update: Unit = marginals.values.foreach(_.update)

  /**Perform N iterations of belief propagation */
  def update(iterations: Int): Unit = for (i <- 1 to iterations) update

  /**Send each message in the lattice once, in order determined by a random tree traversal. */
  def updateTreewise(shuffle: Boolean = false): Unit = {
    marginals.values.foreach(_.resetTree)
    val factors = if (shuffle) marginals.values.toList.shuffle else marginals.values.toList // optionally randomly permute order, ala TRP
    // If the graph is fully connected, "updateTreewise" on the first marginal will do the entire graph, and the other calls will return immediately
    factors.foreach(_.updateTreewise)

    // check that our "randomized tree traversal" touched everything (to run change _msgTo and _msgFrom to public)
    //assert(v2m.values.toList.forall(_.forall(x =>
    //     x._msgTo.forall(_.visitedDuringThisTree) &&  x._msgFrom.forall(_.visitedDuringThisTree)
    //)))
  }

  /**Provide outside access to a BPFactor given is associated Factor */
  def marginal(f: Factor): Array[Double] = marginals(f).marginal

  def marginalMap(f: Factor): HashMap[List[Int], Double] = marginals(f).marginalMap

  def marginal(v: V): DiscreteMarginal1[V] = new DiscreteMarginal1(v, bpFactorsOf(v).map(_.messageTo(v).message))
  /* def sample(v:UncoordinatedCategoricalVariable): DiffList  // TODO: implement this */
  /* def sample: Unit   // TODO: implement this */
  def setVariablesToMarginalMax: Unit = variables.foreach(v => v.set(marginal(v).maxIndex)(null))

  def setVariablesToMarginalMax(vs: Iterable[V]): Unit = vs.foreach(v => v.set(marginal(v).maxIndex)(null))

  def setVariablesToMax: Unit = variables.foreach(v => v.set(marginal(v).maxIndex)(null))

  def setVariablesToMax(vs: Iterable[V]): Unit = vs.foreach(v => v.set(marginal(v).maxIndex)(null))

  def sumLogZ: Double = {
    val factorsTouched = new HashSet[BPFactor]
    val factors = marginals.values.toArray
    var result = 0.0
    factors.foreach {
      f: BPFactor =>
        if (!factorsTouched.contains(f)) {
          result += f.logZ

          // do BFS search
          var bfsStack = new ArrayBuffer[BPFactor]
          bfsStack += f
          while (bfsStack.size > 0) {
            val currf = bfsStack.remove(0)
            factorsTouched += currf
            currf.variables.foreach {
              v => v2m(v).filter(!factorsTouched.contains(_)).foreach {
                otherf => bfsStack += otherf
              }
            }
          }
        }
    }
    result
  }
}
