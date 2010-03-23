/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie.util.Implicits._
import java.util.Arrays
import cc.factorie.util.LinkedHashSet

// Very preliminary explorations in inference by belief propagation among discrete variables.
// Not yet finished, and certainly not yet optimized for speed at all.
// This is here mostly to get some idea of what the interfaces might look like.

/** Holds some global definitions for BP.  */
object BeliefPropagation {
  type BPVariable = UncoordinatedDiscreteVariable  // Our BP implementation currently only handles these types of Variables
  var maxdiff = 0.0  // maximum message difference (used for checking if messages are converging)
  val normalizeMessages = true
}

object TreewiseBookkeeping {
  /* TIM: This is required by my temporary solution for performing the reverse-order tree-wise
   * traversal in updateTreewise. It would be nice for this not to be shared by all instances
   * of BPLattice, because this means you can't run multiple instances of BP in parallel. */
  var stack = new ArrayBuffer[BPFactor#Message]
}

/** A factor in a belief propagation lattice used for inference.
    Note that an instance of this class is not actually a Template#Factor itself;
    but it points to a Template#Factor with its 'factor' member.

    @author Andrew McCallum
    @author Tim Vieira
 */
abstract class BPFactor(val factor:Factor) {
  type V = BeliefPropagation.BPVariable

  /** Given a variable, return the BPFactors touching it.  This method must be provided by subclasses. */
  def factorsOf(v:Variable): Seq[BPFactor];

  /** Iterate through all combinations of values in Variables given their `SettingIterators */
  private def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.first.hasNext) {vs.first.next; true}
    else if (vs.tail != Nil) {vs.first.reset; vs.first.next; nextValues(vs.tail)}
    else false
  }

  abstract class Message(v:V) {
    type MsgType = Message
    lazy protected val msg = new Array[Double](v.domain.size)          // Holds unnormalized log-probabilities
    lazy protected val previous_msg = new Array[Double](v.domain.size) // store previous message so we can look at message convergence
    def msgDiff = Maths.L2(message, previous_msg)
    def message: Array[Double] = msg                                   // TODO: Consider making these scalala Vectors
    def messageCurrentValue: Double = msg(v.intValue)

    /*
     Do one step of belief propagation for the message from this BPFactor to variable 'v'
      - ensure variable's value is restored (if case it was changed)
      - automatically store previous message
      - track maximum difference between messages
      - normalize messages if option is set.
      - returns reference to `this` */
    final def update: this.type = {
      val origIndex = v.index
      // update2 will change msg, update2 might look at previous message so don't change it, so save it in a copy.
      val b4 = new Array[Double](msg.size); msg.copyToArray(b4, 0)
      update2
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
      b4.copyToArray(previous_msg, 0)  // copy previous msg
      BeliefPropagation.maxdiff = BeliefPropagation.maxdiff.max(msgDiff)  // track largest change in a message
      v.setByIndex(origIndex)(null)    // restore orginal value, just in case someone cares
      this   // Returns reference to `this` so we can say messageTo(v).update.message
    }
    protected def update2: Unit

    /*
     Update this message, but first update the messages its depends on, avoiding update loops.
      - automatically manage the visitedDuringThisTree conditions
      - manage the update order stack (`TreewiseBookkeeping.stack`).
      - does NOT automatically normalize */
    final def updateTreewise: this.type = {
      if (visitedDuringThisTree) return this
      visitedDuringThisTree = true
      TreewiseBookkeeping.stack += this
      updateTreewise2
      this
    }
    var visitedDuringThisTree = false
    protected def updateTreewise2: Unit
  }

  // Message from this factor to Variable v.
  // Returns reference to `this` so we can say messageTo(v).update.message
  abstract case class MessageTo(v:V) extends Message(v) {
    // IterableSettings instances for each of the variables neighboring this BPFactor, except the variable 'v'
    protected val neighborSettings = factor.variables.filter(v2 => v2 != v && v2.isInstanceOf[V]).map(v2 => v2.asInstanceOf[V].settings).toList
    def updateTreewise2 = {
      for (n <- neighborSettings) {
        BPFactor.this.messageFrom(n.variable).updateTreewise
      }
      update
    }
  }

  case class SumProductMessageTo(override val v:V) extends MessageTo(v) {
    /** Do one step of belief propagation for the message from this BPFactor to variable 'v' */
    def update2 = {
      for (i <- 0 until v.domain.size) {   // Consider reversing the nested ordering of this loop and the inner one
        v.setByIndex(i)(null)              // Note: this is changing the value of this Variable
        if (neighborSettings.size == 0) {  // This factor has only one variable neighbor, v itself
          msg(i) = factor.statistic.score
        } else {                           // This factor has variable neighbors in addition to v itself
          // Sum over all combinations of values in neighboring variables with v's value fixed to i.
          neighborSettings.foreach(setting => {setting.reset; setting.next}) // reset iterator and advance to first setting.
          msg(i) = Math.NEG_INF_DOUBLE     // i.e. log(0)
          do {
            msg(i) = Maths.sumLogProb(msg(i), factor.statistic.score + neighborSettings.sumDoubles(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue))
          } while (nextValues(neighborSettings))
        }
      }
    }
  }

  case class MaxProductMessageTo(override val v:V) extends MessageTo(v) {
    /*lazy protected val maxIndex = new Array[Int](v.domain.size) */ // Holds how many nextValues calls it takes to get to max value
    def update2 = {
      for (i <- 0 until v.domain.size) {  // Consider reversing the nested ordering of this loop and the inner one
        v.setByIndex(i)(null)             // Note: that this is changing the Variable value
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
            if (score > msg(i)) { msg(i) = score; /*maxIndex(i) = settingCount*/ }
            //settingCount += 1
          } while (nextValues(neighborSettings))
        }
      }
    }
  }

  /** Message from Variable v to this factor. */
  case class MessageFrom(v:V) extends Message(v) {
    val neighborFactors = factorsOf(v).filter(_.!=(BPFactor.this))
    def update2 = {
      if (neighborFactors.size > 0)
        for (i <- 0 until v.domain.size) {
          msg(i) = neighborFactors.sum(_.messageTo(v).message(i))
        }
    }
    def updateTreewise2 = {
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        msg2.updateTreewise
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      if (BeliefPropagation.normalizeMessages) Maths.normalizeLogProb(msg)
    }
  }

  /* For Sum-Product */
  //lazy private val _msgTo: Array[MessageTo] = factor.variables.filter(_.isInstanceOf[V]).map(v => MessageTo(v.asInstanceOf[V])).toSeq.toArray
  /* For Max-Product: */
  lazy private val _msgTo: Array[MessageTo] = factor.variables.filter(_.isInstanceOf[V]).map(v => MaxProductMessageTo(v.asInstanceOf[V])).toSeq.toArray
  lazy private val _msgFrom: Array[MessageFrom] = factor.variables.filter(_.isInstanceOf[V]).map(v => MessageFrom(v.asInstanceOf[V])).toSeq.toArray
  def messageTo(v:V): MessageTo = messageTo(factor.variables.toSeq.indexOf(v))
  def messageTo(vi:Int): MessageTo = _msgTo(vi)
  def messageFrom(v:V): MessageFrom = messageFrom(factor.variables.toSeq.indexOf(v))
  def messageFrom(vi:Int): MessageFrom = _msgFrom(vi)

  def update: Unit = { _msgTo.foreach(_.update); _msgFrom.foreach(_.update);  }

  def updateTreewise: Unit = {
    _msgTo.foreach(_.updateTreewise)
    _msgFrom.foreach(_.updateTreewise)
  }
  def resetTree: Unit = {
    _msgTo.foreach(_.visitedDuringThisTree = false)
    _msgFrom.foreach(_.visitedDuringThisTree = false)
  }
  /** Not the overall marginal over the variable; just this factor's marginal over the variable */
  def marginal(v:V): Array[Double] = {
    val result = new Array[Double](v.domain.size)
    messageTo(v).message.copyToArray(result, 0)  // In Scala 2.8 there will be a more compact way to do this.
    Maths.normalize(result)
    result
  }
  def marginal: Array[Double] = {
    val dim = factor.variables.productInts(_.asInstanceOf[V].domain.size)
    val m = new Array[Double](dim)
    throw new Error("Not yet implemented")
    //_msgFrom.foreach(m => )
    m
  }
  //def neighborDistribution: Array[Double];
  //def sufficientDistribution: Array[Double]
}

trait DiscreteMarginalN {
  /** The settings of each of the N variables that together yield the highest probability. */
  def maxEntry: Array[Int]
}
// TODO: This should really inherit from Multinomial, but first I have to make it not require an "Outcome" variable.
// I should also make a multi-dimensional multinomial.
class DiscreteMarginal1[V<:DiscreteValue](val variable:V) extends RandomAccessSeq[Double] with DiscreteMarginalN {
  def this(v:V, messages:Iterable[Seq[Double]]) = {
    this(v);
    for (message <- messages) {
      assert(message.length == m.length)
      for (i <- 0 until m.length) { m(i) += message(i); sum += message(i) }
    }
    assert(length == v.domain.size)
    Maths.expNormalize(m)
  }
  private val m = new Array[Double](variable.domain.size)
  private var sum = 0.0
  def length = m.length
  def apply(i:Int) = m(i)
  def maxEntry = {
    var i = 0; var mv = m(i); var mi = i
    while (i < m.length) { if (mv < m(i)) { mv = m(i); mi = i }; i += 1 }
    val result = new Array[Int](1); result(0) = mi; result
  }
  def maxIndex = {
    var i = 0; var mv = m(i); var mi = i
    while (i < m.length) { if (mv < m(i)) { mv = m(i); mi = i }; i += 1 }
    mi
  }
  override def toString: String = {
    val sb = new StringBuffer
    for (i <- 0 until length) sb.append("%d=%-6f ".format(i, m(i)))
    // TODO: Make DiscreteDomain have index and lookup, with Int values, so that the above line will work nicely for CategoricalDomains also
    sb.toString
  }
}

class BPLattice(val variables:Collection[BeliefPropagation.BPVariable]) extends Lattice {
  type V = BeliefPropagation.BPVariable

  // Find all the factors touching the 'variables'
  val factors = new LinkedHashSet[Factor]

  def this(model:Model, variables:Collection[BeliefPropagation.BPVariable]) {
    this(variables)
    for (factor <- model.factorsOf[Template](variables)) addFactor(factor)
  }

  def bpFactorsOf(v:V) = v2m(v)

  // Data structure for holding mapping from Variable to the collection of BPFactors that touch it
  private val v2m = new HashMap[Variable, ArrayBuffer[BPFactor]] { override def default(v:Variable) = {this(v) = new ArrayBuffer[BPFactor]; this(v)} }
  // Create a BPFactor for each factor
  val marginals = new HashMap[Factor, BPFactor]

  def addFactor(factor:Factor) = {
    val bpFactor = new BPFactor(factor) {def factorsOf(v:Variable) = v2m(v)}
    marginals(factor) = bpFactor
    for (v <- factor.variables) v2m(v) += bpFactor
    factors += factor
  }

  /** Perform one iteration of belief propagation. */
  def update: Unit = marginals.values.foreach(_.update)
  /** Perform N iterations of belief propagation */
  def update(iterations:Int): Unit = for (i <- 1 to iterations) update

  /** Send each message in the lattice once, in order determined by a random tree traversal. */
  def updateTreewise: Unit = {
    v2m.values.foreach(_.foreach(_.resetTree))
    v2m.values.toList.shuffle.foreach(_.foreach(_.updateTreewise)) // randomly permute order each time
    // check that our "randomized tree traversal" touched everything (to run change _msgTo and _msgFrom to public)
    //assert(v2m.values.toList.forall(_.forall(x =>
    //     x._msgTo.forall(_.visitedDuringThisTree) &&  x._msgFrom.forall(_.visitedDuringThisTree)
    //)))
    for (m <- TreewiseBookkeeping.stack.reverse) m.updateTreewise
    TreewiseBookkeeping.stack.clear
  }
  /** Provide outside access to a BPFactor given is associated Factor */
  def marginal(f:Factor): Array[Double] = marginals(f).marginal
  def marginal(v:V): DiscreteMarginal1[V] = new DiscreteMarginal1(v, bpFactorsOf(v).map(_.messageTo(v).message))
  /* def sample(v:UncoordinatedCategoricalVariable): DiffList  // TODO: implement this */
  /* def sample: Unit   // TODO: implement this */
  def setVariablesToMarginalMax: Unit = variables.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  def setVariablesToMarginalMax(vs:Iterable[V]): Unit = vs.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  def setVariablesToMax: Unit = variables.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  def setVariablesToMax(vs:Iterable[V]): Unit = vs.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
}
