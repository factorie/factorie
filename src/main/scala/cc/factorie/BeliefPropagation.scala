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

// Very preliminary explorations in inference by belief propagation among discrete variables.
// Not yet finished, and certainly not yet optimized for speed at all.
// This is here mostly to get some idea of what the interfaces might look like.

/** Holds some global definitions for BP.  Our BP implementation currently only handles  */
object BeliefPropagation {
  type BPVariable = UncoordinatedDiscreteVariable
}

/** A factor in a belief propagation lattice used for inference.  
    Note that an instance of this class is not actually a Template#Factor itself; 
    but it points to a Template#Factor with its 'factor' member.
  
    @author Andrew McCallum
    @author Tim Viera
 */
abstract class BPFactor(val factor:VectorTemplate#Factor) {
  type V = BeliefPropagation.BPVariable
  /** Given a variable, return the BPFactors touching it.  This method must be provided by subclassers. */
  def factorsOf(v:Variable): Seq[BPFactor];
  /** Given a List of SettingIterators for some Variables, find the next value */
  private def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.first.hasNext) {vs.first.next; true}
    else if (vs.tail != Nil) {vs.first.reset; vs.first.next; nextValues(vs.tail)}
    else false
  }
  /** Message from this factor to Variable v. */
  case class MessageTo(v:V) {
    lazy protected val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message: Array[Double] = msg 
    def messageCurrentValue: Double = msg(v.intValue)
    // IterableSettings instances for each of the variables neighboring this BPFactor, except the variable 'v'
    protected val neighborSettings = factor.variables.filter(v2 => v2 != v && v2.isInstanceOf[V]).map(v2 => v2.asInstanceOf[V].settings).toList
    var visitedDuringThisTree = false
    /** Do one step of belief propagation for the message from this BPFactor to variable 'v' */
    def update: MessageTo = {
      val origIndex = v.index
      for (i <- 0 until v.domain.size) { // Consider reversing the nested ordering of this loop and the inner one
        v.setByIndex(i)(null) // note that this is changing the Variable value
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          msg(i) = factor.statistic.score
        } else { // This factor has variable neighbors in addition to v itself 
          neighborSettings.foreach(setting => {setting.reset; setting.next})
          msg(i) = Math.NEG_INF_DOUBLE
          do {
            msg(i) = Maths.sumLogProb(msg(i), factor.statistic.score + neighborSettings.sum(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue))
          } while (nextValues(neighborSettings))
        }
      }
      v.setByIndex(origIndex)(null) // put it back where it was, just in case someone cares
      this // Return this so we can say messageTo(v).update.message
    }
    /** Update this message, but first update the messages its depends on, avoiding update loops. */
    def updateTreewise: MessageTo = {
      if (visitedDuringThisTree) return this
      visitedDuringThisTree = true
      //println("updateTreewise MessageTo   "+factor+" >>>> "+v)
      for (n <- neighborSettings) BPFactor.this.messageFrom(n.variable).updateTreewise
      update
    }
  }
  case class MaxProductMessageTo(override val v:V) extends MessageTo(v) {
    lazy protected val maxIndex = new Array[Int](v.domain.size) // Holds how many nextValues calls it takes to get to max value
    override def update: MaxProductMessageTo = {
      val origIndex = v.index
      for (i <- 0 until v.domain.size) { // Consider reversing the nested ordering of this loop and the inner one
        v.setByIndex(i)(null) // note that this is changing the Variable value
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          msg(i) = factor.statistic.score
          maxIndex(i) = -1
        } else { // This factor has variable neighbors in addition to v itself 
          neighborSettings.foreach(setting => {setting.reset; setting.next})
          msg(i) = Math.NEG_INF_DOUBLE
          maxIndex(i) = -1
          var settingCount = 0
          do {
            val score = factor.statistic.score + neighborSettings.sum(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue)
            if (score > msg(i)) { msg(i) = score; maxIndex(i) = settingCount }
            settingCount += 1
          } while (nextValues(neighborSettings))
        }
      }
      v.setByIndex(origIndex)(null) // put it back where it was, just in case someone cares
      this // Return this so we can say messageTo(v).update.message
    }
  }
  /** Message from Variable v to this factor. */
  case class MessageFrom(v:V) {
    lazy private val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message: Array[Double] = msg // TODO Consider making these scalala Vectors
    def messageCurrentValue: Double = msg(v.intValue)
    val neighborFactors = factorsOf(v).filter(_.!=(BPFactor.this))
    var visitedDuringThisTree = false
    def update: MessageFrom = {
      if (neighborFactors.size > 0)
        for (i <- 0 until v.domain.size)
          msg(i) = neighborFactors.sum(_.messageTo(v).message(i))
      this // Return this so we can say messageFrom(v).update.message
    }
    def updateTreewise: MessageFrom = {
      if (visitedDuringThisTree) return this
      visitedDuringThisTree = true
      //println("updateTreewise MessageFrom "+factor+" <<<< "+v)
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        msg2.updateTreewise
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      this
    }
  }
  lazy private val _msgTo: Array[MessageTo] = factor.variables.filter(_.isInstanceOf[V]).map(v => MessageTo(v.asInstanceOf[V])).toSeq.toArray
  lazy private val _msgFrom: Array[MessageFrom] = factor.variables.filter(_.isInstanceOf[V]).map(v => MessageFrom(v.asInstanceOf[V])).toSeq.toArray
  def messageTo(v:V) = _msgTo(factor.variables.toSeq.indexOf(v))
  def messageFrom(v:V) = _msgFrom(factor.variables.toSeq.indexOf(v))
  def messageTo(vi: Int) = _msgTo(vi)
  def messageFrom(vi: Int) = _msgFrom(vi)
  def update: Unit = { _msgTo.foreach(_.update); _msgFrom.foreach(_.update);  }
  def updateTreewise: Unit = { _msgTo.foreach(_.updateTreewise); _msgFrom.foreach(_.updateTreewise);  }
  def resetTree: Unit = { _msgTo.foreach(_.visitedDuringThisTree = false); _msgFrom.foreach(_.visitedDuringThisTree = false); }
  /** Not the overall marginal over the variable; just this factor's marginal over the variable */
  def marginal(v:V): Array[Double] = {
    val result = new Array[Double](v.domain.size)
    messageTo(v).message.copyToArray(result, 0) // In Scala 2.8 there will be a more compact way to do this.
    Maths.normalize(result); result
  }
  def marginal: Array[Double] = {
    val dim = factor.variables.productInts(_.asInstanceOf[V].domain.size)
    val m = new Array[Double](dim)
    throw new Error("Not yet implemented")
    //_msgFrom.foreach(m => )
    m
  }
  //def neighborDistribution : Array[Double];
  //def sufficientDistribution : Array[Double]
}

trait DiscreteMarginalN {
  /** The settings of each of the N variables that together yield the highest probability. */
  def maxEntry: Array[Int]
}
// TODO This should really inherit from Multinomial, but first I have to make it not require an "Outcome" variable.
// I should also make a multi-dimensional multinomial.
class DiscreteMarginal1[V<:DiscreteValue](val variable:V) extends RandomAccessSeq[Double] with DiscreteMarginalN {
  def this(v:V, messages:Iterable[Seq[Double]]) = { 
    this(v);
    for (message <- messages) {
      assert(message.length == m.length)
      for (i <- 0 until m.length) { m(i) += message(i); sum += message(i) }
    }
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
    for (i <- 0 to length) sb.append("%d=%-6f ".format(i, m(i))) 
    // TODO Make DiscreteDomain have index and lookup, with Int values, so that the above line will work nicely for CategoricalDomains also
    sb.toString
  }
}

class BPLattice(model:Model, val variables:Collection[BeliefPropagation.BPVariable]) extends Lattice {
  type V = BeliefPropagation.BPVariable
  // Find all the factors touching the 'variables'
  val factors = model.factorsOf[VectorTemplate](variables)
  def bpFactorsOf(v:V) = v2m(v)
  // Data structure for holding mapping from Variable to the collection of BPFactors that touch it
  private val v2m = new HashMap[Variable,ArrayBuffer[BPFactor]] { override def default(v:Variable) = {this(v) = new ArrayBuffer[BPFactor]; this(v)} }
  // Create a BPFactor for each factor
  val marginals = new HashMap[Factor,BPFactor]
  factors.foreach(f => marginals(f) = new BPFactor(f) {def factorsOf(v:Variable) = v2m(v)})
  // Populate the 'v2m' mapping from variable to marginal  
  marginals.values.foreach(m => m.factor.variables.foreach(v => v2m(v) += m))
  
  /** Perform one iteration of belief propagation. */
  def update: Unit = marginals.values.foreach(_.update)
  /** Perform N iterations of belief propagation */
  def update(iterations:Int): Unit = for (i <- 1 to iterations) update
  /** Send each message in the lattice once, in order determined by a random tree traversal. */
  def updateTreewise: Unit = { 
    v2m.values.foreach(_.foreach(_.resetTree))
    v2m.values.toList.shuffle.foreach(_.foreach(_.updateTreewise)) // randomly permute order each time
  }
  /** Provide outside access to a BPFactor given is associated Factor */
  def marginal(f:Factor): Array[Double] = marginals(f).marginal 
  def marginal(v:V): DiscreteMarginal1[V] = 
    new DiscreteMarginal1(v, bpFactorsOf(v).map(_.messageTo(v).message))
  //def sample(v:UncoordinatedCategoricalVariable): DiffList // TODO implement this
  //def sample: Unit // TODO implement this
  def setVariablesToMarginalMax: Unit = variables.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  def setVariablesToMarginalMax(vs:Iterable[V]): Unit = vs.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  // TODO Change these to implment max-product-wise global max.  Currently just a max-marginal placeholder. 
  def setVariablesToMax: Unit = variables.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
  def setVariablesToMax(vs:Iterable[V]): Unit = vs.foreach(v => v.setByIndex(marginal(v).maxIndex)(null))
}

