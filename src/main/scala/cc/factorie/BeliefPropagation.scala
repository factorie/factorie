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

import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import scala.collection.SeqLike
import java.util.Arrays
import cc.factorie._
import cc.factorie.la._

// Very preliminary explorations in inference by belief propagation among discrete variables.
// Not yet finished, and certainly not yet optimized for speed at all.
// This is here mostly to get some idea of what the interfaces might look like.

/** Holds some global definitions for BP.  */
// Many of these things should not be global!  Remove them. -akm
object BeliefPropagation {
  type BPVariable = DiscreteVariable with NoVariableCoordination // Our BP implementation currently only handles these types of Variables
  val normalizeMessages = false
  var useSumMessages = true // If true messages from factors to variables are sum-product else max-product
}

/** A factor in a belief propagation lattice used for inference.
    Note that an instance of this class is not actually a Template#Factor itself;
    but it points to a Template#Factor with its 'factor' member.
    @author Andrew McCallum, Kedar Bellare, Greg Druck, Tim Vieira
*/
abstract class BPFactor(val factor: Factor) {
  type V = BeliefPropagation.BPVariable

  // TODO should filter by lattice's list of variables to infer
  val variables = factor.variables.filter(_.isInstanceOf[V]).toList.asInstanceOf[List[V]] // because factor.variables is not very efficient // TODO Make this toSeq instead?  scala.collection.immutable.Vector
  // TODO Consider alternative to toList?  Note that we do use list in variableSettings

  /**Given a variable, return the BPFactors touching it.  This method must be provided by subclasses. */
  def factorsOf(v: Variable): Seq[BPFactor]

  // TODO this method should be in a utility class or package
  /**Iterate through all combinations of values in Variables given their `SettingIterators */
  def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
    if (vs == Nil) false
    else if (vs.head.hasNext) { vs.head.next; true }
    else if (vs.tail != Nil) { vs.head.reset; vs.head.next; nextValues(vs.tail) }
    else false
  }

  /** Get an Iterator" over all the settings of the neighbors of this factor. The order of the integers matches the order from the 'variables' method. */
  def variableSettings: Iterator[Boolean] = new Iterator[Boolean] {
    val settings = BPFactor.this.variables.map(_.settings).toList
    var hasNext = true
    def reset = settings.foreach(setting => {setting.reset; setting.next})
    def next = { hasNext = nextValues(settings); hasNext }
  }

  abstract class Message(val v: V) {
    lazy protected val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message: Array[Double] = msg
    def messageCurrentValue: Double = msg(v.intValue)
    var updateCount = 0
  }

  // Message from this factor to Variable v.  Return this so we can say messageTo(v).update.message
  abstract class MessageTo(override val v: V) extends Message(v) {
    // IterableSettings instances for each of the variables neighboring this BPFactor, except the variable 'v'
    // TODO Don't we need to remove the variables that are not among those we are inferring?
    protected val neighborSettings = variables.filter(_.!=(v)).map(_.settings).toList
    protected val neighborFactors = factorsOf(v).filter(_.!=(BPFactor.this))

    def updateTreewiseFromLeaves: Unit = {
      if (updateCount > 0) throw new Error("Either tree not reset or lattice has cycle")
      updateCount += 1
      for (n <- neighborSettings) {
        BPFactor.this.messageFrom(n.variable).updateTreewiseFromLeaves
      }
      update
    }

    def updateTreewiseToLeaves: Unit = {
      if (updateCount > 0) throw new Error("Either tree not reset or lattice has cycle")
      updateCount += 1
      update

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
      forIndex(msg.length)(i => { // Consider reversing the nested ordering of this loop and the inner one
        v.set(i)(null) // Note: this is changing the value of this Variable
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          //println("SumProductMessageTo factor.cachedStatistics")
          msg(i) = factor.cachedStatistics.score
        } else if (neighborSettings.size == 1) {
          val neighbor = neighborSettings.head.variable
          msg(i) = Double.NegativeInfinity // i.e. log(0)
          forIndex(neighbor.domain.size)(j => {
            neighbor.set(j)(null)
            msg(i) = maths.sumLogProb(msg(i), factor.cachedStatistics.score + BPFactor.this.messageFrom(neighbor).messageCurrentValue)
          })
        } else { // This factor has variable neighbors in addition to v itself
          // Sum over all combinations of values in neighboring variables with v's value fixed to i.
          // TODO Consider special purpose code for when there is one other neighbor
          neighborSettings.foreach(setting => {setting.reset; setting.next}) // reset iterator and advance to first setting.
          msg(i) = Double.NegativeInfinity // i.e. log(0)
          do {
            msg(i) = maths.sumLogProb(msg(i), factor.cachedStatistics.score + neighborSettings.sumDoubles(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue))
          } while (nextValues(neighborSettings))
        }
      })
      if (BeliefPropagation.normalizeMessages) maths.normalizeLogProb(msg)
    }
  }

  case class MaxProductMessageTo(override val v: V) extends MessageTo(v) {
    /*lazy protected val maxIndex = new Array[Int](v.domain.size) */ // Holds how many nextValues calls it takes to get to max value
    def update = {
      forIndex(v.domain.size)(i => { // Consider reversing the nested ordering of this loop and the inner one
        v.set(i)(null) // Note: that this is changing the Variable value
        if (neighborSettings.size == 0) { // This factor has only one variable neighbor, v itself
          msg(i) = factor.cachedStatistics.score
          //maxIndex(i) = -1
        // } else if (neighborSettings.size == 1) { // TODO Needs to be implemented, to gain the same efficiencies we got above.
        } else { // This factor has variable neighbors in addition to v itself
          neighborSettings.foreach(setting => {setting.reset; setting.next})
          msg(i) = Double.NegativeInfinity
          //maxIndex(i) = -1
          //var settingCount = 0
          do {
            val score = factor.cachedStatistics.score + neighborSettings.sumDoubles(n => BPFactor.this.messageFrom(n.variable).messageCurrentValue)
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
    protected val neighborSettings = variables.filter(_.!=(v)).map(_.settings).toList

    def updateTreewiseFromLeaves: Unit = {
      if (updateCount > 0) throw new Error("Either tree not reset or lattice has cycle")
      updateCount += 1
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        msg2.updateTreewiseFromLeaves
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      if (BeliefPropagation.normalizeMessages) maths.normalizeLogProb(msg)
    }

    def updateTreewiseToLeaves: Unit = {
      if (updateCount > 0) throw new Error("Either tree not reset or lattice has cycle")
      updateCount += 1
      Arrays.fill(msg, 0.0)
      for (n <- neighborFactors) {
        val msg2 = n.messageTo(v)
        for (i <- 0 until v.domain.size) msg(i) += msg2.message(i)
      }
      if (BeliefPropagation.normalizeMessages) maths.normalizeLogProb(msg)

      for (n <- neighborSettings) {
        BPFactor.this.messageTo(n.variable).updateTreewiseToLeaves
      }
    }

    def update = {
//      if (neighborFactors.size > 0)
//        for (i <- 0 until v.domain.size) {
//          msg(i) = neighborFactors.sumDoubles(_.messageTo(v).message(i))
//        }
      if (neighborFactors.size == 1) {
        val nbrmsg = neighborFactors.head.messageTo(v).message
        Array.copy(nbrmsg, 0, msg, 0, msg.length)
      } else if (neighborFactors.size > 1) {
        Arrays.fill(msg, 0.0)
        for (nf <- neighborFactors) {
          val nbrmsg = nf.messageTo(v).message
          var i = 0
          while (i < msg.length) {
            msg(i) += nbrmsg(i); i += 1
          }
        }
      }
      if (BeliefPropagation.normalizeMessages) maths.normalizeLogProb(msg)
    }
  }

  /* For Sum-Product and Max-Product: */
  lazy private val _msgTo: Seq[MessageTo] = {
    if (BeliefPropagation.useSumMessages) this.variables.map(SumProductMessageTo(_)).toSeq
    else this.variables.map(MaxProductMessageTo(_)).toSeq
  }
  lazy private val _msgFrom: Seq[MessageFrom] = this.variables.map(MessageFrom(_)).toSeq

  def messageTo(v: V): MessageTo = messageTo(this.variables.toSeq.indexOf(v))

  def messageTo(vi: Int): MessageTo = _msgTo(vi)

  def messageFrom(v: V): MessageFrom = messageFrom(this.variables.toSeq.indexOf(v))

  def messageFrom(vi: Int): MessageFrom = _msgFrom(vi)

  def update: Unit = {_msgFrom.foreach(_.update); _msgTo.foreach(_.update); } // TODO swap order?

  def updateTreewise: Unit = {
    _msgFrom.foreach(message => if (message.updateCount == 0) {message.updateTreewiseFromLeaves})
    _msgTo.foreach(message => if (message.updateCount == 0) {message.updateTreewiseToLeaves})
  }

  def resetTree: Unit = {
    _msgTo.foreach(_.updateCount = 0)
    _msgFrom.foreach(_.updateCount = 0)
  }

  /**Not the overall marginal over the variable, just this factor's marginal over the variable */
  def marginal(v: V): Array[Double] = {
    val result = new Array[Double](v.domain.size)
    Array.copy(messageTo(v).message, 0, result, 0, result.length)
    maths.expNormalize(result)
    result
  }

  /**The marginal probability distribution over all settings of the neighbor variables of this factor.
  If you want access to the entries by indicies of individual neighbors' values, @see marginalMap.  */
  def marginal: Array[Double] = {
    val dim = this.variables.multiplyInts(_.domain.size)
    val variableSettings = this.variables.map(_.settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    val result = new Array[Double](dim)
    var i = 0
    do {
      result(i) = factorCurrentScore
      i += 1
    } while (nextValues(variableSettings))
    maths.expNormalize(result)
    result
  }

  def marginalMap: HashMap[List[Int], Double] = {
    val dim = this.variables.multiplyInts(_.domain.size)
    val result = new Array[Double](dim)
    val variableSettings = this.variables.map(_.settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    val tmpMap = new HashMap[List[Int], Int]
    var i = 0
    do {
      val varSetting = variableSettings.map(s => s.variable.asInstanceOf[V].intValue).toList
      // add temporary mapping
      tmpMap += varSetting -> i
      // compute score
      result(i) = factorCurrentScore
      i += 1
    } while (nextValues(variableSettings))
    maths.expNormalize(result)
    // create actual map
    val map = new HashMap[List[Int], Double]
    for (varSetting <- tmpMap.keys) map += varSetting -> result(tmpMap(varSetting))
    map
  }

  def factorCurrentScore: Double = factor.cachedStatistics.score + variables.sumDoubles(v => BPFactor.this.messageFrom(v).messageCurrentValue)
  def factorCurrentScore(statistics:Template#Statistics): Double = {
    assert(statistics.template eq factor.template)
    statistics.score + variables.sumDoubles(v => BPFactor.this.messageFrom(v).messageCurrentValue)
  }

  def logZ: Double = {
    val variableSettings = this.variables.map(_.settings).toList
    variableSettings.foreach(setting => {setting.reset; setting.next})
    var result = Double.NegativeInfinity
    do {
      val score = factorCurrentScore
      result = maths.sumLogProb(result, score)
    } while (nextValues(variableSettings))
    result
  }
}

/** A Lattice representing the result of belief propagation inference.  Results can be further updated by calls to various "update" methods.
    @author Andrew McCallum, Kedar Bellare, Greg Druck */
class BPLattice[V<:BeliefPropagation.BPVariable](val variables: Iterable[V], model: Model) extends Lattice[V] {
  //type V = BeliefPropagation.BPVariable
  type VariableMarginalType = DiscreteMarginal[V]
  type FactorMarginalType = DiscreteFactorMarginal

  // TODO Consider moving this further out, for even more efficiency?  But then the cache could get very big.
  model.foreach(_.clearCachedStatistics)

  // Data structure for holding mapping from Variable to the collection of BPFactors that touch it
  private val v2m = new HashMap[Variable, ArrayBuffer[BPFactor]] {override def default(v: Variable) = {this(v) = new ArrayBuffer[BPFactor]; this(v)}}
  // We have a BPFactor for each factor
  val bpFactors = new HashMap[Factor, BPFactor]
  // Holds all factors touching any of the 'variables'
  val factors = new HashSet[Factor]
  // Initialize bpFactors
  for (factor <- model.factorsOf[Template](variables)) {
    val bpFactor = new BPFactor(factor) {def factorsOf(v: Variable) = v2m(v)}
    bpFactors(factor) = bpFactor
    for (v <- factor.variables) v2m(v) += bpFactor
    factors += factor
  }

  /**The BPFactors touching variable v. */
  def bpFactorsOf(v: V): Iterable[BPFactor] = v2m(v)
  /**Perform one iteration of belief propagation. */
  def update: Unit = bpFactors.values.foreach(_.update)
  /**Perform N iterations of belief propagation */
  def update(iterations: Int): Unit = for (i <- 1 to iterations) update
  /**Send each message in the lattice once, in order determined by a random tree traversal. */
  def updateTreewise(expectations:Map[Template,Vector] = null, shuffle: Boolean = false): Unit = {
    if (expectations ne null) throw new Error("Not yet implemented")
    bpFactors.values.foreach(_.resetTree)
    val factors = if (shuffle) bpFactors.values.toSeq.shuffle else bpFactors.values.toSeq // optionally randomly permute order, ala TRP
    // Call updateTreewise on all factors, but note that, if the graph is fully connected,
    // "updateTreewise" on the first marginal will do the entire graph, and the other calls will return immediately
    BeliefPropagation.useSumMessages = true // TODO This won't work multithreaded!  Fix this. -akm
    factors.foreach(_.updateTreewise)
  }
  /** Performs max-product inference. */
  def updateTreewiseMax(shuffle: Boolean = false): Unit = {
    bpFactors.values.foreach(_.resetTree)
    val factors = if (shuffle) bpFactors.values.toList.shuffle else bpFactors.values.toList // optionally randomly permute order, ala TRP
    BeliefPropagation.useSumMessages = false
    factors.foreach(_.updateTreewise)
  }
  /** Provide outside access to a BPFactor marginal given is associated Factor */
  override def marginal(f: Factor): Option[DiscreteFactorMarginal] = {
    val bpFactor = bpFactors(f)
    if (bpFactor ne null)
      Some(new DiscreteFactorMarginal(f, bpFactors(f).marginal))
    else
      None
  }
  def marginalMap(f: Factor): HashMap[List[Int], Double] = bpFactors(f).marginalMap
  override def marginal(v: V): Option[DiscreteMarginal[V]] = { // TODO Consider caching these?
    val factors = bpFactorsOf(v)
    if ((factors ne null) && factors.size > 0) {
      val m = new Array[Double](v.domain.size)
      var sum = 0.0
      for (message <- factors.map(_.messageTo(v).message)) {
        assert(message.length == m.length)
        for (i <- 0 until m.length) { m(i) += message(i); sum += message(i) }
      }
      maths.expNormalize(m)
      Some(new DiscreteMarginal(v, m))
    } else
      None
  }
  /* def sample(v:UncoordinatedCategoricalVariable): DiffList  // TODO: implement this */
  /* def sample: Unit   // TODO: implement this */
  //def setVariablesToMarginalMax(vs:Iterable[V] = variables)(implicit d:DiffList = null): Unit = vs.foreach(v => v.set(marginal(v).get.maxIndex)(d))
  def setVariablesToMax(vs:Iterable[V] = variables)(implicit d:DiffList = null): Unit = vs.foreach(v => v.set(marginal(v).get.maxPrIndex)(d))
  // TODO Why is this called "sumLogZ" instead of just "logZ"? -akm
  def sumLogZ: Double = {
    // TODO Rather than re-traversing the tree, can't we just store during treewise BP a representative factor for each disconnected subgraph?
    val factorsTouched = new HashSet[BPFactor]
    val factors = bpFactors.values.toArray
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


/** Perform inference according to belief propagation.
    @author Andrew McCallum, Kedar Bellare, Tim Vieira
    @since 0.8
 */
class BPInferencer[V<:BeliefPropagation.BPVariable](model:Model) extends VariableInferencer[V] {
  override type LatticeType = BPLattice[V]
  def infer(variables:Iterable[V], varying:Iterable[V]): LatticeType = inferTreewise(variables, varying) // TODO Make a more sensible default
  def infer(variables:Iterable[V], varying:Iterable[V], numIterations:Int): LatticeType = {
    val result = new BPLattice(varying, model)
    result.update(numIterations) // TODO Of course make this smarter later
    result.setVariablesToMax(variables) // For now, just inference my marginal maximization
    // NOTE the above line requires that 'variables' is a subset of varying, of course!
    result
  }
  def infer(variables:Iterable[V], numIterations:Int): LatticeType = infer(variables, variables, numIterations)
  // waiting for Scala 2.8 default parameters...
  def inferTreewise(variables:Iterable[V], varying:Iterable[V]): LatticeType = inferTreewise(variables, varying, 1)
  def inferTreewise(variables:Iterable[V], varying:Iterable[V], maxiterations:Int): LatticeType = {
    // NOTE: 'variables' must be a subset of varying, of course!
    val result = new BPLattice(varying, model)
    result.updateTreewise()
    result.setVariablesToMax(variables)
    result
  }
  def inferTreewise(variables:Iterable[V]): LatticeType = inferTreewise(variables, variables, 1)
  def inferTreewise(variables:Iterable[V], maxiterations:Int): LatticeType = inferTreewise(variables, variables, maxiterations)
  def inferTreewiseMax(variables: Iterable[V], varying: Iterable[V], maxiterations: Int): LatticeType = {
    val result = new BPLattice(varying, model)
    result.updateTreewiseMax()
    result.setVariablesToMax(variables)
    result
  }
  def inferTreewiseMax(variables: Iterable[V]): LatticeType = inferTreewiseMax(variables, variables, 1)
  def inferTreewiseMax(variables: Iterable[V], maxIterations: Int): LatticeType = inferTreewiseMax(variables, variables, maxIterations)
}

