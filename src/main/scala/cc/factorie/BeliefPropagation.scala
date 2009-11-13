package cc.factorie
import scala.collection.mutable.{HashSet,HashMap,ArrayBuffer}
import cc.factorie.util.Implicits._

// Very preliminary explorations in inference by belief propagation among discrete variables.
// Not yet finished, and certainly not yet optimized for speed at all.
// This is here mostly to get some idea of what the interfaces might look like.


abstract class BPFactor(val factor:VectorTemplate#Factor) {
  type V = UncoordinatedCategoricalVariable
  /** Given a variable, return the BPFactors touching it. */
  def marginals(v:Variable): Seq[BPFactor];
  /** Given a List of SettingIterators for some Variables, find the next value */
  protected def nextValues(vs: List[IterableSettings#SettingIterator]): Boolean = {
  	if (vs.first.hasNext) {vs.first.next; true}
  	else if (vs.tail != Nil) {if (!vs.first.variable.isConstant) vs.first.reset; vs.first.next; nextValues(vs.tail)}
  	else false
  }
  case class MessageTo(v:V) {
    lazy private val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message = msg
    // IterableSettings instances for each of the variables neighboring this BPFactor, except the variable 'v'
    protected val msgNeighbors = factor.variables.filter(v2 => v2 != v).map(v2 => v2.asInstanceOf[V].settings).toList
    /** Do one step of belief propagation for the message from this BPFactor to variable 'v' */
    def update: MessageTo = {
      if (!v.isConstant) for (i <- 0 until msg.length) msg(i) = 0.0 // TODO surely there is a faster way
      else {
        val origIndex = v.index
        for (i <- 0 until v.domain.size) {
          v.setByIndex(i)(null) // note that this is changing the Variable value
          msgNeighbors.foreach(setting => {setting.reset; setting.next})
          msg(i) = Math.NEG_INF_DOUBLE
          do {
            msg(i) = Maths.sumLogProb(msg(i), factor.statistic.score + msgNeighbors.sum(n => BPFactor.this.messageFrom(n.variable).message(v.index)))
          } while (nextValues(msgNeighbors))
        }
        v.setByIndex(origIndex)(null) // put it back where it was, just in case someone cares
      }
      this // Return this so we can say messageTo(v).update.message
    }
  }
  case class MessageFrom(v:V) {
    lazy private val msg = new Array[Double](v.domain.size) // Holds unnormalized log-probabilities
    def message = msg
    def update: MessageFrom = {
      for (i <- 0 until v.domain.size)
        msg(i) = marginals(v).filter(_.!=(this)).sum(_.messageTo(v).message(i))
      this // Return this so we can say messageFrom(v).update.message
    }
  }
  lazy private val _msgTo: Array[MessageTo] = factor.variables.map(v => MessageTo(v.asInstanceOf[UncoordinatedCategoricalVariable])).toSeq.toArray
  lazy private val _msgFrom: Array[MessageFrom] = factor.variables.map(v => MessageFrom(v.asInstanceOf[UncoordinatedCategoricalVariable])).toSeq.toArray
  def messageTo(v: UncoordinatedCategoricalVariable) = _msgTo(factor.variables.toSeq.indexOf(v))
  def messageFrom(v: UncoordinatedCategoricalVariable) = _msgFrom(factor.variables.toSeq.indexOf(v))
  def messageTo(vi: Int) = _msgTo(vi)
  def messageFrom(vi: Int) = _msgFrom(vi)
  def update: Unit = { _msgFrom.foreach(_.update); _msgTo.foreach(_.update);  }
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

class BPLattice(model:Model, variables:Collection[UncoordinatedCategoricalVariable]) {
  type V = UncoordinatedCategoricalVariable
  // Find all the factors touching the 'variables'
  val factors = model.factorsOf[VectorTemplate](variables)
  // Data structure for holding mapping from Variable to the collection of BPFactors that touch it
  val v2m = new HashMap[Variable,ArrayBuffer[BPFactor]] { override def default(v:Variable) = {this(v) = new ArrayBuffer[BPFactor]; this(v)} }
  // Create a BPFactor for each factor
  val marginals = new HashMap[Factor,BPFactor]
  factors.foreach(f => marginals(f) = new BPFactor(f) {def marginals(v:Variable) = v2m(v)})
  // Populate the 'v2m' mapping from variable to marginal  
  marginals.values.foreach(m => m.factor.variables.foreach(v => v2m(v) += m))
  
  /** Perform one iteration of belief propagation. */
  def update: Unit = marginals.values.foreach(_.update)
  /** Provide outside access to a BPFactor given is associated Factor */
  def marginal(f:Factor): Array[Double] = marginals(f).marginal 
  //def marginal(v:UncoordinatedCategoricalVariable): Array[Double] // TODO implement this
  //def sample(v:UncoordinatedCategoricalVariable): DiffList // TODO implement this
  //def sample: Unit // TODO implement this
  
}

