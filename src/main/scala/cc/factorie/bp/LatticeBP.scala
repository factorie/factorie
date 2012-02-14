package cc.factorie.bp

import StrictMath._
import collection.mutable.{HashSet, ArrayBuffer, HashMap, Queue, Buffer}
import cc.factorie._
import cc.factorie.la.{DenseVector, SparseVector, Vector}

/**
 * @author sameer, apassos, bmartin
 */

abstract class MessageFactor(val factor: Factor, val varying: Set[Variable], fg: LatticeBP) {

  val variables: Seq[Variable] = factor.variables
  val _incoming = new FactorMessages(variables)
  val _outgoing = new FactorMessages(variables)
  // same as above, but in a ordered and DiscreteVariable form
  val discreteVarying: Seq[(DiscreteVariable, Int)] =
    variables.zipWithIndex.filter(varying contains _._1).map(p => (p._1.asInstanceOf[DiscreteVariable], p._2))
  // complement of varyingNeighbors
  val fixedNeighbors: Seq[(Variable, Int)] = variables.zipWithIndex.filter(v => !varying.contains(v._1))
  // set of neighbors that are varying
  lazy val varyingNeighbors: Set[Variable] = {
    val set: HashSet[Variable] = new HashSet
    set ++= discreteVarying.map(_._1)
    set.toSet
  }
  val nodes: Seq[MessageNode] = variables.map(fg.node(_))
  val fids: Seq[Int] = {
    // adds itself to the neighbor's node
    nodes.zipWithIndex.map(p => p._1.addFactor(this, p._2))
  }

  protected val _valuesSize: Int = discreteVarying.foldLeft(1)(_ * _._1.domain.size)
  protected val _marginal: Array[Double] = Array.fill(_valuesSize)(Double.NaN)

  // caching of the scores
  private var _cache: Array[Double] = Array.fill(_valuesSize)(Double.NaN)

  // Maintain deltas of consecutive outgoing messages
  val outgoingDeltas = new FactorMessages(variables)

  def clearCache = _cache = Array.fill(_valuesSize)(Double.NaN)

  def resetMessages = {
    _incoming.reset
    _outgoing.reset

  }
  def incoming(vid: Int): GenericMessage = _incoming.get(vid)

  protected def setIncoming(vid: Int, message: GenericMessage): Unit = {
    _incoming.set(vid, message)
  }

  def outgoing(vid: Int): GenericMessage = _outgoing.get(vid)

  protected def setOutgoing(vid: Int, message: GenericMessage): Unit = {
    _outgoing.set(vid, message)
  }

  // return the stored marginal probability for the given value
  def marginal(values: Values): Double = _marginal(values.index(varyingNeighbors))

  protected def getScore(assignment: Values, index: Int = -1): Double = {
    assignment.statistics.score
  }

  def updateAllOutgoing() {
    val result = marginalize(_incoming)
    for (pos <- (0 until nodes.length)) {
      //todo: fix case where both incoming and potential are deterministic, leading to a uniform outgoing
      updateSingleOutgoing(nodes(pos), pos, result)
    }
  }

  def updateSingleOutgoing(node: MessageNode, vid: Int, result: FactorMessages = marginalize(_incoming)) {
    // todo: remarginalize, but for individual variables separately
    // val mess = marginalize(node.variable, incoming)
    val mess = result.get(vid) / incoming(vid)
    setSingleOutgoing(node.variable, vid, mess)
    node.incoming(this, fids(vid), mess)
  }

  def setSingleOutgoing(variable: Variable, vid: Int, message: GenericMessage) {
    if (outgoing(vid).isDeterministic) {
      outgoingDeltas.set(vid, outgoing(vid))
    }
    else {
      outgoingDeltas.set(vid, message / outgoing(vid))
    }
    setOutgoing(vid, message)
  }

  def prepareSingleIncoming(node: MessageNode, vid: Int) = setIncoming(vid, node.outgoing(this, fids(vid)))

  def prepareAllIncoming() = (0 until nodes.length).foreach(vid => prepareSingleIncoming(nodes(vid), vid))

  def currentMaxDelta: Double = {
    if (outgoingDeltas.size == 0) Double.PositiveInfinity
    else outgoingDeltas.map(m => {
      val dynamicRange = m.dynamicRange
      assert(!dynamicRange.isNaN)
      log(dynamicRange)
    }).max
  }

  def logZ: Double = {
    var Z = 0.0
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      var num: Double = getScore(assignment)
      for (vid <- 0 until variables.length) {
        val mess = incoming(vid)
        num += mess.score(assignment.get(variables(vid)).get)
      }
      num = exp(num)
      Z += num
    }
    log(Z)
  }

  def foldMarginals[A](init: A)(f: (A, Values, Double) => A): A = {
    var a: A = init
    for (value: Values <- factor.valuesIterator(varyingNeighbors)) {
      val prob = marginal(value)
      a = f(a, value, prob)
    }
    a
  }

  def updateStatExpectations(exps: HashMap[DotFamily, Vector]): Unit = {
    factor match {
      case f: DotFamily#Factor => {
        if (!exps.contains(f.family)) exps(f.family) = new SparseVector(f.family.statisticsVectorLength)
        for (assignment: Values <- f.valuesIterator(varyingNeighbors)) {
          val prob = marginal(assignment)
          val vector = assignment.statistics.asInstanceOf[DotFamily#StatisticsType].vector
          exps(f.family) += (vector * prob)
        }
      }
      case _ =>
    }
  }

  override def toString = "M(%s)".format(factor)

  // def marginalize(target: DiscreteVariable, incoming: FactorMessages): GenericMessage

  def marginalize(incoming: FactorMessages): FactorMessages
}

trait SumFactor extends MessageFactor {

  /*
  def marginalize(target: DiscreteVariable, incoming: FactorMessages): GenericMessage = {
    // TODO add to _marginals
    // TODO incorporate fixed vars
    if (incoming(target).isDeterministic) return incoming(target)
    val scores: HashMap[Any, Double] = new HashMap[Any, Double] {
      override def default(key: Any) = 0.0
    }
    var maxLogScore = Double.NegativeInfinity
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment)
      for (variable <- variables) {
        if (variable != target) {
          val mess = incoming(variable)
          num += mess.score(assignment.get(variable).get)
        }
      }
      if (num > maxLogScore) maxLogScore = num
      _marginal(index) = num
    }
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      val num: Double = _marginal(index) - maxLogScore
      val expnum = exp(num)
      scores(assignment.get(target).get) = expnum + scores(assignment.get(target).get)
    }
    val varScores: Buffer[Double] = new ArrayBuffer(target.domain.size)
    for (value <- target.domain.values) {
      varScores += log(scores(value))
    }
    if (varScores.exists(_.isNegInfinity)) {
      //throw new Exception("output message has negInfinity")
    }
    BPUtil.message(target, varScores)
  } */

  def marginalize(incoming: FactorMessages): FactorMessages = {
    val result = new FactorMessages(variables)
    val scores: Array[Array[Double]] = new Array(varyingNeighbors.size)
    // initialize score arrays for varying neighbors
    for (i <- 0 until discreteVarying.length) {
      scores(i) = Array.fill(discreteVarying(i)._1.domain.size)(0.0)
    }
    var maxLogScore = Double.NegativeInfinity
    // go through all the assignments of the varying variables
    // and find the maximum score for numerical reasons
    val tmpScore: Array[Double] = Array.fill(_valuesSize)(Double.NaN)
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment, index)
      for (dv <- discreteVarying) {
        val vid = dv._2
        val mess = incoming.get(vid)
        num += mess.score(assignment(dv._1))
      }
      if (num > maxLogScore) maxLogScore = num
      tmpScore(index) = num
    }
    // go through all the assignments of the varying variables
    // and find Z, and calculate the scores
    var Z = 0.0
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      val num = tmpScore(index) - maxLogScore
      val expnum = exp(num)
      assert(!expnum.isInfinity)
      _marginal(index) = expnum
      for (i <- 0 until discreteVarying.length) {
        scores(i)(assignment(discreteVarying(i)._1).intValue) += expnum
      }
      Z += expnum
    }
    (0 until _marginal.size).foreach(i => _marginal(i) /= Z)
    // set the outgoing messages
    for (i <- 0 until discreteVarying.length) {
      val dv = discreteVarying(i)
      val vid = dv._2
      result.set(vid, BPUtil.message(dv._1, scores(i).map(s => log(s)).toSeq))
    }
    // deterministic messages for the fixed neighbors
    for (fv <- fixedNeighbors) {
      val vid = fv._2
      result.set(vid, incoming.get(vid))
    }
    result
  }

}

trait MaxFactor extends MessageFactor {

  /*
  def marginalize(target: DiscreteVariable, incoming: FactorMessages): GenericMessage = {
    // TODO add to _marginals
    // TODO incorporate fixed vars
    if (incoming(target).isDeterministic) return incoming(target)
    val scores: HashMap[Any, Double] = new HashMap[Any, Double] {
      override def default(key: Any) = 0.0
    }
    // previously we used new AllAssignmentIterator(variables)
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      var num: Double = getScore(assignment)
      for (variable <- variables) {
        if (variable != target) {
          val mess = incoming(variable)
          num = max(num, mess.score(assignment.get(variable).get))
        }
      }
      scores(assignment.get(target).get) = num + scores(assignment.get(target).get)
    }
    val varScores: Buffer[Double] = new ArrayBuffer(target.domain.size)
    for (value <- target.domain.values) {
      varScores += scores(value)
    }
    BPUtil.message(target, varScores)
  } */

  def marginalize(incoming: FactorMessages): FactorMessages = {
    val result = new FactorMessages(variables)
    val scores: Array[Array[Double]] = new Array(varyingNeighbors.size)
    // initialize score arrays for varying neighbors
    for (i <- 0 until discreteVarying.length) {
      scores(i) = new Array[Double](discreteVarying(i)._1.domain.size)
    }
    // go through all the assignments of the varying variables
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment, index)
      for (dv <- discreteVarying) {
        val mess = incoming(dv._2)
        num = num + mess.score(assignment(dv._1))
      }
      _marginal(index) = num
    }
    // go through all the assignments of the varying variables
    // and find Z, and calculate the scores
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      for (i <- 0 until discreteVarying.length) {
        val as = assignment(discreteVarying(i)._1).intValue
        scores(i)(as) = max(scores(i)(as), _marginal(index))
      }
    }
    // set the outgoing messages
    for (i <- 0 until discreteVarying.length) {
      val dv = discreteVarying(i)
      val vid = dv._2
      result.set(vid, BPUtil.message(dv._1, scores(i).toSeq))
    }
    // deterministic messages for the fixed neighbors
    for (fv <- fixedNeighbors) {
      val vid = fv._2
      result.set(vid, incoming.get(vid))
    }
    result
  }
}

class MessageNode(val variable: Variable, val varying: Set[Variable]) {
  // TODO: Add support for "changed" flag, i.e. recompute only when value is read, and hasnt changed since last read
  private val _factors = new ArrayBuffer[MessageFactor]
  private val vids = new ArrayBuffer[Int]

  lazy val varies: Boolean = varying contains variable

  protected var _marginal: GenericMessage = if (varies) BPUtil.uniformMessage else BPUtil.deterministicMessage(variable, variable.value)
  protected var _incoming: VarMessages = new VarMessages(factors)
  protected var _outgoing: VarMessages = new VarMessages(factors)

  def marginal = _marginal

  def addFactor(mf: MessageFactor, vid: Int = -1): Int = {
    val pos = _factors.length
    assert(pos == vids.length)
    _factors += mf
    vids += vid
    resetMessages
    pos
  }

  def neighbors = _factors.toSeq

  def factors = _factors.map(_.factor).toSeq

  def priority: Double = currentMaxDelta

  def priority(neighbor: MessageFactor): Double = currentMaxDelta

  // Message from variable to factor
  // No recomputation, unless the incoming message from the factor was deterministic
  def outgoing(mf: MessageFactor, fid:Int): GenericMessage = {
    if (varies) {
      if (_incoming.get(fid).isDeterministic) {
        var msg: GenericMessage = BPUtil.uniformMessage
        for (otherFactor <- neighbors; if (otherFactor != mf))
          msg = msg * otherFactor.outgoing(vids(fid)) // FIXME make sure we're not normalizing accidentally
        msg
      } else {
        _outgoing.get(fid) //marginal / _incoming(factor)
      }
    } else {
      // use the current value as the deterministic message
      BPUtil.deterministicMessage(variable, variable.value)
    }
  }

  // Message from factor to variable
  // stores the message and recomputes the marginal and the outgoing messages
  def incoming(mf: MessageFactor, fid:Int, mess: GenericMessage): Unit = {
    _incoming.set(fid, mess)
    if (varies) {
      //TODO update marginal incrementally?
      //_marginal = (_marginal / _incoming(factor)) * mess
      // set the incoming message
      _marginal = BPUtil.uniformMessage
      for (fid <- 0 until neighbors.length) {
        val mf = neighbors(fid)
        _marginal = _marginal * _incoming.get(fid)
        var msg: GenericMessage = BPUtil.uniformMessage
        for (otherFid <- 0 until neighbors.length; if otherFid != fid) {
          msg = msg * _incoming.get(otherFid)
        }
        _outgoing.set(fid, msg)
      }
    }

  }

  def resetMessages = {
    _incoming = new VarMessages(factors)
    _outgoing = new VarMessages(factors)
  }

  def currentMaxDelta: Double = {
    neighbors.map(_.currentMaxDelta).max
  }

  override def toString = "M(%s)".format(variable)
}

abstract class LatticeBP(val varying: Set[Variable]) extends Lattice[Variable] {

  def this(model: Model, varying: Set[Variable]) = {
    this(varying)
    createUnrolled(model)
  }

  val _nodes = new HashMap[Variable, MessageNode]
  val _factors = new HashMap[Factor, MessageFactor]

  def createFactor(potential: Factor)

  def createFactors(factorsToAdd: Seq[Factor]) {
    for (f <- factorsToAdd) createFactor(f)
  }

  def createUnrolled(model: Model) {
    createFactors(model.factors(varying))
  }

  def nodes: Iterable[MessageNode] = _nodes.values

  def variables: Iterable[Variable] = _nodes.keys

  def node(variable: Variable): MessageNode = _nodes.getOrElseUpdate(variable, {
    new MessageNode(variable, varying)
  })

  def mfactors: Iterable[MessageFactor] = _factors.values

  def factors: Iterable[Factor] = _factors.keys

  def mfactor(factor: Factor): MessageFactor = _factors(factor)

  def setToMaxMarginal(variables: Iterable[MutableVar] = varying.map(_.asInstanceOf[MutableVar]).toSet)(implicit d: DiffList = null): Unit = {
    for (variable <- variables) {
      variable.set(node(variable).marginal.map[variable.Value])(d)
    }
  }

  def resetMessages = {
    mfactors.foreach(_.resetMessages)
    nodes.foreach(_.resetMessages)
  }

  type VariableMarginalType = GenericMessage
  type FactorMarginalType = DiscreteFactorMarginal

  override def marginal(v: Variable) = {
    if (_nodes.contains(v)) Some(node(v).marginal) else None
  }

  override def marginal(f: Factor) = if (_factors.contains(f)) {
    val mf = mfactor(f)
    Some(new DiscreteFactorMarginal(f, f.valuesIterator(mf.varying).map(mf marginal _).toArray))
  } else None

  def currentMaxDelta: Double = {
    _factors.values.map(_.currentMaxDelta).max
  }

  def logZ: Double = {
    //Bethe approximation according to wainwright 08 page 83 top.
    var logZ = 0.0
    for (node <- nodes) {
      val nodeEntropy = node.marginal.entropy
      logZ += (1.0 - node.neighbors.size) * nodeEntropy
    }
    for (edge <- mfactors) {
      //we can join expected edge score and entropy as local logZ of potential
      val factorLogZ = edge.logZ
      logZ += factorLogZ
      // compensate for double counting <incoming, mu>
      for (vid <- 0 until edge.nodes.length) {
        val node = edge.nodes(vid)
        // dot product of node marginals and incoming from node
        val dot = node.marginal.domain.foldLeft(0.0)(
          (d, v) => d + (node.marginal.probability(v) * edge.incoming(vid).score(v)))
        logZ -= dot
      }
    }
    logZ
  }

  def statExpectations: HashMap[DotFamily, Vector] = {
    val exps = new HashMap[DotFamily, Vector]
    for (mf <- mfactors) {
      mf.updateStatExpectations(exps)
    }
    exps
  }

}

trait SumProductLattice {
  this: LatticeBP =>

  def createFactor(potential: Factor) = {
    val factor = new MessageFactor(potential, varying, this) with SumFactor
    _factors(potential) = factor
  }
}

trait MaxProductLattice {
  this: LatticeBP =>

  def createFactor(potential: Factor) = {
    val factor = new MessageFactor(potential, varying, this) with MaxFactor
    _factors(potential) = factor
  }
}