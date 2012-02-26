package cc.factorie.bp

import StrictMath._
import collection.mutable.{HashSet, ArrayBuffer, HashMap, Queue, Buffer}
import cc.factorie._
import cc.factorie.la.{DenseVector, SparseVector, Vector}

/**
 * @author sameer, apassos, bmartin
 */

class Edge(val fid: Int,
           val vid: Int,
           val f: MessageFactor,
           val n: MessageNode) {
  // Send message from the variable to the factor
  def vToF = f.receive(this)

  // Send message from the factor to the variable
  def fToV = f.send(this)

  override def toString = "%s -- %s".format(f.factor, n.variable)
}

abstract class MessageFactor(val factor: Factor, val varying: Set[DiscreteVariable], fg: LatticeBP) {
  val variables: Seq[Variable] = factor.variables
  val _incoming = new FactorMessages(variables)
  val _outgoing = new FactorMessages(variables)
  // same as above, but in a ordered and DiscreteVariable form
  val discreteVarying: Seq[(DiscreteVariable, Int)] =
    variables.zipWithIndex.filter(_._1.isInstanceOf[DiscreteVariable])
          .map(p => Pair(p._1.asInstanceOf[DiscreteVariable], p._2))
          .filter(varying contains _._1)
  // complement of varyingNeighbors
  val fixedNeighbors: Seq[(DiscreteVariable, Int)] =
    variables.zipWithIndex.filter(_._1.isInstanceOf[DiscreteVariable])
          .map(p => Pair(p._1.asInstanceOf[DiscreteVariable], p._2))
          .filter(p => !varying.contains(p._1))
  // set of neighbors that are varying
  val varyingNeighbors: Set[Variable] = {
    val set: HashSet[Variable] = new HashSet
    set ++= discreteVarying.map(_._1)
    set.toSet
  }
  val edges: Seq[Edge] = (0 until variables.length).map(vid => {
    val variable = variables(vid)
    val node = fg.node(variable)
    node.addEdge(vid, this)
  })

  val nodes = edges.map(_.n).toSeq

  protected val _valuesSize: Int = discreteVarying.foldLeft(1)(_ * _._1.domain.size)
  protected val _marginal: Array[Double] = Array.fill(_valuesSize)(Double.NaN)
  protected var _remarginalize: Boolean = true

  // caching of the scores
  private var _cache: Array[Double] = Array.fill(_valuesSize)(Double.NaN)

  // Maintain deltas of consecutive send messages
  val outgoingDeltas = new FactorMessages(variables)

  def clearCache = _cache = Array.fill(_valuesSize)(Double.NaN)

  def resetMessages = {
    _incoming.reset
    _outgoing.reset
    (0 until _valuesSize).foreach(i => _marginal(i) = Double.NaN)
    _remarginalize = true
  }

  protected def getScore(assignment: Values, index: Int = -1): Double = {
    assignment.statistics.score
  }

  def incoming(e: Edge): GenericMessage = _incoming.get(e)

  def outgoing(e: Edge): GenericMessage = _outgoing.get(e)

  private def setIncoming(e: Edge, message: GenericMessage) = {
    _incoming.set(e, message)
    _remarginalize = true
  }

  def receive(e: Edge): Unit = setIncoming(e, e.n.send(e))

  def receiveFromAll = edges.foreach(e => receive(e))

  private def setOutgoing(e: Edge, message: GenericMessage) {
    if (outgoing(e).isDeterministic) {
      outgoingDeltas.set(e, outgoing(e))
    } else {
      outgoingDeltas.set(e, message / outgoing(e))
    }
    _outgoing.set(e, message)
  }

  private def incomingToOutgoing = {
    if (_remarginalize) {
      val result = marginalize(_incoming)
      // todo: remarginalize, but for individual variables separately
      // val mess = marginalize(node.variable, incoming)
      for (e <- edges) {
        val mess = if (incoming(e).isDeterministic) incoming(e) else result.get(e) / incoming(e)
        setOutgoing(e, mess)
      }
      _remarginalize = false
    }
  }

  def send(e: Edge) {
    incomingToOutgoing
    e.n.receive(e, outgoing(e))
  }

  def sendToAll = {
    incomingToOutgoing
    //todo: fix case where both incoming and potential are deterministic, leading to a uniform send
    edges.foreach(e => send(e))
  }

  // return the stored marginal probability for the given value
  def marginal(values: Values): Double = {
    incomingToOutgoing
    _marginal(values.index(varyingNeighbors))
  }

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
    var maxScore = Double.NegativeInfinity
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      var num: Double = getScore(assignment)
      for (e <- edges) {
        val mess = incoming(e)
        num += mess.score(assignment(e.n.variable))
      }
      if (maxScore < num) maxScore = num
      val expnum = exp(num)
      Z += expnum
    }
    val lZ = log(Z)
    assert(lZ+0.01 > maxScore, "LogZ %f less than maxScore %f".format(lZ, maxScore))
    lZ
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

  var scores : Array[Array[Double]] = new Array(1)
  var tmpScore: Array[Double] = new Array(1)

  def initializeScores() {
    var i = 0
    while (i < discreteVarying.length) {
      if (scores(i) == null || scores(i).length < discreteVarying(i)._1.domain.size) {
        scores(i) = Array.fill(discreteVarying(i)._1.domain.size)(0.0)
      } else {
        //java.util.Arrays.fill(scores(i), 0.0) should be faster, but isn't
        var j = 0
        while (j < scores(i).length) {
          scores(i)(j) = 0.0
          j += 1
        }
      }
      i += 1
    }
  }

  def sumNeighbors(incoming: FactorMessages) = {
    var maxLogScore = Double.NegativeInfinity
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment, index)
      var i = 0
      while (i < discreteVarying.length) {
        val dv = discreteVarying(i)
        val vid = dv._2
        val mess = incoming.get(vid)
        num += mess.score(assignment(dv._1))
        i += 1
      }
      if (num > maxLogScore) maxLogScore = num
      tmpScore(index) = num
    }
    maxLogScore
  }

  def computeZ(maxLogScore: Double) = {
    var Z = 0.0
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      val num = tmpScore(index) - maxLogScore
      val expnum = exp(num)
      assert(!expnum.isInfinity)
      _marginal(index) = expnum
      var i = 0
      while (i < discreteVarying.length) {
        scores(i)(assignment(discreteVarying(i)._1).intValue) += expnum
        i += 1
      }
      Z += expnum
    }
    Z
  }

  def setMessages(result: FactorMessages, incoming: FactorMessages) {
    var i = 0
    while (i < discreteVarying.length) {
      val dv = discreteVarying(i)
      val vid = dv._2
      result.set(vid, BPUtil.message(dv._1, scores(i).map(s => log(s)).toSeq))
      i += 1
    }
    // deterministic messages for the fixed neighbors
    i = 0
    while (i < fixedNeighbors.length) {
      val fv = fixedNeighbors(i)
      i += 1
      val vid = fv._2
      result.set(vid, incoming.get(vid))
    }
  }

  def marginalize(incoming: FactorMessages): FactorMessages = {
    val result = new FactorMessages(variables)
    if (scores.length < varyingNeighbors.size) {
      scores = new Array(varyingNeighbors.size)
    }
    initializeScores()
    if (tmpScore.length < _valuesSize) tmpScore = Array.fill(_valuesSize)(Double.NaN)
    // go through all the assignments of the varying variables
    // and find the maximum score for numerical reasons
    val maxLogScore = sumNeighbors(incoming)
    val Z = computeZ(maxLogScore)
    var i = 0
    while (i < _marginal.size) {
      _marginal(i) /= Z
      i += 1
    }
    setMessages(result, incoming)
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
    // set the send messages
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

class MessageNode(val variable: Variable, val varying: Set[DiscreteVariable]) {
  private val _edges = new ArrayBuffer[Edge]

  val varies: Boolean = variable match {
    case dv: DiscreteVariable => varying contains dv
    case _ => false
  }

  protected var _marginal: GenericMessage = if (varies) BPUtil.uniformMessage else BPUtil.deterministicMessage(variable, variable.value)
  protected var _incoming: VarMessages = new VarMessages(factors)
  protected var _outgoing: VarMessages = new VarMessages(factors)
  protected var _remarginalize: Boolean = false

  def marginal = {
    marginalize
    _marginal
  }

  def addEdge(vid: Int, mf: MessageFactor): Edge = {
    val pos = _edges.length
    val e = new Edge(pos, vid, mf, this)
    _edges += e
    resetMessages
    e
  }

  def neighbors = _edges.map(_.f).toSeq

  def factors = neighbors.map(_.factor).toSeq

  def edges = _edges.toSeq

  def priority: Double = currentMaxDelta

  def priority(neighbor: MessageFactor): Double = currentMaxDelta

  // Message from factor to variable
  // stores the message
  def receive(e: Edge, mess: GenericMessage): Unit = {
    _incoming.set(e, mess)
    _remarginalize = true
  }

  // ue incoming messages to compute the outgoing messages
  protected def marginalize = {
    if (varies && _remarginalize) {
      //TODO update marginal incrementally?
      //_marginal = (_marginal / _incoming(factor)) * mess
      /*if (_incoming.get(e).isDeterministic) {
        var msg: GenericMessage = BPUtil.uniformMessage
        for (other <- edges; if (other != e))
          msg = msg * _incoming.get(other) // FIXME make sure we're not normalizing accidentally
        msg
      } else {*/
      _marginal = BPUtil.uniformMessage
      for (e <- edges) {
        _marginal = _marginal * _incoming.get(e)
        var msg: GenericMessage = BPUtil.uniformMessage
        for (other <- edges; if other != e) {
          msg = msg * _incoming.get(other)
        }
        _outgoing.set(e, msg)
      }
      _remarginalize = false
    }
  }

  // Message from variable to factor
  // recomputes the marginal and the send messages, if required
  def send(e: Edge): GenericMessage = {
    if (varies) {
      marginalize
      _outgoing.get(e) //marginal / _incoming(factor)
      //}
    } else {
      // use the current value as the deterministic message
      BPUtil.deterministicMessage(variable, variable.value)
    }
  }

  def resetMessages = {
    _incoming = new VarMessages(factors)
    _outgoing = new VarMessages(factors)
    _marginal = if (varies) BPUtil.uniformMessage else BPUtil.deterministicMessage(variable, variable.value)
    _remarginalize = false
  }

  def currentMaxDelta: Double = {
    neighbors.map(_.currentMaxDelta).max
  }

  override def toString = "M(%s)".format(variable)
}

abstract class LatticeBP(val varying: Set[DiscreteVariable]) extends Lattice[Variable] {

  def this(model: Model, varying: Set[DiscreteVariable]) = {
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

  def edges = nodes.map(_.edges).flatten

  def node(variable: Variable): MessageNode = _nodes.getOrElseUpdate(variable, {
    new MessageNode(variable, varying)
  })

  def mfactors: Iterable[MessageFactor] = _factors.values

  def factors: Iterable[Factor] = _factors.keys

  def mfactor(factor: Factor): MessageFactor = _factors(factor)

  def setToMaxMarginal(variables: Iterable[MutableVar] = varying.toSet)(implicit d: DiffList = null): Unit = {
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
    Some(new DiscreteFactorMarginal(f, f.valuesIterator(mf.varying.toSet).map(mf marginal _).toArray))
  } else None

  def currentMaxDelta: Double = {
    _factors.values.map(_.currentMaxDelta).max
  }

  def logZ(debug: Boolean = false): Double = {
    //Bethe approximation according to wainwright 08 page 83 top.
    var logZ = 0.0
    for (node <- nodes) {
      val nodeEntropy = node.marginal.entropy
      logZ += (1.0 - node.neighbors.size) * nodeEntropy
    }
    if (debug) println("post entropy: " + logZ)
    for (mf <- mfactors) {
      //we can join expected edge score and entropy as local logZ of potential
      mf.receiveFromAll
      val factorLogZ = mf.logZ
      logZ += factorLogZ
      if (debug) println("factor: " + factorLogZ + ", logZ: " + logZ)
      // compensate for double counting <incoming, mu>
      for (e <- mf.edges) {
        // dot product of node marginals and incoming from node
        val dot = e.n.marginal.domain.foldLeft(0.0)(
          (d, v) => d + (e.n.marginal.probability(v) * mf.incoming(e).score(v)))
        logZ -= dot
      }
      if (debug) println("post compensation: " + logZ)
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

  var finalPass: Boolean = false

  def createFactor(potential: Factor) = {
    val factor = new MessageFactor(potential, varying, this) with MaxFactor
    _factors(potential) = factor
  }
}
