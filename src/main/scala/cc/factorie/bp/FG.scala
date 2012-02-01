package cc.factorie.bp

import StrictMath._
import collection.mutable.{HashSet, ArrayBuffer, HashMap, Queue, Buffer}
import cc.factorie._
import cc.factorie.la.{DenseVector, SparseVector, Vector}

/**
 * @author sameer, apassos
 */

abstract class MessageFactor(val factor: Factor, val varying: Set[Variable], fg: FG) {

  val variables: Seq[Variable] = factor.variables
  // set of neighbors that are varying
  lazy val varyingNeighbors: Set[Variable] = {
    val set: HashSet[Variable] = new HashSet
    set ++= discreteVarying
    set.toSet
  }
  // same as above, but in a ordered and DiscreteVariable form
  val discreteVarying: Seq[DiscreteVariable] = variables.filter(varying contains _).map(_.asInstanceOf[DiscreteVariable])
  // complement of varyingNeighbors
  val fixedNeighbors: Set[Variable] = variables.toSet -- varyingNeighbors
  val nodes: Seq[MessageNode] = variables.map(fg.node(_))
  // adds itself to the neighbor's node
  for (node <- nodes) node.factors += this

  val _incoming = new FactorMessages
  val _outgoing = new FactorMessages
  private val _valuesSize: Int = discreteVarying.foldLeft(1)(_ * _.domain.size)
  protected val _marginal: Array[Double] = new Array(_valuesSize)
  private var _cache: Array[Double] = Array.fill(_valuesSize)(Double.NaN)

  def clearCache = _cache = Array.fill(_valuesSize)(Double.NaN)

  def incoming: FactorMessages = _incoming

  def outgoing: FactorMessages = _outgoing

  // Maintain deltas of consecutive outgoing messages
  val outgoingDeltas = new FactorMessages

  // return the stored marginal probability for the given value
  def marginal(values: Values): Double = _marginal(values.index(varyingNeighbors))


  def getScore(assignment: Values, index: Int = -1): Double = {
    assignment.statistics.score
  }


  def updateAllOutgoing() {
    val result = marginalize(incoming)
    // remarginalize, but for individual variables separately
    //  for (node <- nodes) {
    //    val mess = marginalize(node.variable, incoming)
    //    setSingleOutgoing(node.variable, mess)
    //    node.incoming(this, mess)
    //  }
    for (node <- nodes) {
      //todo: fix case where both incoming and potential are deterministic, leading to a uniform outgoing
      val mess = result(node.variable) / incoming(node.variable)
      setSingleOutgoing(node.variable, mess)
      node.incoming(this, mess)
    }
  }

  def prepareIncoming() = nodes.foreach(prepareSingleIncoming(_))

  def setSingleOutgoing(variable: Variable, message: GenericMessage) {
    if (outgoing(variable).isDeterministic) {
      outgoingDeltas(variable) = outgoing(variable)
    }
    else {
      outgoingDeltas(variable) = message / outgoing(variable)
    }
    outgoing(variable) = message
  }

  def prepareSingleIncoming(node: MessageNode) = incoming(node.variable) = node.outgoing(this)

  def currentMaxDelta: Double = {
    if (outgoingDeltas.size == 0) Double.PositiveInfinity
    else outgoingDeltas.values.map(m => {
      val dynamicRange = m.dynamicRange
      assert(!dynamicRange.isNaN)
      log(dynamicRange)
    }).max
  }

  def logZ: Double = {
    var Z = 0.0
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      var num: Double = getScore(assignment)
      for (variable <- variables) {
        val mess = incoming(variable)
        num += mess.score(assignment.get(variable).get)
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

  def marginalize(target: DiscreteVariable, incoming: FactorMessages): GenericMessage

  def marginalize(incoming: FactorMessages): FactorMessages
}

trait SumFactor extends MessageFactor {

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
  }

  def marginalize(incoming: FactorMessages): FactorMessages = {
    val result = new FactorMessages()
    val scores: Array[Array[Double]] = new Array(varyingNeighbors.size)
    // initialize score arrays for varying neighbors
    for (i <- 0 until discreteVarying.length) {
      scores(i) = new Array[Double](discreteVarying(i).domain.size)
    }
    var maxLogScore = Double.NegativeInfinity
    // go through all the assignments of the varying variables
    // and find the maximum score for numerical reasons
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment, index)
      for (dv <- discreteVarying) {
        val mess = incoming(dv)
        num += mess.score(assignment(dv))
      }
      if (num > maxLogScore) maxLogScore = num
      _marginal(index) = num
    }
    // go through all the assignments of the varying variables
    // and find Z, and calculate the scores
    var Z = 0.0
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      val num = _marginal(index) - maxLogScore
      val expnum = exp(num)
      assert(!expnum.isInfinity)
      _marginal(index) = expnum
      for (i <- 0 until discreteVarying.length) {
        scores(i)(assignment(discreteVarying(i)).intValue) += expnum
      }
      Z += expnum
    }
    (0 until _marginal.size).foreach(i => _marginal(i) /= Z)
    // set the outgoing messages
    for (i <- 0 until discreteVarying.length) {
      result(discreteVarying(i)) = BPUtil.message(discreteVarying(i), scores(i).map(s => log(s)).toSeq)
    }
    // deterministic messages for the fixed neighbors
    for (variable <- fixedNeighbors) {
      result(variable) = incoming(variable)
    }
    result
  }

}

trait MaxFactor extends MessageFactor {

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
  }

  def marginalize(incoming: FactorMessages): FactorMessages = {
    val result = new FactorMessages()
    val scores: Array[Array[Double]] = new Array(varyingNeighbors.size)
    // initialize score arrays for varying neighbors
    for (i <- 0 until discreteVarying.length) {
      scores(i) = new Array[Double](discreteVarying(i).domain.size)
    }
    // go through all the assignments of the varying variables
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      var num: Double = getScore(assignment, index)
      for (dv <- discreteVarying) {
        val mess = incoming(dv)
        num = num + mess.score(assignment(dv))
      }
      _marginal(index) = num
    }
    // go through all the assignments of the varying variables
    // and find Z, and calculate the scores
    for (assignment: Values <- factor.valuesIterator(varyingNeighbors)) {
      val index = assignment.index(varyingNeighbors)
      for (i <- 0 until discreteVarying.length) {
        val as = assignment(discreteVarying(i)).intValue
        scores(i)(as) = max(scores(i)(as), _marginal(index))
      }
    }
    // set the outgoing messages
    for (i <- 0 until discreteVarying.length) {
      result(discreteVarying(i)) = BPUtil.message(discreteVarying(i), scores(i).toSeq)
    }
    // deterministic messages for the fixed neighbors
    for (variable <- fixedNeighbors) {
      result(variable) = incoming(variable)
    }
    result
  }
}

class MessageNode(val variable: Variable, val varying: Set[Variable]) {
  // TODO: Add support for "changed" flag, i.e. recompute only when value is read, and hasnt changed since last read
  val factors = new ArrayBuffer[MessageFactor]

  lazy val varies: Boolean = varying contains variable

  def neighbors = factors.toSeq

  def priority: Double = currentMaxDelta

  def priority(neighbor: MessageFactor): Double = currentMaxDelta

  protected var _marginal: GenericMessage = if (varies) BPUtil.uniformMessage else BPUtil.deterministicMessage(variable, variable.value)
  protected var _incoming: VarMessages = new VarMessages
  protected var _outgoing: VarMessages = new VarMessages

  def marginal = _marginal

  // Message from variable to factor
  // No recomputation, unless the incoming message from the factor was deterministic
  def outgoing(mf: MessageFactor): GenericMessage = {
    if (varies) {
      if (_incoming(mf.factor).isDeterministic) {
        var msg: GenericMessage = BPUtil.uniformMessage
        for (otherFactor <- factors; if (otherFactor != mf))
          msg = msg * otherFactor.outgoing(variable) // FIXME make sure we're not normalizing accidentally
        msg
      } else {
        _outgoing(mf.factor) //marginal / _incoming(factor)
      }
    } else {
      // use the current value as the deterministic message
      BPUtil.deterministicMessage(variable, variable.value)
    }
  }

  // Message from factor to variable
  // stores the message and recomputes the marginal and the outgoing messages
  def incoming(mf: MessageFactor, mess: GenericMessage): Unit = {
    _incoming(mf.factor) = mess
    if (varies) {
      //TODO update marginal incrementally?
      //_marginal = (_marginal / _incoming(factor)) * mess
      // set the incoming message
      _marginal = BPUtil.uniformMessage
      for (mf: MessageFactor <- factors) {
        _marginal = _marginal * _incoming(mf.factor)
        var msg: GenericMessage = BPUtil.uniformMessage
        for (other: MessageFactor <- factors; if other != mf) {
          msg = msg * _incoming(other.factor)
        }
        _outgoing(mf.factor) = msg
      }
    }

  }

  def currentMaxDelta: Double = {
    factors.map(_.currentMaxDelta).max
  }

  override def toString = "M(%s)".format(variable)
}

abstract class FG(val varying: Set[Variable]) {

  def this(model: Model, varying: Set[Variable]) = {
    this (varying)
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

  def currentMaxDelta: Double = {
    _factors.values.map(_.currentMaxDelta).max
  }

  def logZ: Double = {
    //Bethe approximation according to wainwright 08 page 83 top.
    var logZ = 0.0
    for (node <- nodes) {
      val nodeEntropy = node.marginal.entropy
      logZ += (1.0 - node.factors.size) * nodeEntropy
    }
    for (edge <- mfactors) {
      //we can join expected edge score and entropy as local logZ of potential
      val factorLogZ = edge.logZ
      logZ += factorLogZ
      // compensate for double counting <incoming, mu>
      for (node <- edge.nodes) {
        // dot product of node marginals and incoming from node
        val dot = node.marginal.domain.foldLeft(0.0)(
          (d, v) => d + (node.marginal.probability(v) * edge.incoming(node.variable).score(v)))
        logZ -= dot
      }
    }
    logZ
  }

  def inferLoopyBP(iterations: Int = 1) {
    for (iteration <- 0 until iterations) {
      for (factor <- _factors.values.shuffle(cc.factorie.random)) {
        //for every factor first calculate all incoming beliefs
        factor.prepareIncoming()
        //synchronous belief updates on all outgoing edges
        factor.updateAllOutgoing()
      }
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

  private def _bfs(root: MessageNode, checkLoops: Boolean): Seq[MessageFactor] = {
    val visited: HashSet[MessageFactor] = new HashSet
    val result = new ArrayBuffer[MessageFactor]
    val toProcess = new Queue[(MessageNode, MessageFactor)]
    root.factors foreach (f => toProcess += Pair(root, f))
    while (!toProcess.isEmpty) {
      val (origin, factor) = toProcess.dequeue()
      if (!checkLoops || !visited(factor)) {
        visited += factor
        result += factor
        for (node <- factor.nodes; if (node != origin && node.varies)) {
          node.factors filter (_ != factor) foreach (f => toProcess += Pair(node, f))
        }
      }
    }
    result
  }

  def inferUpDown(variable: DiscreteVariable, checkLoops: Boolean = true): Unit = inferTreewise(node(variable), checkLoops)

  // Perform a single iteration of up-down BP using the given root to discover the tree. For
  // loopy models, enable checkLoops to avoid infinite loops.
  def inferTreewise(root: MessageNode, checkLoops: Boolean = true) {
    // perform BFS
    val bfsOrdering: Seq[MessageFactor] = _bfs(root, checkLoops)
    // send messages leaf to root
    for (factor <- bfsOrdering.reverse) {
      factor.prepareIncoming()
      factor.updateAllOutgoing()
    }
    // send root to leaves
    for (factor <- bfsOrdering) {
      factor.prepareIncoming()
      factor.updateAllOutgoing()
    }
  }

  // Perform up-down scheduling of messages. Picks a random root, and performs BFS
  // ordering to discover the tree (efficiently if checkLoops is false), resulting in
  // exact inference for tree-shaped models. For loopy models, enable checkLoops to avoid
  // infinite loops, and have iterations>1 till convergence.
  def inferTreeUpDown(iterations: Int = 1, checkLoops: Boolean = true) {
    for (iteration <- 0 until iterations) {
      // find a random root
      val root = nodes.sampleUniformly
      // treewise
      inferTreewise(root, checkLoops)
      // println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
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

  def statExpectations: HashMap[DotFamily, Vector] = {
    val exps = new HashMap[DotFamily, Vector]
    for (mf <- mfactors) {
      mf.updateStatExpectations(exps)
    }
    exps
  }

}

trait SumProductFG {
  this: FG =>

  def createFactor(potential: Factor) = {
    val factor = new MessageFactor(potential, varying, this) with SumFactor
    _factors(potential) = factor
  }
}

trait MaxProductFG {
  this: FG =>

  def createFactor(potential: Factor) = {
    val factor = new MessageFactor(potential, varying, this) with MaxFactor
    _factors(potential) = factor
  }
}