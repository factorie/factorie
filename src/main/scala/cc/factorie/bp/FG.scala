package cc.factorie.bp

import base._
import StrictMath._
import collection.mutable.ArrayBuffer._
import collection.mutable.{Buffer, ArrayBuffer, HashMap}
import cc.factorie.{Model, Values, Factor}

/**
 * @author sameer
 */

class FG {

  type F = MessageFactor
  type N = MessageNode

  class MessageFactor(val potential: Factor) {

    val bpVariables: Set[Var] = potential.variables.map(_.asInstanceOf[BPVariable]).toSet

    val nodes: Set[N] = bpVariables.map(node(_))

    for (node <- nodes) node.factors += this.asInstanceOf[F]

    def priority: Double = currentMaxDelta

    def priority(neighbor: N): Double = currentMaxDelta

    def neighbors: Seq[N] = nodes.toSeq

    val _incoming = new Messages
    val _outgoing = new Messages

    def incoming: Messages = _incoming.synchronized {
      _incoming
    }

    def outgoing: Messages = _outgoing.synchronized {
      _outgoing
    }

    val deltas = new Messages {
      override def default(key: Var) = True
    }

    def setSingleOutgoing(variable: Var, message: GenericMessage) {
      if (outgoing(variable).isDeterministic) {
        deltas(variable) = outgoing(variable)
      }
      else {
        deltas(variable) = message / outgoing(variable)
      }
      //assert(!deltas(variable).dynamicRange.isNaN)
      outgoing(variable) = message
    }

    def prepareSingleIncoming(node: N) = incoming(node.variable) = node.outgoing(this)

    def marginalize(target: Var, incoming: Messages): GenericMessage = {
      if (incoming(target).isDeterministic) return incoming(target)
      val scores: HashMap[Any, Double] = new HashMap[Any, Double] {
        override def default(key: Any) = 0.0
      }
      val valuesIterator = potential.valuesIterator(potential.variables)
      // previously we used new AllAssignmentIterator(variables)
      for (assignment: Values <- valuesIterator) {
        var num: Double = assignment.statistics.score
        for (variable <- bpVariables) {
          if (variable != target) {
            val mess = incoming(variable)
            num += mess.score(assignment.get(variable).get)
          }
        }
        num = exp(num)
        scores(assignment.get(target).get) = num + scores(assignment.get(target).get)
      }
      val varScores: Buffer[Double] = new ArrayBuffer(target.domainSize)
      for (value <- target.bpDomain) {
        varScores += log(scores(value))
      }
      if (varScores.exists(_.isNegInfinity)) {
        //throw new Exception("output message has negInfinity")
      }
      target.message(varScores)
    }

    def updateOutgoing() = {
      for (node <- nodes) {
        val mess = marginalize(node.variable, incoming)
        setSingleOutgoing(node.variable, mess)
        node.incoming(this, mess)
      }
    }

    def prepareIncoming() = nodes.foreach(prepareSingleIncoming(_))

    def currentMaxDelta: Double = {
      if (deltas.size == 0) log(True.dynamicRange)
      else deltas.values.map(m => {
        val dynamicRange = m.dynamicRange
        assert(!dynamicRange.isNaN)
        log(dynamicRange)
      }).max
    }

    def isLocal = nodes.size == 1

    def allDeterministic = nodes.forall(_.value.isDeterministic)
  }

  class MessageNode(val variable: Var) {
    val factors = new ArrayBuffer[F]

    def neighbors = factors.toSeq

    def priority: Double = currentMaxDelta

    def priority(neighbor: F): Double = currentMaxDelta

    protected var _value: GenericMessage = variable.uniformMessage
    protected var _marginal: GenericMessage = variable.uniformMessage
    protected var _incoming: HashMap[F, GenericMessage] = new HashMap[F, GenericMessage] {
      override def default(key: F) = variable.uniformMessage
    }
    protected var _outgoing: HashMap[MessageFactor, GenericMessage] = new HashMap[F, GenericMessage] {
      override def default(key: F) = variable.uniformMessage
    }

    def value: GenericMessage = _value

    def setValue(mess: GenericMessage) = _value = mess

    def marginal = {
      _marginal
      /*
      var msg: GenericMessage = variable.uniformMessage
      for (factor <- factors) {
      msg = msg * factor.outgoing(variable)
    }
      msg
       */
    }

    def outgoing(factor: F): GenericMessage = {
      if (value.isDeterministic)
        value
      else {
        if (_incoming(factor).isDeterministic) {
          var msg: GenericMessage = variable.uniformMessage
          for (otherFactor <- factors; if (otherFactor != factor))
            msg = msg * otherFactor.outgoing(variable)
          msg
        } else {
          _outgoing(factor) //marginal / _incoming(factor)
        }
      }
    }

    def incoming(factor: F, mess: GenericMessage): Unit = {
      //TODO update marginal incrementally?
      //_marginal = (_marginal / _incoming(factor)) * mess
      // set the incoming message
      _incoming(factor) = mess
      _marginal = variable.uniformMessage
      for (factor: F <- factors) {
        _marginal = _marginal * _incoming(factor)
        var msg: GenericMessage = variable.uniformMessage
        for (other: F <- factors; if other != factor) {
          msg = msg * _incoming(other)
        }
        _outgoing(factor) = msg
      }

    }

    def currentMaxDelta: Double = {
      factors.map(_.currentMaxDelta).max
    }

    def setValueToMarginal = _value = marginal
  }

  val nodes = new HashMap[Var, N]
  val factors = new ArrayBuffer[F]

  def newFactor(potential: Factor) = new MessageFactor(potential)

  def newNode(variable: Var) = new MessageNode(variable)

  def createFactor(potential: Factor) {
    val factor = newFactor(potential)
    factors += factor
  }

  def createFactors(factorsToAdd: Seq[Factor]) {
    for (f <- factorsToAdd) createFactor(f)
  }

  def createUnrolled(model: Model, variables: Seq[BPVariable]) {
    createFactors(model.factors(variables))
  }

  def currentMaxDelta: Double = {
    factors.map(_.currentMaxDelta).max
  }

  def infer(iterations: Int = 1) {
    for (iteration <- 0 until iterations) {
      for (factor <- factors) {
        //for every factor first calculate all incoming beliefs
        factor.prepareIncoming()
        //synchronous belief updates on all outgoing edges
        factor.updateOutgoing()
      }
      println("Iteration %d max delta range: %f".format(iteration, currentMaxDelta))
    }
  }

  def node(variable: Var): N = nodes.getOrElseUpdate(variable, {
    val result = newNode(variable)
    result
  })

  def localFactors: Seq[F] = factors.filter(_.isLocal)

  def nonLocalFactors: Seq[F] = factors.filter(!_.isLocal)

  //def setInputMessage(variable: Var, message: GenericMessage) {
  // createFactor(new LocalPotential(variable, message))
  //}

  def getOutputMessage(variable: Var): GenericMessage = {
    node(variable).marginal
  }

  def typedOut[M <: GenericMessage](variable: Var) = getOutputMessage(variable).asInstanceOf[M]

  def setMarginalsAsValues = nodes.values.foreach(n => n.setValueToMarginal)

  def maxMarginal: HashMap[Var, Any]= {
    val result = new HashMap[Var, Any]
    for (node <- nodes.values) {
      result(node.variable) = node.value.map
    }
    result
  }

  /*
  def initUsing(fg: FG) {
    nodes.clear
    factors.clear
    for (factor <- fg.factors) {
      createFactor(factor.asInstanceOf[fg.MessageFactor].potential)
    }
  }
  */

  /*def copyMarginalsTo(fg: FG) {
    for (variable <- nodes.keys) {
      val thisNode = node(variable)
      val thatNode = fg.node(variable)
      thatNode.setValue(thisNode.value)
    }
  }*/

  /*def setAssignmentAsDeterminstic(assignment: Assignment) {
    for (variable <- assignment.variables) {
      if (nodes.contains(variable)) {
        node(variable).setValue(variable.deterministicMessage(assignment.typed(variable)))
      }
    }
  }*/

  /*
  def correctCount(truth: Assignment): Int = {
    var count = 0
    val assignment = maxMarginal
    for (variable <- nodes.keys) {
      if (truth.value(variable) == assignment.value(variable)) count += 1
    }
    count
  }
  */

}
