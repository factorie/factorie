package cc.factorie.bp

import scala.math._
import cc.factorie._
import cc.factorie.la._
import collection.mutable.{ArrayBuffer, HashMap}

/**
 * todo: we should define messages in a way that they are always
 * defined over Any, but return 0/-inf for most values. This
 * will reduce type clutter, and should be sufficient for
 * all operations we need.
 */
trait GenericMessage extends cc.factorie.Marginal {
  self =>

  import StrictMath._

  def *(that: GenericMessage): GenericMessage

  def /(that: GenericMessage): GenericMessage

  def inverse: GenericMessage

  def deterministicValue: Option[Any]

  def unnormalized(value: Any) = exp(score(value))

  def probability(value: Any) = unnormalized(value) / Z

  def sample[A]: A = domain.sampleExpProportionally(score(_)).asInstanceOf[A]

  def score(value: Any): Double

  def domain: Seq[Any]

  def defaultScore: Double = Double.NegativeInfinity

  def Z: Double

  def entropy = {
    var result = 0.0
    for (x <- domain) {
      val prob = probability(x)
      result -= prob * log(prob)
    }
    assert(!result.isNaN, () => toString)
    assert(!result.isInfinity, () => toString)
    result
  }

  def dynamicRange = {
    if (domain.size == 1) 0.0
    else {
      val ratios = for (i <- domain; j <- domain; if (i != j)) yield {
        val prob_i = probability(i)
        val prob_j = probability(j)
        prob_i / prob_j
      }
      val maxRatio = ratios.max
      if (!maxRatio.isNaN) assert(maxRatio >= 1.0)
      val dr = sqrt(maxRatio)
      if (dr.isNaN) Double.PositiveInfinity else dr
    }
  }

  def ranked: Seq[(Any, Double)] = domain.map(v => v -> score(v)).sortBy(-_._2)

  def isUniform = false

  def isDeterministic = false

  def map[A]: A = domain.maxByDouble(v => score(v)).asInstanceOf[A]

  def map2[A]: (A, A) = domain.asInstanceOf[Seq[A]].max2ByDouble(v => score(v))

  def mapWithThreshold[A](threshold: Double, default: A): A = {
    // get the best two values
    val (v1, v2) = map2[A]
    // pick the one to check for threshold (should be non-default)
    val value = if (v1 == default) v2 else v1
    if (probability(value) > threshold) value else default
  }

  override def toString: String = domain.map(x => "%s:%5.5f/%f".format(x, probability(x), score(x))).mkString(",")
}

trait GenericUniformMessage extends GenericMessage {
  def defaultValue = null

  def Z = error("Can't sum over domain of the uniform message")

  def domain = error("Can't iterate over domain of the uniform message")

  def score(value: Any) = 0.0

  def deterministicValue = None

  def /(that: GenericMessage) = that.inverse

  def *(that: GenericMessage) = that

  def inverse = this

  override def toString = "Uniform"

  override def dynamicRange = 1.0
}

object UniformMessage extends GenericUniformMessage {

  def sample = null

  override def isUniform = true
}

/**
 * An immutable message that is defined over the complete domain of a discrete variable.
 * @param scores - Array of unnormalized scores defined over the domain of the variable
 * @param variable - The variable that the message is defined over
 * @tparam DV - Type of the variable that the message is defined over
 */
class DiscreteMessage[DV <: DiscreteVariable](val variable: DV, val scores: Vector) extends GenericMessage {
  assert(!scores.exists(_.isNaN), "Nan in scores: " + scores)
  assert(!scores.exists(_.isPosInfinity), "Pos Inf in scores: " + scores)
  assert(scores.length == variable.domain.size, "Message score should be defined on the whole domain")

  type Value = DV#ValueType

  override def domain: Seq[Value] = variable.domain.values

  override def *(that: GenericMessage) = that match {
    case m if (m.isUniform) => this
    case d: DeterministicMessage[_] => d * this
    case d: DiscreteMessage[DV] => new DiscreteMessage(variable, {
      val dv = new DenseVector(scores.length)
      dv += scores
      dv += d.scores
      dv
    })
    case _ => sys.error("Need compatible messages")
  }

  override def /(that: GenericMessage) = that match {
    case m if (m.isUniform) => this
    case d: DeterministicMessage[_] => sys.error("Cannot divide regular message with Deterministic Message")
    case d: DiscreteMessage[DV] => new DiscreteMessage(variable, {
      val dv = new DenseVector(scores.length)
      dv += scores
      dv += (d.scores * -1)
      dv
    })
    case _ => sys.error("Need compatible messages")
  }

  override def score(value: Any) = {
    value match {
      //      case i: Int => scores(i)
      case v: Value => scores(v.intValue)
      case _ => Double.NegativeInfinity
    }
  }

  override lazy val entropy = {
    var result = 0.0
    for (i <- 0 until scores.size) {
      val score = scores(i)
      val prob = exp(score) / Z
      result -= prob * log(prob)
    }
    result
  }

  lazy val Z = scores.map(exp(_)).sum

  def deterministicValue: Option[Value] =
    if (scores.exists(s => s.isPosInfinity)) {
      domain.find(score(_) == Double.PositiveInfinity)
    } else if (scores.exists(s => s.isNegInfinity)) {
      domain.find(score(_) != Double.NegativeInfinity)
    } else None

  lazy val inverse = new DiscreteMessage(variable, scores * -1)

  def defaultValue = domain.head

  override lazy val isDeterministic = scores.exists(s => s.isPosInfinity || s.isNegInfinity)

  override lazy val isUniform = scores.min == scores.max
}

class DeterministicMessage[Value](val value: Value) extends GenericMessage {
  def inverse = sys.error("inverse not implemented for deterministic message")

  def domain = Seq(value)

  override lazy val isDeterministic = true

  override lazy val deterministicValue: Option[Any] = Some(value)

  override def sample[A]: A = value match {
    case a: A => a
  }

  override def map[A] = sample[A]

  override def map2[A] = value match {
    case a: A => Pair(a, a)
  }

  override def *(that: GenericMessage) = that match {
    case m if (m.isUniform) => this
    case d: DeterministicMessage[Value] => {
      if (d.value != value)
        error("Cannot multiply %s with %s".format(value, d.value))
      else this
    }
    case d: DiscreteMessage[_] => this
    case _ => error("Need compatible messages")
  }

  override def /(that: GenericMessage) = that match {
    case m if (m.isUniform) => this
    case d: DeterministicMessage[Value] => {
      if (d.value != value)
        error("Cannot divide %s with %s".format(value, d.value))
      else UniformMessage
    }
    case d: DiscreteMessage[_] => this // TODO: correct?
    case _ => error("Need compatible messages")
  }

  override def score(arg: Any) = {
    arg match {
      case m if (m == value) => 0.0
      case _ => Double.NegativeInfinity
    }
  }

  override def entropy = 0.0

  override def dynamicRange = Double.PositiveInfinity

  override lazy val Z = 1.0

  override def probability(other: Any) = other match {
    case v if (value == other) => 1.0
    case _ => 0.0
  }
}

abstract class Messages[T](val neighbors: Seq[T]) extends Seq[GenericMessage] {

  val length = neighbors.length
  val _msgs: ArrayBuffer[GenericMessage] = ArrayBuffer.fill(length)(BPUtil.uniformMessage)

  def apply(i: Int) = get(i)

  def iterator = _msgs.iterator

  def get(i: Int): GenericMessage = {
    assert(i < length)
    _msgs(i)
  }

  def set(i: Int, msg: GenericMessage) = {
    assert(i < length)
    _msgs(i) = msg
  }

  def typed[M <: GenericMessage](i: Int) = this(i).asInstanceOf[M]

  def copy(messages: Messages[T]) = {
    assert(length == messages.length)
    (0 until length) foreach (i => set(i, messages.get(i)))
  }

  def reset = {
    (0 until length) foreach (i => set(i, BPUtil.uniformMessage))
  }

  override def toString: String = (0 until length).map(i => "%-10s %s".format(neighbors(i), get(i))).mkString("\n")
}

class FactorMessages(vars: Seq[Variable]) extends Messages[Variable](vars) {
  def get(e: Edge): GenericMessage = get(e.vid)

  def set(e: Edge, msg: GenericMessage): Unit = set(e.vid, msg)
}

class VarMessages(factors: Seq[Factor]) extends Messages[Factor](factors) {
  def get(e: Edge): GenericMessage = get(e.fid)

  def set(e: Edge, msg: GenericMessage): Unit = set(e.fid, msg)
}

case class MessageOperationNotSupported(thisType: GenericMessage, thatType: GenericMessage)
      extends RuntimeException("Messages of type %s can't be combined with messages of type %s".format(
        thisType.getClass.getSimpleName, thatType.getClass.getSimpleName))

case class CantCompute(function: String) extends RuntimeException("Can't compute " + function)

trait Marginal {
  var _marginal: GenericMessage = UniformMessage

  def setMarginal(msg: GenericMessage) = _marginal = msg

  def marginal = _marginal
}