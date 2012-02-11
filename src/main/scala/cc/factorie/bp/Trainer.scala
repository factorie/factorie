package cc.factorie.bp

import cc.factorie.{DotFamily, Model}
import cc.factorie.optimize.OptimizableByValueAndGradient
import collection.mutable.{HashMap, Map}
import cc.factorie.la.{DenseVector, ArrayFromVectors, Vector}

/**
 * @author sameer
 * @date 12/22/11
 */

trait Regularizer {
  def model: Model

  def regValue: Double = 0.0

  def regGradients(gradients: Map[DotFamily, Vector]): Unit = {}
}

trait L2Regularizer extends Regularizer {
  def sigmaSq: Double = 10.0

  override def regValue = {
    -(model.familiesOfClass[DotFamily].foldLeft(0.0)(
      (tot: Double, t: DotFamily) =>
        tot + t.weights.activeElements.foldLeft(0.0)(
          (totw, v) => totw + math.pow(v._2, 2)))) / (2.0 * sigmaSq)
  }

  override def regGradients(gradients: Map[DotFamily, Vector]) = {
    for (df <- model.familiesOfClass[DotFamily]) {
      val wv = df.weights
      val gv = gradients.getOrElseUpdate(df, new DenseVector(df.statisticsVectorLength))
      for (i: Int <- wv.activeDomain) {
        val reg: Double = wv(i) / sigmaSq
        gv(i) = gv(i) - reg
      }
    }
  }
}

class Trainer(val model: Model, val pieces: Seq[Piece], val families: Seq[DotFamily])
      extends OptimizableByValueAndGradient with Regularizer {

  def this(model: Model, pieces: Seq[Piece]) = this(model, pieces, model.familiesOfClass[DotFamily].toSeq)

  var _weights: ArrayFromVectors = null
  var _gradients: ArrayFromVectors = null
  var _changed: Boolean = true
  var _value: Double = Double.NaN
  init

  def init = {
    _weights = new ArrayFromVectors(families.map(_.weights))
    println("Number of pieces: " + pieces.length)
  }

  def numOptimizableParameters = _weights.vectorsArraySize

  def getOptimizableParameters(a: Array[Double]) = _weights.getVectorsInArray(a)

  def setOptimizableParameters(a: Array[Double]): Unit = {
    _weights.setVectorsFromArray(a)
    _changed = true
  }

  def optimizableParameter(index: Int): Double = _weights.vectorValueAtArrayIndex(index)

  def optimizableParameter_=(index: Int, d: Double): Unit = {
    _changed = true
    _weights.vectorValueAtArrayIndex_=(index, d)
  }

  def updateValueAndGradient: Unit = {
    _value = 0.0
    val grads = new HashMap[DotFamily, Vector]
    // compute total grad and value of the pieces
    println("Computing value and gradient")
    var i = 0
    for (piece <- pieces) {
      i += 1
      val (pv, pg) = piece.valueAndGradient
      _value += pv
      for (df <- pg.keys) {
        grads.getOrElseUpdate(df, new DenseVector(df.statisticsVectorLength)) += pg(df)
      }
      if (pieces.length >= 10000 && i % (pieces.length / 25) == 0)
        println("Done %d of %d pieces".format(i, pieces.length))
    }
    println("Regularizing")
    // include the regularization
    _value += regValue
    regGradients(grads)
    // create the gradient
    _gradients = new ArrayFromVectors(families.map(grads(_)))
  }

  def getOptimizableGradient(g: Array[Double]) = {
    //TimeUtil.snapshot("Calculate Gradients")
    if (_changed) {
      updateValueAndGradient
      _changed = false
    }
    _gradients.getVectorsInArray(g)
  }

  def optimizableValue = {
    if (_changed) {
      updateValueAndGradient
      _changed = false
    }
    _value
  }
}

class ParallelTrainer(model: Model, pieces: Seq[Piece], families: Seq[DotFamily])
      extends Trainer(model, pieces, families) {

  var seqCalls = 0
  var combCalls = 0

  class ValAndGrad(var value: Double = 0.0, val grad: HashMap[DotFamily, Vector] = new HashMap) {

    def augment(piece: Piece): ValAndGrad = {
      val (pv, pg) = piece.valueAndGradient
      val result = new HashMap[DotFamily, Vector]
      for (df <- grad.keySet ++ pg.keySet) {
        val v = if (grad contains df) grad(df) else new DenseVector(df.statisticsVectorLength)
        if (pg contains df) v += pg(df)
        result(df) = v
      }
      seqCalls += 1
      new ValAndGrad(value + pv, result)
    }

    def combineWith(vandg: ValAndGrad): ValAndGrad = {
      val a: HashMap[DotFamily, Vector] = grad
      val b: HashMap[DotFamily, Vector] = vandg.grad
      val c = new HashMap[DotFamily, Vector]
      for (df <- a.keySet ++ b.keySet) {
        val v = new DenseVector(df.statisticsVectorLength)
        if (a contains df) v += a(df)
        if (b contains df) v += b(df)
        c(df) = v
      }
      combCalls += 1
      new ValAndGrad(value + vandg.value, c)
    }
  }

  override def updateValueAndGradient: Unit = {
    // compute total grad and value of the pieces
    val result = pieces.par.aggregate({
      new ValAndGrad
    })(_ augment _, _ combineWith _)
    println("--- seqCalls: %d, combCalls: %d".format(seqCalls, combCalls))
    seqCalls = 0
    combCalls = 0
    _value = result.value
    // include the regularization
    _value += regValue
    regGradients(result.grad)
    // create the gradient
    _gradients = new ArrayFromVectors(families.map(result.grad(_)))
  }
}