package cc.factorie.bp

import cc.factorie._
import cc.factorie.optimize.OptimizableByValueAndGradient
import cc.factorie.la.{SparseVector, ArrayFromVectors, Vector, DenseVector}
import collection.mutable.{ArrayBuffer, HashMap, Map}

/**
 * @author sameer
 * @date 12/22/11
 */

trait Regularizer {
  def families: Seq[DotFamily]

  def regValue: Double = 0.0

  def regGradients(gradients: Map[DotFamily, Vector]): Unit = {}
}

trait L2Regularizer extends Regularizer {
  def sigmaSq: Double = 10.0

  override def regValue = {
    -(families.foldLeft(0.0)(
      (tot: Double, t: DotFamily) =>
        tot + t.weights.activeElements.foldLeft(0.0)(
          (totw, v) => totw + math.pow(v._2, 2)))) / (2.0 * sigmaSq)
  }

  override def regGradients(gradients: Map[DotFamily, Vector]) = {
    for (df <- families) {
      val wv = df.weights
      val gv = gradients.getOrElseUpdate(df, df.newWeightsTypeVector)
      for (i: Int <- wv.activeDomain) {
        val reg: Double = wv(i) / sigmaSq
        gv(i) = gv(i) - reg
      }
    }
  }
}

class Trainer(val pieces: Seq[Piece], val families: Seq[DotFamily])
      extends OptimizableByValueAndGradient with Regularizer {

  def this(model: Model, pieces: Seq[Piece]) = this(pieces, model.familiesOfClass[DotFamily].toSeq)

  var _weights: ArrayFromVectors = null
  var _gradients: ArrayFromVectors = null
  var _changed: Boolean = true
  var _value: Double = Double.NaN
  init

  def init = {
    _weights = new ArrayFromVectors(families.map(_.weights))
    println("Number of pieces: " + pieces.length)
    println("Number of active weights: %s (%d)".format(families.map(_.weights.activeDomainSize).mkString(", "), _weights.vectorsArraySize))
    println("Number of total weights:  %s (%d)".format(families.map(_.weights.length).mkString(", "), families.foldLeft(0)(_ + _.weights.length)))
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
        grads.getOrElseUpdate(df, df.newWeightsTypeVector) += pg(df)
      }
      if (pieces.length >= 10000 && i % (pieces.length / 25) == 0)
        println("Done %d of %d pieces".format(i, pieces.length))
    }
    //println("--- Normalizing ---")
    //_value *= (1.0 / pieces.length)
    //grads.values.foreach(g => g.activeDomain.foreach(i => g(i) = g(i) * (1.0 / pieces.length)))
    println("Regularizing")
    // include the regularization
    _value += regValue
    regGradients(grads)
    // project the gradient
    grads.foreach(p => projectGradient(p._1, p._2))
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

  def projectGradient(df: DotFamily, grad: Vector): Unit = {}
}

class ParallelTrainer(pieces: Seq[Piece], families: Seq[DotFamily])
      extends Trainer(pieces, families) {

  def this(model: Model, pieces: Seq[Piece]) = this(pieces, model.familiesOfClass[DotFamily].toSeq)

  var seqCalls = 0
  var combCalls = 0

  class ValAndGrad(var value: Double = 0.0, val grad: HashMap[DotFamily, Vector] = new HashMap) {

    def augment(piece: Piece): ValAndGrad = {
      val (pv, pg) = piece.valueAndGradient
      val result = new HashMap[DotFamily, Vector]
      for (df <- grad.keySet ++ pg.keySet) {
        val v = if (grad contains df) grad(df) else df.newWeightsTypeVector
        if (pg contains df) v += pg(df)
        result(df) = v
      }
      seqCalls += 1
      // project the gradient
      result.foreach(p => projectGradient(p._1, p._2))
      new ValAndGrad(value + pv, result)
    }

    def combineWith(vandg: ValAndGrad): ValAndGrad = {
      val a: HashMap[DotFamily, Vector] = grad
      val b: HashMap[DotFamily, Vector] = vandg.grad
      val c = new HashMap[DotFamily, Vector]
      for (df <- a.keySet ++ b.keySet) {
        val v = df.newWeightsTypeVector
        if (a contains df) v += a(df)
        if (b contains df) v += b(df)
        c(df) = v
      }
      combCalls += 1
      // project the gradient
      c.foreach(p => projectGradient(p._1, p._2))
      new ValAndGrad(value + vandg.value, c)
    }
  }

  override def updateValueAndGradient: Unit = {
    // compute total grad and value of the pieces
    val result = pieces.par.aggregate({
      new ValAndGrad
    })(_ augment _, _ combineWith _)
    println("--- seqCalls: %d, combCalls: %d".format(seqCalls, combCalls))
    //result.value *= (1.0 / pieces.length)
    //result.grad.values.foreach(g => g.activeDomain.foreach(i => g(i) = g(i) * (1.0 / pieces.length)))
    //println("--- Normalized ---")
    seqCalls = 0
    combCalls = 0
    _value = result.value
    // include the regularization
    _value += regValue
    regGradients(result.grad)
    // project the gradient
    result.grad.foreach(p => projectGradient(p._1, p._2))
    // create the gradient
    _gradients = new ArrayFromVectors(families.map(result.grad(_)))
  }
}


class SGDTrainer(val pieces: Seq[Piece], val families: Seq[DotFamily],
                 val minibatchSize: Int, var initialLearningRate: Double,
                 val decayDamping: Double, val l2: Double,
                 val l1: Double = 0.0, val verbose: Boolean = true,
                 val calibrateLrSteps: Int = 10) {
  var t = 0.0

  def lrate(t: Double) = initialLearningRate / (1 + l2 * t)

  families.foreach(_.freezeDomains)
  val gradients = families.map(f => ArrayBuffer[Vector]()).toArray

  var batches: Seq[Seq[Piece]] = null

  def initializeBatches() {
    batches = pieces.shuffle.grouped(minibatchSize).toSeq
  }

  def updateGradients(batch: Int) = {
    var obj = 0.0
    assert(batch < batches.length, "%d should be less than %d".format(batch, batches.length))
    for (vg <- batches(batch).map(_.valueAndGradient)) {
      vg._2.foreach(g => projectGradient(g._1, g._2))
      obj += vg._1
      val pg = vg._2
      var i = 0
      families.foreach(f => {
        gradients(i).append(pg(f) * (1.0 / batches(batch).length))
        i += 1
      })
    }
    obj
  }

  def truncate(x: Double) = math.signum(x) * math.max(0.0, math.abs(x) - l1)

  def addGradients() {
    var f = 0
    val lr: Double = lrate(t)
    while (f < families.length) {
      val fam = families(f)
      val grad = gradients(f)
      if (l1 == 0.0) {
        val mult = 1.0 - l2 * lr
        val scaledWeights = fam.weights * mult
        fam.setWeights(scaledWeights)
        grad.foreach(g => fam.weights += g * lr)
      } else {
        var i = 0
        while (i < fam.weights.length) {
          fam.weights(i) = truncate(fam.weights(i) + lr * (grad.map(_(i)).sum - l2 * fam.weights(i)))
          i += 1
        }
      }
      grad.clear()
      f += 1
    }
  }

  def doStep(i: Int) = {
    t += 1
    val r = updateGradients(i)
    addGradients()
    r
  }

  def testLr(eta: Double) = {
    val oldLr = initialLearningRate
    initialLearningRate = eta
    val oldWeights = families.map(f => {
      val a = f.newWeightsTypeVector
      a += f.weights
      a
    }).toArray
    var obj = 0.0
    var i = 0
    while (i < math.min(batches.length, calibrateLrSteps)) {
      obj += doStep(i)
      i += 1
    }
    t -= math.min(batches.length, calibrateLrSteps)
    initialLearningRate = oldLr
    i = 0
    obj -= (5.0 * l2 / batches.length) * families.sumDoubles(f => f.weights.dot(f.weights))
    while (i < families.length) {
      val fam = families(i)
      val ws = oldWeights(i)
      assert(fam.weights.activeDomainSize == ws.activeDomainSize)
      for (j <- fam.weights.activeDomain) {
        fam.weights(j) = ws(j)
      }
      i += 1
    }
    if (verbose) println("lr: " + eta + " obj: " + obj)
    obj
  }

  def calibrateLearningRate() {
    val eta = List(100.0, 10.0, 1.0, 0.1, 0.01).map(_ * initialLearningRate).map(r => (r, testLr(r))).maxBy(x => (x._2))._1
    initialLearningRate = eta
    if (verbose) println("Using learning rate " + initialLearningRate)
  }

  def iterate() {
    //t = 0
    initializeBatches()
    if (calibrateLrSteps > 0) calibrateLearningRate()
    for (i <- 0 until batches.length)
      doStep(i)
  }

  def projectGradient(df: DotFamily, grad: Vector): Unit = {}
}
