package cc.factorie.bp

import cc.factorie.{DotFamily, Model}
import cc.factorie.optimize.OptimizableByValueAndGradient
import collection.mutable.{HashMap, Map}
import cc.factorie.la.{ArrayFromVectors, Vector}

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
    println("Number of active weights: " + _weights.vectorsArraySize)
    println("Number of total weights: " + families.foldLeft(0)(_ + _.weights.length))
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
    // create the gradient
    _gradients = new ArrayFromVectors(families.map(result.grad(_)))
  }
}

class SGDTrainer(val pieces: Seq[Piece], val families: Seq[DotFamily], val minibatchSize: Int,
                  val initialLearningRate: Double, val decayDamping: Double, val l2: Double,  val l1: Double) {
  var t = 0.0
  def lrate(t: Double) = initialLearningRate/math.sqrt(decayDamping + t)

  families.foreach(_.freezeDomains)
  val gradients = families.map(f => f.newWeightsTypeVector).toArray
 
  var batches : Seq[Seq[Piece]] = null
  val rng = new util.Random()
  def initializeBatches() {
    rng.shuffle(pieces)
    batches = pieces.grouped(minibatchSize).toSeq
  }

  def updateGradients(batch: Int) {
    for (pg <- batches(batch).map(_.valueAndGradient._2).seq) {
      var i = 0
      families.foreach(f => {
        gradients(i) += pg(f)
        i += 1
      })
    }
  }

  def truncate(x: Double) = math.signum(x)*math.max(0.0, math.abs(x) - l1)

  def addGradients() {
    var f = 0
    val lr = lrate(t)
    while (f < families.length) {
      val fam = families(f)
      val grad = gradients(f)
      for (i <- grad.activeDomain) {
        fam.weights(i) = truncate(fam.weights(i) + lr*(grad(i) - l2*fam.weights(i)))
        grad(i) = 0.0
      }
      f += 1
    }
  }

  def doStep(i: Int) {
    t += 1
    updateGradients(i)
    addGradients()
  }

  def iterate() {
    initializeBatches()
    for (i <- 0 until batches.length)
      doStep(i)
  }
}
