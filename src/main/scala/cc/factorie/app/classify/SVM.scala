package cc.factorie.app.classify

import cc.factorie._
import cc.factorie.la._

class SVMTrainer(parallel: Boolean = true) extends ClassifierTrainer {

  def train[L <: LabeledCategoricalVariable[_], F <: DiscreteTensorVar](ll: LabelList[L, F]): Classifier[L] = {
    val cmodel = new LogLinearModel(ll.labelToFeatures, ll.labelDomain, ll.instanceDomain)(ll.labelManifest, ll.featureManifest)

    val numLabels = ll.labelDomain.size
    val numFeatures = ll.featureDomain.size
    val xs: Seq[Tensor1] = ll.map(ll.labelToFeatures(_).tensor.asInstanceOf[Tensor1])
    val ys: Array[Int]   = ll.map(_.intValue).toArray // TODO: tighten the bounds on L so targetIntValue is available here.
    val weightTensor = {
      if (parallel)
        (0 until numLabels).par.map { label => (new LiblinearL2SV).getWeight(xs, ys, label) }
      else
        (0 until numLabels).map { label => (new LiblinearL2SV).getWeight(xs, ys, label) }
    }
    for (f <- 0 until numFeatures;
         (l,t) <- (0 until numLabels).zip(weightTensor)) {
      cmodel.evidenceTemplate.weights(l,f) = t(f)
    }

    new ModelBasedClassifier[L](cmodel, ll.head.domain)
  }
}

class LiblinearL2SV(lossType: Int = 0, cost: Double = 0.1, eps: Double = 1e-5, bias: Double = -1, maxIterations: Int = 1000) {

  @inline private final def GETI(y: Array[Byte], i: Int) = y(i) + 1
  @inline private final def swap(a: Array[Int], i0: Int, i1: Int) { val t = a(i0); a(i0) = a(i1); a(i1) = t }

  def getWeight(xTensors: Seq[Tensor1], ys: Array[Int], currLabel: Int): DenseTensor1 = {

    val N = ys.size
    val D = xTensors.head.length

    val xs = xTensors.map(t => t.activeDomain.asArray)

    val QD = Array.ofDim[Double](N)
    val alpha = Array.ofDim[Double](N)
    val weight = new DenseTensor1(D, 0.0)
    var U, G, d, alpha_old = 0.0

    val index = Array.ofDim[Int](N)
    val aY = Array.ofDim[Byte](N)

    var active_size = N
    var i, j, s, iter = 0
    var yi: Byte = 0
    var xi = Array.ofDim[Int](0)
    var vi = Array.ofDim[Double](0)


    // PG: projected gradient, for shrinking and stopping
    var PG: Double = 0.0
    var PGmax_old = Double.PositiveInfinity
    var PGmin_old = Double.NegativeInfinity

    var PGmax_new, PGmin_new = 0.0

    // for loss function
    val diag = Array[Double](0, 0, 0)
    val upper_bound = Array(cost, 0, cost)

    if (lossType == 2) {
      diag(0) = 0.5 / cost
      diag(2) = 0.5 / cost
      upper_bound(0) = Double.PositiveInfinity
      upper_bound(2) = Double.PositiveInfinity
    }

    i = 0
    while (i < N) {
      index(i) = i
      aY   (i) = if (ys(i) == currLabel) 1 else -1
      QD   (i) = diag(GETI(aY, i))

      if (bias > 0)    QD(i) += bias * bias

      QD(i) += xs(i).length

      i += 1
    }


    iter = -1
    var break = false
    while (iter < maxIterations - 1 && !break) {
      iter += 1

      PGmax_new = Double.NegativeInfinity
      PGmin_new = Double.PositiveInfinity

      i = 0
      while (i < active_size) {
          j = i + random.nextInt(active_size - i)
          swap(index, i, j)
          i += 1
      }

      s = -1
      while (s < active_size - 1) {
        s += 1
        var continu = false

        i  = index(s)
        yi = aY(i)
        xi = xs(i)
        U  = upper_bound(GETI(aY, i))
        G  = if (bias > 0) weight(0) * bias else 0

        j = 0
        while (j < xi.length) {
            G += weight(xi(j))
            j += 1
        }

         G = G * yi - 1
        G += alpha(i) * diag(GETI(aY, i))

        if (alpha(i) == 0) {
          if (G > PGmax_old) {
            active_size -= 1
            swap(index, s, active_size)
            s -= 1
            continu = true
          }
          else PG = Math.min(G, 0)
        }
        else if (alpha(i) == U) {
          if (G < PGmin_old) {
            active_size -= 1
            swap(index, s, active_size)
              s -= 1
            continu = true
          }
          else PG = Math.max(G, 0)
        }
        else PG = G

        if (!continu) {

          PGmax_new = Math.max(PGmax_new, PG)
          PGmin_new = Math.min(PGmin_new, PG)

          if (Math.abs(PG) > 1.0e-12) {

            alpha_old = alpha(i)
            alpha(i) = Math.min(Math.max(alpha(i) - G / QD(i), 0.0), U)
            d = (alpha(i) - alpha_old) * yi;

            if (bias > 0) weight(0) += d * bias

            j = 0
            while (j < xi.length) {
              weight(xi(j)) += d
              j += 1
            }

          }

        }
      }

      var continu = false

      if (PGmax_new - PGmin_new <= eps) {
        if (active_size == N) {
          break = true
          continu = true
        }
        else {
          active_size = N
          PGmax_old = Double.PositiveInfinity
          PGmin_old = Double.NegativeInfinity
          continu = true
        }
      }

      if (!continu) {
        PGmax_old = PGmax_new
        PGmin_old = PGmin_new
        if (PGmax_old <= 0) PGmax_old = Double.PositiveInfinity
        if (PGmin_old >= 0) PGmin_old = Double.NegativeInfinity
      }
    }

    var nSV = 0

    i = 0
    while (i < N) {
      if (alpha(i) > 0) nSV += 1
      i += 1
    }

    val build = new StringBuilder

    build.append("- label = ")
    build.append(currLabel)
    build.append(": iter = ")
    build.append(iter)
    build.append(", nSV = ")
    build.append(nSV)

    System.out.println(build.toString())

    weight
  }
}

