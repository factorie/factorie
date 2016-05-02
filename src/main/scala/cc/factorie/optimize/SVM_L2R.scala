/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.optimize

import cc.factorie.la.{DenseTensor1, Tensor1}


/**
 * An implementation of the liblinear algorithm. Not really a GradientOptimizer.
 * @param lossType 1 if l1 hinge loss, 2 if hinge squared
 * @param cost The C parameter of the SVM algorithm
 * @param eps The tolerance of the optimizer
 * @param bias TODO: no idea what this is - Alex
 * @param maxIterations Maximum number of iterations
 */
class LinearL2SVM(lossType: Int = 0, cost: Double = 0.1, eps: Double = 1e-5, bias: Double = -1, maxIterations: Int = 1000) {

  @inline private final def GETI(y: Array[Byte], i: Int) = y(i) + 1
  @inline private final def swap(a: Array[Int], i0: Int, i1: Int) { val t = a(i0); a(i0) = a(i1); a(i1) = t }

  def train(xTensors: Seq[Tensor1], ys: Array[Int], currLabel: Int): DenseTensor1 = {
    val random = new scala.util.Random(0)

    val N = ys.size
    val D = xTensors.head.length

    val xs = xTensors.map(t => t.activeDomain.asSeq)

    val QD = Array.ofDim[Double](N)
    val alpha = Array.ofDim[Double](N)
    val weight = new DenseTensor1(D, 0.0)
    var U, G, d, alpha_old = 0.0

    val index = Array.ofDim[Int](N)
    val aY = Array.ofDim[Byte](N)

    var active_size = N
    var i, j, s, iter = 0
    var yi: Byte = 0
    var xi: Seq[Int] = Array.ofDim[Int](0)
    var vi: Seq[Double] = Array.ofDim[Double](0)

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
      aY   (i) = (if (ys(i) == currLabel) 1 else -1).toByte
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

          PGmax_new = math.max(PGmax_new, PG)
          PGmin_new = math.min(PGmin_new, PG)

          if (Math.abs(PG) > 1.0e-12) {

            alpha_old = alpha(i)
            alpha(i) = math.min(math.max(alpha(i) - G / QD(i), 0.0), U)
            d = (alpha(i) - alpha_old) * yi

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

