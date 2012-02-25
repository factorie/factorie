package cc.factorie.bp.optimized

import cc.factorie._
import collection.mutable.ListBuffer

/**
 * Author: martin
 * Date: 2/11/12
 */

trait BeamSearch {

  var debug = false

  // the contract here is: null out array elements you don't like
  def threshold(domainScores: Array[(Int, Double)]): Unit

  def searchAndSetToMax[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
          localTemplate: TemplateWithDotStatistics2[LV, OV],
          transTemplate: TemplateWithDotStatistics2[LV, LV],
          vs: Seq[LV]
        ): Unit = {
    val resultAssignment: Seq[Int] = search(localTemplate, transTemplate, vs)
    var i = 0
    while (i < vs.size) {
      vs(i).set(resultAssignment(i))(null)
      i += 1
    }
  }

  def search[OV <: DiscreteVectorVar, LV <: LabelVariable[_]](
          localTemplate: TemplateWithDotStatistics2[LV, OV],
          transTemplate: TemplateWithDotStatistics2[LV, LV],
          vs: Seq[LV]
        ): Seq[Int] = {

    // get localScores
    val localScores: Array[Array[Double]] = {
      val arrays = Array.fill[Array[Double]](vs.size)(Array.ofDim[Double](vs.head.domain.size))
      for ((v,vi) <- vs.zipWithIndex) {
        val localFactor = localTemplate.factors(v).head
        for ((_,di) <- v.settings.zipWithIndex)
          arrays(vi)(di) = localFactor.score
      }
      arrays
    }
    
    if (vs.size == 0) return Seq.empty[Int]
    else if (vs.size == 1) return Seq(localScores.head.zipWithIndex.maxBy(_._1)._2)

    def transScores(i: Int, j: Int): Double = transTemplate.weight(i,j)


    // the int is the intValue of the previous variable, the double is the alpha
    val backPtrs: Array[Array[(Int, Double)]] = Array.fill(vs.size)(null)
    
    //do the first variable first
    var vi = 0
    val firstCol = Array((0 until localScores(vi).size).zip(localScores(vi)): _*)
    threshold(firstCol) // todo: consider not doing this
    backPtrs(0) = firstCol
    vi += 1

    var currentColumn: Array[(Int, Double)] = Array.empty[(Int, Double)]
    while (vi < vs.size) {

      currentColumn = Array.ofDim[(Int, Double)](localScores(vi).size)

      // for each intValue of the current variable's domain determine the best way to get there and save it
      var i = 0
      while(i < localScores(vi).size) {
        var bestVal = -1
        var bestAlpha = Double.NegativeInfinity
        var di = 0
        while (di < localScores(vi).size) {
          val prev = backPtrs(vi-1)(di)
          if (prev ne null) {
            val alpha = prev._2 + transScores(di,i) + localScores(vi)(i)
            if (alpha > bestAlpha) {
              bestAlpha = alpha
              bestVal = di
            }
          }
          di += 1
        }
        currentColumn(i) = (bestVal, bestAlpha)
        i += 1
      }

      if (debug)
        printColumn(currentColumn)

      // threshold alphas and keep ptrs
      threshold(currentColumn)
      backPtrs(vi) = currentColumn

      if (debug)
        printColumn(currentColumn)

      vi += 1
    }

    decodeBackPtrs(backPtrs)
  }

  def decodeBackPtrs(backPtrs: Array[Array[(Int, Double)]]): Seq[Int] = {
    // decoding from the end of the chain backwards
    val dSize = backPtrs.head.size

    // first find the index of the max-scoring assignment for the last variable
    var bestCurrIdx = -1
    var bestScore = Double.NegativeInfinity
    var di = 0
    while (di < dSize) {
      if ((backPtrs.last(di) ne null) && backPtrs.last(di)._2 > bestScore) {
        bestCurrIdx = di
        bestScore = backPtrs.last(di)._2
      }
      di += 1
    }

    // prepend it to the MAP list
    val intValSeq = new ListBuffer[Int]
    intValSeq.prepend(bestCurrIdx)

    // continue prepending back to the beginning (from the second to last variable)
    var vi = backPtrs.size - 1
    while(vi > 0) {
      intValSeq.prepend(backPtrs(vi)(intValSeq.head)._1)
      vi -= 1
    }

    if (debug)
      printBackPtrs(backPtrs)

    intValSeq
  }
  
  def printBackPtrs(backPtrs: Array[Array[(Int, Double)]]): Unit = {
    for (j <- 0 until backPtrs.head.size) {
      for (i <- 0 until backPtrs.size)
        print(if (backPtrs(i)(j) eq null) "null\t\t\t" else backPtrs(i)(j) + "\t\t")
      println()
    }
  }

  def printColumn(col: Array[(Int, Double)]): Unit = {
    println(
      (for (x <- col) yield {
        if (x eq null) "null"
        else x
      }).toSeq.mkString(" - ")
    )
    println("-------------------------------------------")
  }
}

trait FullBeam {
  this: BeamSearch =>
  def threshold(domainScores: Array[(Int, Double)]): Unit = ()
}

trait FixedBeamWidth {
  this: BeamSearch =>

  var width: Int

  def threshold(domainScores: Array[(Int, Double)]): Unit = {
    val sorted = domainScores.zipWithIndex.toArray.sortBy(_._1._2)
    val max = domainScores.size - width
    var i = 0
    while (i < max) {
      domainScores(sorted(i)._2) = null
      i += 1
    }
  }
}

trait CDFBeamWidth {
  this: BeamSearch =>

  var ratio: Double

  def threshold(domainScores: Array[(Int, Double)]): Unit = {
    val total = domainScores.toArray.map(_._2).sum
    val sorted = domainScores.zipWithIndex.toArray.sortBy(_._1._2)

    // find the width
    var target = ratio * total
    var accumulator = 0.0
    var width = 0
    while (accumulator < target) {
      width += 1
      accumulator += sorted(sorted.size - width)._1._2
    }

    // null out values outside of the width
    val max = domainScores.size - width
    var i = 0
    while (i < max) {
      domainScores(sorted(i)._2) = null
      i += 1
    }
  }
}


/*
object TestBeam {
  object D20 extends CategoricalDomain[Int] { 1 to 50 foreach { this += _ } }
  class Var20 extends DiscreteVariable { def domain = D20 }
  
  def main(args: Array[String]): Unit = {
    val localWeights = Seq((1 to 500).map(_.toDouble): _*).toArray
    val transWeights = Seq((1 to 250000).reverse.map(_.toDouble): _*).toArray
    val vars = 1 to 50 map { i => i; new Var20 }

    val searcher =
      new BeamSearch
        with CDFBeamWidth
        with StaticScoring {
          val ratio = .5
          val staticLocalScores = localWeights
          val staticTransScores = transWeights
        }

    val mapAss = searcher.search(vars)
    //val mapAss = (new BeamSearch with FixedBeamWidth { val width = 5 }).search(vars, localWeights, transWeights)
    for (i <- 1 to 100) {
      val start = System.currentTimeMillis()
      searcher.search(vars)
      println(System.currentTimeMillis() - start)
    }
    println(mapAss)
  }
}
*/
