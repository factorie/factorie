package cc.factorie.tutorial

import collection.mutable.{Buffer, ArrayBuffer}
import cc.factorie._
import cc.factorie.variable.{HammingTemplate, LabeledCategoricalVariable, DoubleVariable, BooleanDomain}
import cc.factorie.model.{TupleTemplateWithStatistics2, CombinedModel}
import cc.factorie.infer.{VariableSettingsSampler, SamplingMaximizer}

//import bp.{InferencerBPWorker, InferencerBP, MaxProductLattice, LatticeBP}

/**
 * Example of synthetic Ising Grid models
 * @author sameer
 */

object Grid {

  class Observed(val score: Double) extends DoubleVariable(score)

  class Pixel(val x: Int, val y: Int, val image: Seq[Seq[Pixel]], val score: Double, truth: Boolean) extends LabeledCategoricalVariable(truth) {

    def domain = BooleanDomain

    val observed = new Observed(score)

    def setUsingObserved() = this.setCategory(observed.doubleValue > 0.0)(null)
  }

  def createDonut(sigma: Double, N: Int = 25, outerRadius: Double = 7.5, innerRadius: Double = 7.5, c: (Double, Double) = Pair(10.0, 10.0), random: scala.util.Random = new scala.util.Random(0)): Seq[Seq[Pixel]] = {

    def checkInCircle(x: Int, y: Int, c: (Double, Double), outer: Double, inner: Double) = {
      val d = math.pow(x - c._1, 2) + math.pow(y - c._2, 2)
      (d < outer * outer) && (d  > inner * inner)
    }

    val image: Buffer[Seq[Pixel]] = new ArrayBuffer
    for (i <- 0 until N) {
      val row = new ArrayBuffer[Pixel]
      for (j <- 0 until N) {
        val truth = checkInCircle(i, j, c, outerRadius, innerRadius)
        val score = (if (truth) 1.0 else -1.0) + random.nextGaussian() * sigma
        row += new Pixel(i, j, image, score, truth)
      }
      image += row
    }
    image
  }

  object LocalTemplate extends TupleTemplateWithStatistics2[Pixel, Observed] /*with Statistics[Double]*/ {
    val alpha = 1.0
    def score(v1:Pixel#Value, v2:Observed#Value) = v2 * (if (v1.category) 1.0 else -1.0)
    //def score(s: Double) = s
    //def statistics(values: LocalTemplate.ValuesType) = Stat(values._2 * (if (values._1.category) 1.0 else -1.0))
    //def statistics(v1:Pixel#Value, v2:Observed#Value) = Statistics(v2 * (if (v1.category) 1.0 else -1.0))
    def unroll1(p: Pixel) = Factor(p, p.observed)
    def unroll2(v: Observed) = Nil
  }

  object PairwiseTemplate extends TupleTemplateWithStatistics2[Pixel, Pixel] /*with Statistics1[Double]*/ {
    //def score(s: Statistics) = s._1

    //def statistics(values: PairwiseTemplate.ValuesType) = Stat(if (values._1.category == values._2.category) 1.0 else -1.0)
    def score(v1:Pixel#Value, v2:Pixel#Value) = if (v1.category == v2.category) 1.0 else -1.0

    // (v1,v2), where v2.x = vx.i+1 or v2.y = v1.y +1
    def unroll1(v: Pixel) = {
      val img = v.image
      val factors = new ArrayBuffer[FactorType]
      if (v.x < img.length - 1) factors += Factor(v, img(v.x + 1)(v.y))
      if (v.y < img.length - 1) factors += Factor(v, img(v.x)(v.y + 1))
      if (v.x > 0) factors += Factor(img(v.x - 1)(v.y), v)
      if (v.y > 0) factors += Factor(img(v.x)(v.y - 1), v)
      factors
    }

    // symmetric
    def unroll2(v2: Pixel) = Nil
  }

  def printImage(img: Seq[Seq[Pixel]]) =
    for (i: Int <- 0 until img.length) {
      for (j: Int <- 0 until img(i).length) {
        if (img(i)(j).categoryValue) print("W") else print(" ")
      }
      println()
    }


  def main(args: Array[String]) {
    val image = createDonut(1.0, 50, 20, 7.5, Pair(25.0, 25.0))
    val pixels = image.flatMap(_.toSeq).toSeq
    val gridModel = new CombinedModel(LocalTemplate, PairwiseTemplate)
    val objective = new HammingTemplate[Pixel]
    pixels.foreach(_.setUsingObserved())
    implicit val random = new scala.util.Random(0)
    pixels.foreach(_.setRandomly)
    //*
    val sampler = new SamplingMaximizer[Pixel](new VariableSettingsSampler(gridModel))
    sampler.maximize(pixels, iterations=10, rounds=10)
    println("Accuracy: %f".format(objective.accuracy(pixels)))
  }
}
