package cc.factorie.example

import cc.factorie._
import cc.factorie.generative._

object GaussianDemo {
  def main(args: Array[String]): Unit = {
    implicit val model = GenerativeModel()
    val mean = new DoubleVariable(10)
    val variance = new DoubleVariable(1.0)

    val data = for (i <- 1 to 1000) yield new DoubleVariable :~ Gaussian(mean, variance)
    data.take(50).foreach(println(_))

    val origMean = mean.value
    val origVariance = variance.value
    println("Original mean=" + origMean)
    println("Original variance=" + origVariance)

    Maximize(Seq(mean), model)
    Maximize(Seq(variance), model)
    // Or alternatively:
    //MaximizeGaussianMean(mean, model)
    //MaximizeGaussianVariance(variance, model)

    println("Estimated mean=" + mean.value)
    println("Estimated variance=" + variance.value)
    assert(math.abs((mean.value / origMean) - 1.0) < .05, "Mean estimate failed")
    assert(math.abs((variance.value / origVariance) - 1.0) < .05, "Variance estimate failed")
  }
}