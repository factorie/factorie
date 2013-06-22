package cc.factorie.maths

import org.junit.Test

class TestGamma {
  @Test def runTest(): Unit = {
    def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
    val xs = Seq[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    for (x <- xs) {
//      println(f"gamma($x%f) old: ${factorial(x.toInt - 1).toDouble}%f new: ${math.exp(logGamma(x))}%f")
      assert(math.abs(factorial(x.toInt - 1).toDouble) - math.exp(logGamma(x)) < .01)
    }
  }
}