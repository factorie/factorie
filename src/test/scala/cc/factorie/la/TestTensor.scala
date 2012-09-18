package cc.factorie.la

import cc.factorie._
import cc.factorie.la._
import scala.util.Random
import org.junit._

class TestTensor {
  @Test def runTest {
    val dim = 20
    val ts = Seq(new DenseTensor1(dim), new SparseTensor1(dim))
    val r = new Random
    for (i <- 0 until 10) {
      val index = math.abs(r.nextInt) % dim
      val value = r.nextDouble
      println("index="+index+" value="+value)
      ts.foreach(_.+=(index, value))
    }
    //println(ts.head.toSeq)
    //println(ts.last.toSeq)
    assert(ts.head.toSeq == ts.last.toSeq)
  }
}
