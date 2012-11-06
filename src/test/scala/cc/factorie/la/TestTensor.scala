package cc.factorie.la

import cc.factorie._
import cc.factorie.la._
import scala.util.Random
import org.junit._
import org.junit.Assert._

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

  trait TensorCreator {
    def create(i: Int): Tensor
  }


  @Test def testPlusEqual() {
    val creators : Seq[TensorCreator] = Seq(
      new TensorCreator { def create(i: Int) = new DenseTensor1(i) },
      new TensorCreator { def create(i: Int) = new SparseTensor1(i) },
      new TensorCreator { def create(i: Int) = new DenseTensor2(1, i) },
      new TensorCreator { def create(i: Int) = new DenseLayeredTensor2(1, i, new DenseTensor1(_)) },
      new TensorCreator { def create(i: Int) = new DenseLayeredTensor2(1, i, new SparseTensor1(_)) },
      new TensorCreator { def create(i: Int) = new DenseTensor3(1, 1, i) },
      new TensorCreator { def create(i: Int) = new Dense2LayeredTensor3(1, 1, i, new DenseTensor1(_)) },
      new TensorCreator { def create(i: Int) = new Dense2LayeredTensor3(1, 1, i, new SparseTensor1(_)) }
    // TODO: add all other tensor types above here
    )
    
    def fill(t: TensorCreator) = {
      val tensor = t.create(100)
      tensor(10) = 20
      tensor(1) = 2
      tensor(2) = -5
      tensor
    }

    for (c1 <- creators; c2 <- creators) {
      val t1 = fill(c1)
      val t2 = fill(c2)
      if ((!(t1.isInstanceOf[Tensor2] && t2.isInstanceOf[Tensor3]) &&
           !(t1.isInstanceOf[Tensor3] && t2.isInstanceOf[Tensor2]))) {

        println("testing " + t1.getClass.getName + " and " + t2.getClass.getName)
        assertEquals(20.0*20 + 2*2 + 5*5, t1 dot t2, 0.001)
        t1 += (t2, 0.1)
        assertEquals(t1(10), 22, 0.01)

        t1 *= 0.5
        assertEquals(11, t1(10), 0.001)

        try {
          assertEquals(22, (t1*2)(10), 0.001)
        } catch {
          case e: Error => assert(e.getMessage.contains("Method copy not defined"))
        }
      }
    }
  }
}
