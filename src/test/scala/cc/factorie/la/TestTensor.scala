package cc.factorie.la

import cc.factorie._
import cc.factorie.la._
import scala.util.Random
import org.junit._
import org.junit.Assert._
import cc.factorie.util.FastSorting
import languageFeature.postfixOps

class TestTensor extends cc.factorie.util.FastLogging {

  @Test def testSorting(): Unit = {
    val arr1 = Array(1, 2, 5, 3, 9, 7, 8, 4)
    val arr2 = Array(1, 2, 3, 4, 5, 6, 7, 8)
    FastSorting.quickSort(arr1, arr2)
    assert(arr2.sameElements(Array(1, 2, 4, 8, 3, 6, 7, 5)))
  }

  @Test def runTest(): Unit = {
    val dim = 20
    val ts = Seq(new DenseTensor1(dim), new SparseTensor1(dim))
    val r = new Random
    for (i <- 0 until 10) {
      val index = math.abs(r.nextInt()) % dim
      val value = r.nextDouble()
      logger.debug("index="+index+" value="+value)
      ts.foreach(_.+=(index, value))
    }
    //println(ts.head.toSeq)
    //println(ts.last.toSeq)
    for (i <- 0 until 20) assertEquals(ts.head(i), ts.last(i), 0.001)
    // assert(ts.head.toSeq == ts.last.toSeq)
  }

  @Test def testBinary(): Unit = {
    val foo = new SparseBinaryTensor1(100)
    foo += (50, 1.0)
    foo += (60, 0.0)
    foo(70) = 0.0
    foo(71) = 1.0
    val bar = new SparseBinaryTensor2(10, 10)
    bar(5, 5) = 0.0
    bar (6, 6) = 1.0
  }

  trait TensorCreator {
    def create(i: Int): Tensor
  }

  def creators: Seq[TensorCreator] = Seq(
    new TensorCreator { def create(i: Int) = new DenseTensor1(i) },
    new TensorCreator { def create(i: Int) = new SparseTensor1(i) },
    new TensorCreator { def create(i: Int) = new GrowableSparseIndexedTensor1(Iterable.fill(i)(0)) },
    new TensorCreator { def create(i: Int) = new SparseIndexedTensor2(i, 1) },
    new TensorCreator { def create(i: Int) = new DenseTensor2(1, i) },
    new TensorCreator { def create(i: Int) = new DenseLayeredTensor2(1, i, new DenseTensor1(_)) },
    new TensorCreator { def create(i: Int) = new DenseLayeredTensor2(1, i, new SparseTensor1(_)) },
    new TensorCreator { def create(i: Int) = new DenseTensor3(1, 1, i) },
    new TensorCreator { def create(i: Int) = new Dense2LayeredTensor3(1, 1, i, new DenseTensor1(_)) },
    new TensorCreator { def create(i: Int) = new Dense2LayeredTensor3(1, 1, i, new SparseTensor1(_)) }
  // TODO: add all other tensor types above here
  )

  @Test def testZero() {
    def fill(t: TensorCreator) = {
      val tensor = t.create(100)
      tensor(10) = 20
      tensor(1) = 2
      tensor(2) = -5
      tensor
    }
    testPairwise(fill) { (t1, t2) =>
      t1 += t2
      t1.zero()
      assert(t1.forall(0.0.==), "Failed zero check at %s, %s" format (t1.getClass.getName, t2.getClass))
      val t3 = t1.blankCopy
      t3 += t1
      t3 += t2
//      assert((0 until t2.size).forall(i => t2(i) == t3(i)), "Failed += after zero() at %s, %s" format (t1.getClass, t2.getClass))
    }
  }

  def testPairwise(fill: TensorCreator => Tensor)(test: (Tensor, Tensor) => Unit): Unit = {
    for (c1 <- creators; c2 <- creators) {
      val t1 = fill(c1)
      val t2 = fill(c2)
      if (!(t1.isInstanceOf[Tensor2] && t2.isInstanceOf[Tensor3]) &&
          !(t1.isInstanceOf[Tensor3] && t2.isInstanceOf[Tensor2]))
        test(t1, t2)
    }
  }
  
  @Test def testDot() {
    val dim1 = 10; val dim2 = 1000
    val random = new scala.util.Random(0)
    val dense = new DenseTensor2(dim1, dim2)
    val sparse = new DenseLayeredTensor2(dim1, dim2, new SparseIndexedTensor1(_))
    for (i <- 0 until 1000) dense(random.nextInt(dim1*dim2)) = random.nextDouble()
    sparse += dense
    val features = new SparseBinaryTensor1(dim2)
    for (i <- 0 until 20) features.+=(random.nextInt(dim2))
    val statistics = new SingletonLayeredTensor2(dim1, dim2, 3, 0.3, features)
    assertEquals(dense dot statistics, sparse dot statistics, 0.0001)
  }

  @Test def testPlusEqual() {

    def fill(t: TensorCreator) = {
      val tensor = t.create(100)
      tensor(10) = 20
      tensor(1) = 2
      tensor(2) = -5
      tensor
    }

    testPairwise(fill) { (t1, t2) =>

      logger.debug("testing " + t1.getClass.getName + " and " + t2.getClass.getName)
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
