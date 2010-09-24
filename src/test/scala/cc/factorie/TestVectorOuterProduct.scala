package cc.factorie

import cc.factorie.la._
import junit.framework._
import Assert._

class TestVectorOuterProduct extends TestCase {

  def test_flatOuter():Unit = {

    val sva = new SparseBinaryVector(4, Array(1,3))
    val svb = new SparseBinaryVector(4, Array(2,3))

    val sv1 = new SparseBinaryVector(3000, 1 to 10 by 2 toArray)
    val sv2 = new SparseBinaryVector(3000, 1 to 10 by 2 toArray)
    val sv3 = new SparseBinaryVector(3000, 1 to 10 by 3 toArray)
    val sv4 = new SparseBinaryVector(1000, Array(42))

    val siv1 = new SingletonBinaryVector(1000, 42)
    val siv2 = new SingletonBinaryVector(1000, 42)
    val siv3 = new SingletonBinaryVector(1000, 43)

    def equalDomains(v1:Vector, v2:Vector): Boolean = v1.activeDomain.zip(v2.activeDomain).forall {case (a,b) => a==b}


    // test equality of flatOuter implementations run on pairs of sparse/singleton binary vectors
    val tests = List(
      (sv1, sv1),
      (sv1, sv2),
      (sv2, sv3),
      (sv4, siv1),
      (siv1, siv2),
      (siv1, siv3)
    )
    for (test <- tests) {
      test match {
        case (v1, v2) => {
          //println("v1: " + sv1.activeDomain)
          //println("v2: " + sv2.activeDomain)
          val r1 = v1 flatOuter v2
          val r2 = OriginalFlatOuter.flatOuter(v1, v2)
          //println("new v.flatOuter.v " + r1.activeDomain)
          //println("old flatOuter(v,v)" + r2.activeDomain)
          assertTrue("domains are not equal", equalDomains(r1,r2))
          assertTrue("result vector domain size does not equal product of input vectors domain sizes", r1.length == v1.length * v2.length)
        }
      }
    }
  }
}

// This code was moved from Template.scala - the implementation of flatOuter has changed, this is used as
//   a reference implementation to catch any errors in the new version
object OriginalFlatOuter {
  def flatOuter(vector1: Vector, vector2: Vector) : Vector = vector1 match {
    case v1: SingletonBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SingletonBinaryVector(v1.size * v2.size, v1.singleIndex * v2.size + v2.singleIndex)
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v2.activeDomain.size);
                                 var i = 0;
                                 for (i2 <- v2.activeDomain) {
                                   arr(i) = v1.singleIndex * v2.size + i2;
                                   i += 1;
                                 };
                                 arr
                               })
    }
    case v1: SparseBinaryVector => vector2 match {
      case v2: SingletonBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v1.activeDomain.size);
                                 var i = 0;
                                 for (i1 <- v1.activeDomain) {
                                   arr(i) = i1 * v2.size + v2.singleIndex;
                                   i += 1;
                                 };
                                 arr
                               })
      case v2: SparseBinaryVector =>
        new SparseBinaryVector(v1.size * v2.size,
                               {
                                 val arr = new Array[Int](v1.activeDomain.size * v2.activeDomain.size);
                                 var i = 0;
                                 for (i1 <- v1.activeDomain; i2 <- v2.activeDomain) {
                                   arr(i) = i1 * v2.size + i2;
                                   i += 1;
                                 };
                                 arr
                               })
    }
  }
}

object TestVectorOuterProduct extends TestSuite {
  addTestSuite(classOf[TestVectorOuterProduct])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
