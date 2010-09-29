package cc.factorie

import cc.factorie.la._

import org.junit.Assert._
import org.junit.Before
import org.junit.Test

// Only use junit, not scalatest. -akm

/*
import org.scalatest.junit.JUnitSuite

import cc.factorie.OuterProduct.{computeMatrix => outerProductArrayJava}

class TestVectorOuterProduct extends JUnitSuite {

  // def equalDomains(v1:Vector, v2:Vector): Boolean = v1.activeDomain.zip(v2.activeDomain).forall {case (a,b) => a==b}
  def equalDomains(v1:Vector, v2:Vector): Boolean = equalDomains(v1.activeDomain.toArray, v1.activeDomain.toArray)
  def equalDomains(a1:Array[Int], a2:Array[Int]): Boolean = a1.zip(a2).forall {case (a,b) => a==b}

  @Test
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

  import OuterProductMath.outerProductArray

  @Test
  def outerProductArrayTests = {    
    for {i1 <- List(0, 1)
         i2 <- List(0, 1)
         i3 <- List(0, 1)} 
    {
      // sparse sparse sparse
      val op1 = outerProductArray(Array(i1), 1,
                                  Array(i2), 1, 4,
                                  Array(i3), 1, 4)

      // sparse sparse singleton
      val op2 = outerProductArray(Array(i1), 1,
                                  Array(i2), 1, 4,
                                  i3, 4)
      // sparse singleton singleton
      val op3 = outerProductArray(Array(i1), 1,
                                  i2, 4,
                                  i3, 4)

      val str = """
      | i1 = %s
      | i2 = %s
      | i3 = %s
      | op1  = %s
      | op2  = %s
      | op3  = %s
      """.stripMargin.format(i1, i2, i3,
                             op1.mkString("[", ", ", "]"), 
                             op2.mkString("[", ", ", "]"), 
                             op3.mkString("[", ", ", "]"))

      // println(str)
      assertTrue("outer prod are not equal", equalDomains(op1, op2))
      assertTrue("outer prod are not equal", equalDomains(op1, op3))

    }
  }

  def combos3():Seq[Tuple3[Int,Int,Int]] = {
    var list = List[Tuple3[Int,Int,Int]]()
    val b = List(0,1);
    for {b1 <- b
         b2 <- b
         b3 <- b} list = list ::: List((b1, b2, b3))
    list
  }

  @Test
  def test_flatOuter3():Unit = {
    val vectors = 
      List(List(new SparseBinaryVector(3, Array(0,1)),
                new SparseBinaryVector(3, Array(0,2))),
           List(new SingletonBinaryVector(2, 0),
                new SingletonBinaryVector(2, 1)
              ))

    // test that v1.flatOuter(v2).flatOuter(v3) is the same as
    //           v1.flatOuter(v2, v3) for all v1/2/3 types:
    for (c  <- combos3) c match {
      case (i1:Int, i2:Int, i3:Int) => {
        for {j1 <- 0 to vectors(i1).size-1
             j2 <- 0 to vectors(i2).size-1
             j3 <- 0 to vectors(i3).size-1} {
               val v1 = vectors(i1)(j1)
               val v2 = vectors(i2)(j2)
               val v3 = vectors(i3)(j3)

               val ov1 = OriginalFlatOuter.flatOuter(v1, v2)
               val r0 = OriginalFlatOuter.flatOuter(ov1, v3)
               val r1 = v1.flatOuter(v2).flatOuter(v3)
               val r2 = v1.flatOuter(v2, v3)

               val opj12 = outerProductArrayJava(v1.activeDomain.toArray, v2.activeDomain.toArray, v2.size)
               val r3 = outerProductArrayJava(opj12, v3.activeDomain.toArray, v3.size)

               assertTrue("orig/pairwise domains are not equal for " + ((i1, j1), (i2, j2), (i3, j3)) + "\n" + 
                          List(v1, v2, v3),
                          equalDomains(r0,r1))
               assertTrue("orig/3-way domains are not equal for " + ((i1, j1), (i2, j2), (i3, j3)) + "\n" + 
                          List(v1, v2, v3),
                          equalDomains(r0,r2))
               assertTrue("orig/java-pairwise domains are not equal for " + ((i1, j1), (i2, j2), (i3, j3)) + "\n" + 
                          List(v1, v2, v3),
                          equalDomains(r0.activeDomain.toArray, r3))
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

// object TestVectorOuterProduct extends TestSuite {
//   addTestSuite(classOf[TestVectorOuterProduct])
//   def main(args: Array[String]) {
//     junit.textui.TestRunner.run(this)
//   }
// }
*/
