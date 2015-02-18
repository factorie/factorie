package cc.factorie.epistemodb

import cc.factorie.util
import org.junit.Assert._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import com.github.fakemongo.Fongo
import com.mongodb.{DBCollection, BasicDBObject, DB}
import cc.factorie.la.SparseIndexedTensor2
import scala.util.Random


/**
 * Created by beroth on 1/30/15.
 */
class TestCoocMatrix extends JUnitSuite  with util.FastLogging {

  val eps = 1e-4

  @Test def getSetCellsTest() {
    val m = new CoocMatrix(0,0)
    m.set(0,0,1.0)
    m.set(4,2,3.0)
    m.set(1,3,1.0)
    m.set(4,2,2.0)
    assertEquals(m.numRows(),5)
    assertEquals(m.numCols(),4)
    assertEquals(m.get(1, 3),1.0, eps)
    assertEquals(m.get(4, 2),2.0, eps)
    assertEquals(m.get(2, 2),0.0, eps)
    assertEquals(m.get(5, 5),0.0, eps)
    assertEquals(m.nnz(), 3)
    m.set(1,3,0)
    assertEquals(m.nnz(), 2)
    m.set(1,3,0)
    assertEquals(m.nnz(), 2)
    m.set(5,4,0)
    assertEquals(m.nnz(), 2)
    assertEquals(m.numRows(),6)
    assertEquals(m.numCols(),5)
  }


  @Test def copyTest() {
    val m = new CoocMatrix(0,0)
    m.set(0,0,1.0)
    m.set(1,3,1.0)
    m.set(4,2,2.0)
    val m2 = m.copy()
    assertTrue(m.hasSameContent(m2))
    m2.set(1,3,0)
    assertFalse(m.hasSameContent(m2))
    assertFalse(m.getNnzCells().toSet == m2.getNnzCells().toSet)
  }

  @Test def pruneMatrixTest() {
    val m = new CoocMatrix(0,0)
    m.set(1,1,1.0)
    m.set(2,2,1.0)
    m.set(2,3,1.0)
    m.set(3,3,1.0)

    val (m0, rowMap0, colMap0) = m.prune(0,0)
    // pruned matrix only contains biggest component, i.e. rows 2 and 3, and columns 2 and 3
    assertEquals(m0.numRows(), 2)
    assertEquals(m0.numCols(), 2)

    assertFalse(rowMap0.contains(0))
    assertFalse(colMap0.contains(0))
    assertFalse(rowMap0.contains(1))
    assertFalse(colMap0.contains(1))
    assertTrue(rowMap0.contains(2))
    assertTrue(colMap0.contains(2))
    assertTrue(rowMap0.contains(3))
    assertTrue(colMap0.contains(3))

    // check that the columns are mapped with the order preserved
    assertEquals(colMap0(2), 0)
    assertEquals(colMap0(3), 1)
    assertEquals(rowMap0(2), 0)
    assertEquals(rowMap0(3), 1)

    val (m1, rowMap1, colMap1) = m.prune(0,1)
    assertEquals(2, m1.numRows())
    assertEquals(1, m1.numCols())
    assertFalse(colMap1.contains(0))
    assertFalse(colMap1.contains(1))
    assertFalse(colMap1.contains(2))
    assertEquals(0, colMap1(3))
    assertFalse(rowMap1.contains(0))
    assertFalse(rowMap1.contains(1))
    assertEquals(0, rowMap1(2))
    assertEquals(1, rowMap1(3))
  }

  @Test def equalsTest() {
    val m1 = new CoocMatrix(0,0)
    m1.set(0,0,1.0)
    m1.set(0,1,1.0)
    m1.set(0,2,1.0)
    m1.set(0,3,1.0)
    m1.set(4,2,3.0)
    m1.set(1,3,1.0)
    m1.set(4,2,2.0)

    val m2 = new CoocMatrix(0,0)
    m2.set(4,2,2.0)
    m2.set(1,3,1.0)
    m2.set(0,3,1.0)
    m2.set(0,2,1.0)
    m2.set(0,1,1.0)
    m2.set(0,0,1.0)

    val m3 = new CoocMatrix(0,0)
    m3.set(4,2,2.0)
    m3.set(1,3,1.0)
    m3.set(0,0,1.0)

    assertTrue(m1.hasSameContent(m2))
    assertTrue(m2.hasSameContent(m1))
    assertTrue(m3.hasSameContent(m3))
    assertFalse(m1.hasSameContent(m3))
    assertFalse(m3.hasSameContent(m1))
  }

  @Test def writeReadMongoTest() {
    // Fake in-memory mongo server.
    val fongo = new Fongo("myserver");
    val db : DB = fongo.getDB("mydb");

    val m1 = new CoocMatrix(0,0)
    m1.set(0,0,1.0)
    m1.set(0,1,1.0)
    m1.set(0,2,1.0)
    m1.set(0,3,1.0)
    m1.set(4,2,3.0)
    m1.set(1,3,1.0)
    m1.set(4,2,2.0)

    m1.writeToMongo(db)

    val m2 = new CoocMatrix(0,0)
    m2.populateFromMongo(db)
    assertTrue(m1.hasSameContent(m2))
  }

  /*
  @Test def writeReadMongoCellBasedTest() {
    // Fake in-memory mongo server.
    val fongo = new Fongo("myserver");
    val db : DB = fongo.getDB("mydb");

    val m1 = new CoocMatrix(0,0)
    m1.set(0,0,1.0)
    m1.set(0,1,1.0)
    m1.set(0,2,1.0)
    m1.set(0,3,1.0)
    m1.set(4,2,3.0)
    m1.set(1,3,1.0)
    m1.set(4,2,2.0)

    m1.writeToMongoCellBased(db)

    val m2 = CoocMatrix.fromMongoCellBased(db)
    assertTrue(m1.hasSameContent(m2))
  }
  */

  @Test def testSplitTest() {
    //0101
    //1101
    //0010
    //1101
    val m = new CoocMatrix(0,0)
    m.set(0,1,1.0)
    m.set(0,3,1.0)
    m.set(1,0,1.0)
    m.set(1,1,1.0)
    m.set(1,3,1.0)
    m.set(2,2,1.0)
    m.set(3,0,1.0)
    m.set(3,1,1.0)
    m.set(3,3,1.0)
    // Just use rows and cols 1,2,3 for testing purposes
    val testRows = Set(1,2,3)
    val testCols = Set(1,2,3)

    // Make sure that test passes for different random initialiaztions
    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val (mtrain, mdev, mtest) = m.randomTestSplit(2,3,Some(testRows), Some(testCols), random)
      // Cell 2,2 is not elegible, so there are only 2 cells left for test set
      assertFalse(mtest.getNnzCells().toSet.contains((2,2)))
      assertFalse(mdev.getNnzCells().toSet.contains((2,2)))
      assertEquals(2,mdev.nnz())
      assertEquals(2,mtest.nnz())
      assertEquals(5,mtrain.nnz())
      // the 3 matrices are a partitoning of m:
      // 1. their size is 2+2+5 = 9
      // 2. they contain all elements
      assertEquals(m.getNnzCells().toSet, mtrain.getNnzCells().toSet ++ mtest.getNnzCells().toSet ++ mdev.getNnzCells().toSet)
    }
  }

  @Test def testSplitRandomizedTest() {
    val numRows = 1000
    val numCols = 100
    val nnz = 10000
    val numDevNNZ = 100
    val numTestNNZ = 150

    val numTopics = 1
    val noise1 = 0.1
    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1)
      val (mTrain,mDev,mTest) = m.randomTestSplit(numDevNNZ, numTestNNZ, None, Some(Set(0,1,2,3,4,5,6,7,8,9)), random)
      assertEquals(numDevNNZ, mDev.nnz())
      assertEquals(numTestNNZ, mTest.nnz())
      assertEquals(m.nnz(), mTrain.nnz() + mDev.nnz() + mTest.nnz())
    }
  }

  @Test def randomMatrixTest() {
    val numRows = 1000
    val numCols = 1000
    val nnz = 10000
    val numTopics = 10
    val noise1 = 0.1
    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val m = CoocMatrix.randomOneZeroMatrix(numRows, numCols, nnz, random, numTopics, noise1)
      // non-zeros roughly as specified
      assertTrue(m.nnz() <= nnz)
      assertTrue(m.nnz() > 0.9 * nnz)
      val noiseCells = m.getNnzCells().filter(cell => (cell._1 % numTopics != cell._2 % numTopics))
      // Ratio of noise roughly as specified
      assertEquals(noiseCells.size / m.nnz().toDouble, noise1, 0.05)
    }
  }

  @Test def readFromTensor2Test() {
    val t2 = new SparseIndexedTensor2(10, 10)
    t2.+=(0, 2, 3.0)
    t2.+=(0,0, 5.0)
    t2.+=(3, 0, 7.0)
    t2.+=(5, 9, 10.0)
    val m = CoocMatrix.fromTensor2(t2)
    assert(m.get(0,2) == 3.0)
    assert(m.get(0,0) == 5.0)
    assert(m.get(3,0) == 7.0)
    assert(m.get(5,9) == 10.0)
  }
}