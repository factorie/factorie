package cc.factorie.epistemodb

import org.scalatest.junit.JUnitSuite
import cc.factorie.util
import org.junit.Test
import org.junit.Assert._
import com.github.fakemongo.Fongo
import com.mongodb.DB
import scala.util.Random

/**
 * Created by beroth on 2/6/15.
 */
class TestEntityRelationKBMatrix extends JUnitSuite  with util.FastLogging  {

  val eps = 1e-4

  @Test def getSetCellsTest() {
    val m = new EntityRelationKBMatrix()
    m.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 5.0)
    m.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 10.0)
    m.set(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse", 1.0)
    m.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 2.0)
    assertEquals(m.numRows(),2)
    assertEquals(m.numCols(),3)
    assertEquals(10.0, m.get(EntityPair("Barack Obama", "Michelle Obama"), "is married to"), eps)
    assertEquals(1.0, m.get(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse"), eps)
    assertEquals(1.0, m.get(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse"), eps)
    assertEquals(2.0, m.get(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife"), eps)
    assertEquals(0.0, m.get(EntityPair("Nicola Sarcozy", "Carla Bruni"), "per:spouse"), eps)
    assertEquals(0.0, m.get(EntityPair("Barack Obama", "Michelle Obama"), "and his wife"), eps)
  }

  @Test def equalsTest() {
    val m1 = new EntityRelationKBMatrix()
    m1.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 5.0)
    m1.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 10.0)
    m1.set(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse", 1.0)
    m1.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m1.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 2.0)

    // same as m1, but constructed in different order
    val m2 = new EntityRelationKBMatrix()
    m2.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 2.0)
    m2.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m2.set(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse", 1.0)
    m2.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 10.0)

    // similar to m2, but one different cell value
    val m3 = new EntityRelationKBMatrix()
    m3.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 1.0)
    m3.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m3.set(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse", 1.0)
    m3.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 10.0)

    // different rows/columns
    val m4 = new EntityRelationKBMatrix()
    m4.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 2.0)
    m4.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m4.set(EntityPair("Nicola Sarcozy", "Carla Bruni"), "per:spouse", 1.0)
    m4.set(EntityPair("Barack Obama", "Michelle Obama"), "and his wife", 1.0)

    assertTrue(m1.hasSameContent(m2))
    assertTrue(m2.hasSameContent(m1))
    assertFalse(m2.hasSameContent(m3))
    assertFalse(m3.hasSameContent(m4))
  }

  @Test def writeReadMongoTest() {
    // Fake in-memory mongo server.
    val fongo = new Fongo("myserver");
    val db : DB = fongo.getDB("mydb");

    val m1 = new EntityRelationKBMatrix()
    m1.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 2.0)
    m1.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m1.set(EntityPair("Barack Obama", "Michelle Obama"), "per:spouse", 1.0)
    m1.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 10.0)

    m1.writeToMongo(db)

    val m2 = new EntityRelationKBMatrix
    m2.populateFromMongo(db)
    assertTrue(m1.hasSameContent(m2))
  }

  @Test def pruneMatrixTest() {
    val m = new EntityRelationKBMatrix()
    m.set(EntityPair("Barack Obama", "Michelle Obama"), "is married to", 1.0)
    m.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 1.0)
    m.set(EntityPair("Nicola Sarcozy", "Carla Bruni"), "and his wife", 1.0)

    val m0 = m.prune(0,0)

    val m0goal = new EntityRelationKBMatrix()
    m0goal.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m0goal.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 1.0)
    m0goal.set(EntityPair("Nicola Sarcozy", "Carla Bruni"), "and his wife", 1.0)

    assertTrue(m0.hasSameContent(m0goal))

    val m1 = m.prune(0,1)
    val m1goal = new EntityRelationKBMatrix()
    m1goal.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 1.0)
    m1goal.set(EntityPair("Nicola Sarcozy", "Carla Bruni"), "and his wife", 1.0)

    assertTrue(m1.hasSameContent(m1goal))

    val m2 = m.prune(1,0)
    val m2goal = new EntityRelationKBMatrix()
    m2goal.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "per:spouse", 1.0)
    m2goal.set(EntityPair("Frank Sinatra", "Nancy Barbato"), "and his wife", 1.0)
    assertTrue(m2.hasSameContent(m2goal))
  }

  @Test def testSplitTest() {
    //0101
    //1101
    //0010
    //1101
    val m = new EntityRelationKBMatrix()
    m.set(EntityPair("A", "A"), "1",1.0)
    m.set(EntityPair("A", "A"), "3",1.0)

    m.set(EntityPair("B", "B"), "0",1.0)
    m.set(EntityPair("B", "B"), "1",1.0)
    m.set(EntityPair("B", "B"), "3",1.0)

    m.set(EntityPair("C", "C"), "2",1.0)

    m.set(EntityPair("D", "D"), "0",1.0)
    m.set(EntityPair("D", "D"), "1",1.0)
    m.set(EntityPair("D", "D"), "3",1.0)
    // Just use rows and cols 1,2,3 for testing purposes
    val testRows = Set(EntityPair("B", "B"), EntityPair("C", "C"), EntityPair("D", "D"))
    val testCols = Set("1", "2", "3")

    // Make sure that test passes for different random initialiaztions
    for (seed <- 0 until 10) {
      val random = new Random(seed)
      val (mtrain, mdev, mtest) = m.randomTestSplit(2,3,Some(testRows), Some(testCols), random)
      // Cell 2,2 is not elegible, so there are only 2 cells left for test set
      assertEquals(1.0, mtrain.get(EntityPair("C", "C"), "2"), eps)
      assertEquals(0, mtest.get(EntityPair("C", "C"), "2"), eps)
      assertEquals(0, mdev.get(EntityPair("C", "C"), "2"), eps)
      assertEquals(2,mdev.nnz())
      assertEquals(2,mtest.nnz())
      assertEquals(5,mtrain.nnz())
      // the 3 matrices are a partitoning of m:
      // 1. their size is 2+2+5 = 9
      // 2. they contain all elements
      //assertEquals(m.getNnzCells().toSet, mtrain.getNnzCells().toSet ++ mtest.getNnzCells().toSet ++ mdev.getNnzCells().toSet)
    }
  }

}
