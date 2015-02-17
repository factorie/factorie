package cc.factorie.epistemodb

import org.scalatest.junit.JUnitSuite
import cc.factorie.util
import org.junit.Test
import org.junit.Assert._
import com.github.fakemongo.Fongo
import com.mongodb.DB

/**
 * Created by beroth on 2/6/15.
 */
class TestKBMatrix extends JUnitSuite  with util.FastLogging  {

  val eps = 1e-4

  @Test def getSetCellsTest() {
    val m = new KBMatrix()
    m.set("Barack Obama", "Michelle Obama", "is married to", 5.0)
    m.set("Barack Obama", "Michelle Obama", "is married to", 10.0)
    m.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)
    assertEquals(m.numRows(),2)
    assertEquals(m.numCols(),3)
    assertEquals(10.0, m.get("Barack Obama", "Michelle Obama", "is married to"), eps)
    assertEquals(1.0, m.get("Barack Obama", "Michelle Obama", "per:spouse"), eps)
    assertEquals(1.0, m.get("Frank Sinatra", "Nancy Barbato", "per:spouse"), eps)
    assertEquals(2.0, m.get("Frank Sinatra", "Nancy Barbato", "and his wife"), eps)
    assertEquals(0.0, m.get("Nicola Sarcozy", "Carla Bruni", "per:spouse"), eps)
    assertEquals(0.0, m.get("Barack Obama", "Michelle Obama", "and his wife"), eps)
  }

  @Test def equalsTest() {
    val m1 = new KBMatrix()
    m1.set("Barack Obama", "Michelle Obama", "is married to", 5.0)
    m1.set("Barack Obama", "Michelle Obama", "is married to", 10.0)
    m1.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m1.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m1.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)

    // same as m1, but constructed in different order
    val m2 = new KBMatrix()
    m2.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)
    m2.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m2.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m2.set("Barack Obama", "Michelle Obama", "is married to", 10.0)

    // similar to m2, but one different cell value
    val m3 = new KBMatrix()
    m3.set("Frank Sinatra", "Nancy Barbato", "and his wife", 1.0)
    m3.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m3.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m3.set("Barack Obama", "Michelle Obama", "is married to", 10.0)

    // different rows/columns
    val m4 = new KBMatrix()
    m4.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)
    m4.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m4.set("Nicola Sarcozy", "Carla Bruni", "per:spouse", 1.0)
    m4.set("Barack Obama", "Michelle Obama", "and his wife", 1.0)

    assertTrue(m1.hasSameContent(m2))
    assertTrue(m2.hasSameContent(m1))
    assertFalse(m2.hasSameContent(m3))
    assertFalse(m3.hasSameContent(m4))
  }

  @Test def writeReadMongo() {
    // Fake in-memory mongo server.
    val fongo = new Fongo("myserver");
    val db : DB = fongo.getDB("mydb");

    val m1 = new KBMatrix()
    m1.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)
    m1.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m1.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m1.set("Barack Obama", "Michelle Obama", "is married to", 10.0)

    m1.writeToMongo(db)

    val m2 = KBMatrix.fromMongo(db)
    assertTrue(m1.hasSameContent(m2))
  }


  @Test def writeReadMongoCellBased() {
    // Fake in-memory mongo server.
    val fongo = new Fongo("myserver");
    val db : DB = fongo.getDB("mydb");

    val m1 = new KBMatrix()
    m1.set("Frank Sinatra", "Nancy Barbato", "and his wife", 2.0)
    m1.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m1.set("Barack Obama", "Michelle Obama", "per:spouse", 1.0)
    m1.set("Barack Obama", "Michelle Obama", "is married to", 10.0)

    m1.writeToMongoCellBased(db)

    val m2 = KBMatrix.fromMongoCellBased(db)
    assertTrue(m1.hasSameContent(m2))
  }

  @Test def pruneMatrixTest() {
    val m = new KBMatrix()
    m.set("Barack Obama", "Michelle Obama", "is married to", 1.0)
    m.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m.set("Frank Sinatra", "Nancy Barbato", "and his wife", 1.0)
    m.set("Nicola Sarcozy", "Carla Bruni", "and his wife", 1.0)

    val m0 = m.prune(0,0)

    val m0goal = new KBMatrix()
    m0goal.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m0goal.set("Frank Sinatra", "Nancy Barbato", "and his wife", 1.0)
    m0goal.set("Nicola Sarcozy", "Carla Bruni", "and his wife", 1.0)

    assertTrue(m0.hasSameContent(m0goal))

    val m1 = m.prune(0,1)
    val m1goal = new KBMatrix()
    m1goal.set("Frank Sinatra", "Nancy Barbato", "and his wife", 1.0)
    m1goal.set("Nicola Sarcozy", "Carla Bruni", "and his wife", 1.0)

    assertTrue(m1.hasSameContent(m1goal))

    val m2 = m.prune(1,0)
    val m2goal = new KBMatrix()
    m2goal.set("Frank Sinatra", "Nancy Barbato", "per:spouse", 1.0)
    m2goal.set("Frank Sinatra", "Nancy Barbato", "and his wife", 1.0)
    assertTrue(m2.hasSameContent(m2goal))
  }

}
