package cc.factorie.util

import cc.factorie.db.mongo.MongoCubbieConverter
import org.scalatest._

/**
 * @author John Sullivan
 */
class TestIntAndDoubleSeqCubbie extends FlatSpec with Matchers {

  def doubleSeqFixture = new {
    class DoubleSeqCubbie extends Cubbie {
      val doubleSeq = DoubleSeqSlot("test")
    }
    val c = new DoubleSeqCubbie
    val a = Array(1.9, 122323.999, -1293.99127361)
    val d = DoubleSeq(a)
    c.doubleSeq := d
  }

  "DoubleSeqSlot" should "work" in {
    val f = doubleSeqFixture
    import f._
    assert(c.doubleSeq.value.asArray.zip(f.a).forall{case (x, y) => x == y})
  }

  "IntSeqSlot" should "work" in {
    val c = new Cubbie {
      val intSeq = IntSeqSlot("test")
    }
    val a = Array(1, 1999, 49923, -237194)
    val i = new ArrayIntSeq(a)
    c.intSeq := i
    assert(c.intSeq.value.asArray.zip(a).forall{case (x, y) => x == y})
  }
  
  "IntSeq conversion" should "work" in {
    class MyCubbie extends Cubbie { val i = IntSeqSlot("test") }
    val a = new ArrayIntSeq(Array(1,2,3))
    val c = new MyCubbie 
    c.i := a
    val dbo = MongoCubbieConverter.eagerDBO(c)
    val c2 = MongoCubbieConverter.eagerCubbie(dbo, () => new MyCubbie)
    assert(c2.i.value.asSeq == c.i.value.asSeq)
  }
  
}
