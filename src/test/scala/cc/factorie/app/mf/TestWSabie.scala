package cc.factorie.app.mf

import org.junit._
import org.junit.Assert._
import cc.factorie._
import la._
import optimize.OnlineTrainer
import cc.factorie.variable.DiscreteDomain

/**
 * User: apassos
 * Date: 4/5/13
 * Time: 3:19 PM
 */
class TestWSabie {
  @Test def simpleTestWsabie() {
    val d = new DiscreteDomain(3)
    val m = new WSabie.WSabieModel(d, 5, new java.util.Random(0))
    val q = new SparseBinaryTensor1(3)
    q += 0
    val p = new SparseBinaryTensor1(3)
    p += 1
    val n = new SparseBinaryTensor1(3)
    n += 2
    val e = new WSabie.WSabieExample(m, q, p, n)
    val trainer = new OnlineTrainer(m.parameters)
    while (!trainer.isConverged) {
      trainer.processExamples(Seq(e))
    }
    assert(m.score(q, p) > m.score(q, n))
  }
}
