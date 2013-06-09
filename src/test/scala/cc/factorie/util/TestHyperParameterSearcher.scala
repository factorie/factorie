package cc.factorie.util
import org.junit.Test
import org.junit.Assert._
import scala.concurrent.Future

/**
 * User: apassos
 * Date: 6/9/13
 * Time: 7:51 AM
 */
class TestHyperParameterSearcher {
  @Test def testSimpleParamSearch() {
    class CmdClass extends CmdOptions {
      val value = new CmdOption("value", 0.0, "DOUBLE", "A simple value")
    }
    val cmds = new CmdClass
    val uniformHyper = new UniformDoubleSampler(0.0, 1.0)
    def test(args: Array[String]) = {
      val t = new CmdClass
      t.parse(args)
      Future.successful(t.value.value)
    }
    val optimizer = new HyperParameterSearcher(cmds,
      Seq(HyperParameter(cmds.value, uniformHyper)),
      test, 10, 10, secondsToSleep = 1)
    optimizer.optimize()
    assertTrue(cmds.value.hasValue)
    assertTrue(cmds.value.value > 0.8)
    val seqHyper = new SampleFromSeq(Seq(0.0, 0.1, 0.5, 1.0))
    val cmds2 = new CmdClass
    val optimizer2 = new HyperParameterSearcher(cmds2,
      Seq(HyperParameter(cmds2.value, seqHyper)),
      test, 10, 10, secondsToSleep = 1)
    optimizer2.optimize()
    assertTrue(cmds2.value.hasValue)
    assertEquals(cmds2.value.value, 1.0, 0.0001)
  }
}
