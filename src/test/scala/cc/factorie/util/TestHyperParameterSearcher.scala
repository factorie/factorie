/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
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
