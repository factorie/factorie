/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.parse

import cc.factorie.app.nlp.load.LoadOntonotes5
import org.junit.{Assert, Test}

/**
 * Created by diwe01 on 17.06.14.
 */
class TestCollapsedParseTree {

  val testFileName = this.getClass.getResource("/parser-test-input").getPath()

  @Test
  def testCollapsing() = {
    val testDoc = LoadOntonotes5.fromFilename(testFileName).head
    val testSentence = testDoc.sentences.tail.head

    val tree = ParseTree2.collapsedFromParseTree(testSentence.parse)
    Assert.assertNotNull(tree)
    Assert.assertNotNull(tree.toString)
    Assert.assertEquals(tree.labels.length, tree.parents.length)
    Assert.assertEquals(tree.labels.length, tree.vertices.length)
  }

}
