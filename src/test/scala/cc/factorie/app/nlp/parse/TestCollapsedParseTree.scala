package cc.factorie.app.nlp.parse

import org.junit.{Assert, Before, Test}
import cc.factorie.app.nlp.load.LoadOntonotes5

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
