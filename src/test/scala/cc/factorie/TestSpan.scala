package cc.factorie

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import junit.framework._
import Assert._
import cc.factorie.app.nlp.segment.ClearTokenizer
import Implicits._

class TestSpanVariable extends TestCase  with cc.factorie.util.FastLogging {

   def testDiffLists:Unit = {
     val doc = LoadPlainText.fromString("aaa bb John Smith eee ff ggg").head
     ClearTokenizer.process(doc)
     //doc.foreach(logger.debug(_))
     assertEquals(7, doc.tokenCount)
     val d = new DiffList
     val s1 = new TokenSpan(doc.asSection, 1, 1)(d)
     assert(doc.asSection.spans.head.start == 1)
     //logger.debug("DiffList "+d)
     //logger.debug("new span 1 1")
     //logger.debug(doc.spans.mkString("\n"))
     //logger.debug("DiffList "+d)
     d.undo
     //logger.debug("undo")
     //logger.debug("DiffList "+d)
     //logger.debug(doc.spans.mkString("\n"))
     assert(doc.asSection.spans.length == 0)
     val s2 = new NerSpan(doc.asSection, "PER", 2, 2)(d)
     assert(s2.phrase == "John Smith")
     val s3 = new TokenSpan(doc.asSection, 4, 1)(d)
     assert(doc.asSection.spansOfClass[NerSpan].length == 1)
     val d2 = new DiffList
     doc.asSection.removeSpan(s3)(d2)
     assert(doc.asSection.spans.length == 1)
     d2.undo
     assert(doc.asSection.spans.length == 2)
     doc.asSection.clearSpans(null)
     assert(doc.asSection.spans.length == 0)
   }
   
}


object TestSpanVariable extends TestSuite {
  addTestSuite(classOf[TestSpanVariable])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
