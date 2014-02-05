package cc.factorie

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import junit.framework._
import Assert._
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.variable.{SpanVarBuffer, SpanVarList}

class TestSpanVariable extends TestCase  with cc.factorie.util.FastLogging {
  
  class MySpanBuffer extends SpanVarBuffer[TokenSpan,Section,Token]

  def testDiffLists(): Unit = {
     val doc = load.LoadPlainText.fromString("aaa bb John Smith eee ff ggg").head
     val sl = new MySpanBuffer
     doc.attr += sl
       
     DeterministicTokenizer.process(doc)
     //doc.foreach(logger.debug(_))
     assertEquals(7, doc.tokenCount)
     val d = new DiffList
     val s1 = new TokenSpan(doc.asSection, 1, 1)
     doc.attr[MySpanBuffer].add(s1)(d)
     assert(sl.head.start == 1)
     //logger.debug("DiffList "+d)
     //logger.debug("new span 1 1")
     //logger.debug(doc.spans.mkString("\n"))
     //logger.debug("DiffList "+d)
     d.undo()
     //logger.debug("undo")
     //logger.debug("DiffList "+d)
     //logger.debug(doc.spans.mkString("\n"))
     assert(sl.length == 0)
     val s2 = new ConllNerSpan(doc.asSection, 2, 2, "PER")
     sl += s2
     assert(s2.phrase == "John Smith")
     val s3 = new TokenSpan(doc.asSection, 4, 1)
     sl += s3
     assert(sl.spansOfClass[NerSpan].length == 1)
     val d2 = new DiffList
     sl.remove(s3)(d2)
     assert(sl.length == 1)
     d2.undo()
     assert(sl.length == 2)
     sl.clear()
     assert(sl.length == 0)
   }

}


object TestSpanVariable extends TestSuite {
  addTestSuite(classOf[TestSpanVariable])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
