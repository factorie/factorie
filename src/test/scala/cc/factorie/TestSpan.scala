package cc.factorie

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import junit.framework._
import Assert._
import cc.factorie.app.nlp.segment.Tokenizer1
import cc.factorie.variable.SpanList

class TestSpanVariable extends TestCase  with cc.factorie.util.FastLogging {
  
  class MySpanList extends SpanList[TokenSpan,Section,Token]

  def testDiffLists(): Unit = {
     val doc = load.LoadPlainText.fromString("aaa bb John Smith eee ff ggg").head
     val sl = new MySpanList
     doc.attr += sl
       
     Tokenizer1.process(doc)
     //doc.foreach(logger.debug(_))
     assertEquals(7, doc.tokenCount)
     val d = new DiffList
     val s1 = new TokenSpan(doc.asSection, 1, 1)
     doc.attr[MySpanList].add(s1)(d)
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
     val s2 = new NerSpan(doc.asSection, "PER", 2, 2)(d)
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
