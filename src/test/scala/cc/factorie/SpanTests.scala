package cc.factorie

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import junit.framework._
import Assert._

class TestSpanVariable extends TestCase {

   def testDiffLists:Unit = {
     val doc = LoadPlainText.fromString("testdoc", "aaa bb John Smith eee ff ggg", false)
     //doc.foreach(println(_))
     assert(doc.length == 7)
     val d = new DiffList
     val s1 = new TokenSpan(doc, 1, 1)(d)
     assert(doc.spans.head.start == 1)
     //println("DiffList "+d)
     //println("new span 1 1")
     //println(doc.spans.mkString("\n"))
     //println("DiffList "+d)
     d.undo
     //println("undo")
     //println("DiffList "+d)
     //println(doc.spans.mkString("\n"))
     assert(doc.spans.length == 0)
     val s2 = new NerSpan(doc, "PER", 2, 2)(d)
     assert(s2.phrase == "John Smith")
     val s3 = new TokenSpan(doc, 4, 1)(d)
     assert(doc.spansOfClass[NerSpan].length == 1)
     val d2 = new DiffList
     doc.removeSpan(s3)(d2)
     assert(doc.spans.length == 1)
     d2.undo
     assert(doc.spans.length == 2)
     doc.clearSpans(null)
     assert(doc.spans.length == 0)
   }
   
}


object TestSpanVariable extends TestSuite {
  addTestSuite(classOf[TestSpanVariable])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
