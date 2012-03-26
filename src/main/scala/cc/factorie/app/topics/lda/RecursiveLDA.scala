package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._
import java.io.File

class RecursiveDocument(superDoc:Doc, superTopic:Int) 
extends Document(superDoc.ws.domain, superDoc.name+superTopic, Nil)
{
  for (i <- 0 until superDoc.ws.length)
    if (superDoc.zs.intValue(i) == superTopic) ws.appendInt(superDoc.ws.intValue(i))
  //ws.trimCapacity
}


class RecursiveLDA(wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, beta1:Double = 0.01)(implicit model:MutableGenerativeModel)
extends LDA(wordSeqDomain, numTopics, alpha1, beta1)(model)
{

}

object RecursiveLDA {
  def main(args:Array[String]): Unit = {
    val numTopics = 10
    object WordSeqDomain extends CategoricalSeqDomain[String]
    //implicit val model = new GenerativeFactorModel 
    var lda = new RecursiveLDA(WordSeqDomain, numTopics, 0.01, 0.01)(GenerativeModel())
    val directories = 
      if (args.length == 0) Range(0,13).reverse.map(i => "%02d".format(i)).take(12).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      else args.toSeq

    for (directory <- directories) {
      val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        val doc = Document.fromFile(WordSeqDomain, file, "UTF-8")
        if (doc.length >= 3) lda.addDocument(doc)
        print("."); Console.flush
      }
      println()
    }
    
    println("Read "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    
    val startTime = System.currentTimeMillis
    lda.inferTopics(30, 10)
    var lda2 = Seq.tabulate(numTopics)(i => new RecursiveLDA(WordSeqDomain, numTopics, 0.01, 0.01)(GenerativeModel()))
    for (doc <- lda.documents) {
      for (ti <- doc.zs.uniqueIntValues) {
        val rdoc = new RecursiveDocument(doc, ti)
        if (rdoc.ws.length > 0) lda2(ti).addDocument(rdoc)
      }
    }
    lda2.par.foreach(_.inferTopics(30, 10))
    println("Second-layer topics")
    for (i <- 0 until lda2.length) {
      println()
      println(lda.topicSummary(i, 10))
      println("  "+lda2(i).topicsSummary().replace("\n", "\n  "))
    }
    println("Finished in "+(System.currentTimeMillis - startTime)+" ms.")
    
    // Build single flat LDA
    lda = null
    val bigNumTopics = numTopics * numTopics
    val lda3 = new RecursiveLDA(WordSeqDomain, bigNumTopics, 0.01, 0.01)(GenerativeModel())
    for (ldaIndex <- 0 until numTopics; doc <- lda2(ldaIndex).documents) {
      doc.theta = null
      val oldZs = doc.zs
      val innerLda = lda2(ldaIndex)
      doc.zs = new lda3.Zs(oldZs.map(i => i.intValue + ldaIndex*numTopics))
      lda3.addDocument(doc)
    }
    lda2 = null // To allow garbage collection
    lda3.maximizePhisAndThetas
    println("Flat LDA")
    println(lda3.topicsSummary(10))
  }
  
}
