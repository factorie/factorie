package cc.factorie.tutorial
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.xml._
import cc.factorie._
import cc.factorie.directed._
import cc.factorie.app.topics.lda._
import cc.factorie.variable.CategoricalSeqDomain

// For FUSE research abstracts in XML
object LDA4 {
  
  class Document(domain:CategoricalSeqDomain[String], name:String, contents:Seq[String]) extends cc.factorie.app.topics.lda.Document(domain, name, contents) {
    var year = 2001
  }
  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    val numTopics = 20
    val numIterations = 50
    val tokenizer = cc.factorie.app.strings.alphaSegmenter
    object WordSeqDomain extends CategoricalSeqDomain[String]
    implicit val model = DirectedModel()
    val lda = new LDA(WordSeqDomain, numTopics, 0.1, 0.1)
    var minYear = 9999
    var maxYear = 0000
    
    val directories = args
    for (dirname <- directories) {
      // println("\nReading directory "+dirname)
      // XML processing is slow, so read them in parallel!
      val texts = recursiveFiles(new File(dirname)).filter(_.getName.endsWith("xml"))./*par.*/map(file => {
        val article = XML.withSAXParser(getParser).loadFile(file)
        val text = (article \\ "abstract").text
        val yearTxt = (article \\ "year").text
        // println("yearTxt="+yearTxt)
        val year = if (yearTxt.length >= 4) yearTxt.substring(0,4) else "2000"
        //val doc = Document(WordSeqDomain, file.getCanonicalPath, text)
        print("."); Console.flush()
        (file.getCanonicalPath, text, year)
      })
      texts.foreach({case(name, text, year) => {
        val doc = new Document(WordSeqDomain, name, tokenizer(text).map(_.toLowerCase).filter(!cc.factorie.app.strings.Stopwords.contains(_)).toIndexedSeq)
        doc.year = year.toInt
        if (doc.year > maxYear) maxYear = doc.year
        if (doc.year < minYear) minYear = doc.year
        if (doc.length > 3) { print("+"+doc.year); Console.flush(); lda.addDocument(doc, random) }
      }})
      println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
      /*for (file <- recursiveFiles(new File(dirname))) {
        //println("Considering "+file.getCanonicalPath)
        if (file.getName.endsWith("xml")) {
          //println(file.getName)
          val article = XML.withSAXParser(getParser).loadFile(file)
          val text = (article \\ "abstract").text
          //println("Text: "+text)
          val doc = Document(WordSeqDomain, file.getCanonicalPath, text)
          if (doc.length > 3) {
            lda.addDocument(doc)
            print("."); Console.flush
          }
        }
      }*/
    } 
    if (lda.documents.size == 0) { System.err.println("No documents found."); System.exit(-1) }
    println("Read "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    
    val startTime = System.currentTimeMillis
    lda.inferTopics(numIterations, 10)
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")

    println("minYear=%d maxYear=%d".format(minYear, maxYear))
    val numYears = maxYear - minYear + 1
    val histogram = Array.ofDim[Double](numYears, numTopics)
    for (doc <- lda.documents.asInstanceOf[Iterable[Document]]) maths.ArrayOps.incr(histogram(doc.year - minYear), doc.theta.value.toArray, 1.0)
    for (year <- 0 until histogram.length) maths.ArrayOps.normalize(histogram(year))
    for (phi <- lda.phis) {
      val phiIndex = lda.phis.indexOf(phi)
      println(phi.value.top(20).map(dp => WordSeqDomain.elementDomain.category(dp.index)).mkString(" ")+"  "+Range(0, numYears).map(year => histogram(year)(phiIndex)).mkString(" "))
    }
    
  }
  
  def getParser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    f.setValidating(false)
    f.setFeature("http://xml.org/sax/features/validation", false)
    f.newSAXParser()
  }

  def recursiveFiles(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= recursiveFiles(entry)
    }
    result
  }

}