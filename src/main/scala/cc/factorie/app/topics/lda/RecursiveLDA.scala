package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._
import scala.collection.mutable.HashMap
import java.io.{PrintWriter, FileWriter, File, BufferedReader, InputStreamReader, FileInputStream}
import collection.mutable.{ArrayBuffer, HashSet, HashMap}

class RecursiveDocument(superDoc:Doc, val superTopic:Int) extends Document(superDoc.ws.domain, superDoc.name+superTopic, Nil)
{
  for (i <- 0 until superDoc.ws.length)
    if (superDoc.zs.intValue(i) == superTopic) ws.appendInt(superDoc.ws.intValue(i))
  ws.trimCapacity
}


class RecursiveLDA(wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, beta1:Double = 0.01)(implicit model:MutableGenerativeModel)
extends LDA(wordSeqDomain, numTopics, alpha1, beta1)(model)


object RecursiveLDA {
  import scala.util.control.Breaks._
  val minDocLength = 5

  def main(args:Array[String]): Unit = {
    var verbose = false
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val numTopics =     new CmdOption("num-topics", 't', 10, "N", "Number of topics at each of the two recursive levels; total number of topics will be N*N.")
      val numLayers =     new CmdOption("num-layers", 'l', 2, "N", "Number of layers of recursion in topic tree; currently only values accepted are 1 and 2.")
      val alpha =         new CmdOption("alpha", 0.1, "N", "Dirichlet parameter for per-document topic proportions.")
      val beta =          new CmdOption("beta", 0.01, "N", "Dirichlet parameter for per-topic word proportions.")
      val numThreads =    new CmdOption("num-threads", 2, "N", "Number of threads for multithreaded topic inference.")
      val numIterations = new CmdOption("num-iterations", 'i', 50, "N", "Number of iterations of inference.")
      val diagnostic =    new CmdOption("diagnostic-interval", 'd', 10, "N", "Number of iterations between each diagnostic printing of intermediate results.")
      val diagnosticPhrases= new CmdOption("diagnostic-phrases", false, "true|false", "If true diagnostic printing will include multi-word phrases.")
      val fitAlpha =      new CmdOption("fit-alpha-interval", Int.MaxValue, "N", "Number of iterations between each re-estimation of prior on per-document topic distribution.")
      val tokenRegex =    new CmdOption("token-regex", "\\p{Alpha}+", "REGEX", "Regular expression for segmenting tokens.")
      val readDirs =      new CmdOption("read-dirs", List(""), "DIR...", "Space-(or comma)-separated list of directories containing plain text input files.")
      val readNIPS=       new CmdOption("read-nips", List(""), "DIR...", "Read data from McCallum's local directory of NIPS papers.")
      val readLines =     new CmdOption("read-lines", "", "FILENAME", "File containing lines of text, one for each document.")
      val readLinesRegex= new CmdOption("read-lines-regex", "", "REGEX", "Regular expression with parens around the portion of the line that should be read as the text of the document.")
      val readLinesRegexGroups= new CmdOption("read-lines-regex-groups", List(1), "GROUPNUMS", "The --read-lines-regex group numbers from which to grab the text of the document.")
      val readLinesRegexPrint = new CmdOption("read-lines-regex-print", false, "BOOL", "Print the --read-lines-regex match that will become the text of the document.")
      val readDocs =      new CmdOption("read-docs", "lda-docs.txt", "FILENAME", "Add documents from filename , reading document names, words and z assignments") 
      val writeDocs =     new CmdOption("write-docs", "lda-docs.txt", "FILENAME", "Save LDA state, writing document names, words and z assignments") 
      val maxNumDocs =    new CmdOption("max-num-docs", Int.MaxValue, "N", "The maximum number of documents to read.")
      val printTopics =   new CmdOption("print-topics", 20, "N", "Just before exiting print top N words for each topic.")
      val printPhrases =  new CmdOption("print-topics-phrases", 20, "N", "Just before exiting print top N phrases for each topic.")
      val verboseOpt =       new CmdOption("verbose", "Turn on verbose output") { override def invoke = verbose = true }
      // TODO Add stopwords option
    }
    opts.parse(args)
    val numTopics = opts.numTopics.value
    println("numTopics="+numTopics)
    object WordSeqDomain extends CategoricalSeqDomain[String]
    //implicit val model = GenerativeModel() 
    var lda = new RecursiveLDA(WordSeqDomain, numTopics, opts.alpha.value, opts.beta.value)(GenerativeModel())
    val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(opts.tokenRegex.value.r)
    if (opts.readDirs.wasInvoked) {
      for (directory <- opts.readDirs.value) {
        val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
        println("Reading files from directory " + directory)
        breakable { for (file <- new File(directory).listFiles; if (file.isFile)) {
          if (lda.documents.size == opts.maxNumDocs.value) break
          val doc = Document.fromFile(WordSeqDomain, file, "UTF-8", segmenter = mySegmenter)
          if (doc.length >= minDocLength) lda.addDocument(doc)
          if (lda.documents.size % 1000 == 0) { print(" "+lda.documents.size); Console.flush() }; if (lda.documents.size % 10000 == 0) println()
        }}
        //println()
      }
    }
    if (opts.readNIPS.wasInvoked) {
      // A temporary hack for McCallum's development/debugging
      val directories = Range(0,13).reverse.map(i => "%02d".format(i)).take(12).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      for (directory <- directories) {
        val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
        println("Reading NIPS files from directory " + directory)
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          val doc = Document.fromFile(WordSeqDomain, file, "UTF-8")
          if (doc.length >= 3) lda.addDocument(doc)
          print("."); Console.flush
        }
        println()
      }
    }
    if (opts.readLines.wasInvoked) {
      val name = if (opts.readLines.value == "-") "stdin" else opts.readLines.value
      val source = if (opts.readLines.value == "-") scala.io.Source.stdin else scala.io.Source.fromFile(new File(opts.readLines.value))
      var count = 0
      breakable { for (line <- source.getLines()) {
        if (lda.documents.size == opts.maxNumDocs.value) break
        val text: String = 
          if (!opts.readLinesRegex.wasInvoked) line 
          else {
            val textbuffer = new StringBuffer
            for (groupIndex <- opts.readLinesRegexGroups.value) {
            	val mi = opts.readLinesRegex.value.r.findFirstMatchIn(line).getOrElse(throw new Error("No regex match for --read-lines-regex in "+line))
            	if (mi.groupCount >= groupIndex) textbuffer append mi.group(groupIndex)
            	else throw new Error("No group found with index "+groupIndex)
            }
            textbuffer.toString
          }
        if (text eq null) throw new Error("No () group for --read-lines-regex in "+line)
        if (opts.readLinesRegexPrint.value) println(text)
        val doc = Document.fromString(WordSeqDomain, name+":"+count, text, segmenter = mySegmenter)
        if (doc.length >= minDocLength) lda.addDocument(doc)
        count += 1
        if (count % 1000 == 0) { print(" "+count); Console.flush() }; if (count % 10000 == 0) println()
      }}
      source.close()
    }
    /* // Need to determine an on-disk representation for RecursiveLDA 
    if (opts.readDocs.wasInvoked) {
      val file = new File(opts.readDocs.value)
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
      breakable { while (true) {
        if (lda.documents.size == opts.maxNumDocs.value) break
        val doc = new Document(WordSeqDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
        doc.zs = new lda.Zs(Nil)
        val numWords = doc.readNameWordsZs(reader)
        if (numWords < 0) break
        else if (numWords >= minDocLength) lda.addDocument(doc) // Skip documents that have only one word because inference can't handle them
        else System.err.println("--read-docs skipping document %s: only %d words found.".format(doc.name, numWords))
      }}
      reader.close()
      lda.maximizePhisAndThetas
      //println(lda.documents.head.ws.categoryValues.mkString(" "))
      //println(lda.documents.head.zs.intValues.mkString(" "))
    }*/
    if (lda.documents.size == 0) { System.err.println("You must specific either the --input-dirs or --input-lines options to provide documents."); System.exit(-1) }
    println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    
    // Fit top-level LDA model    
    val startTime = System.currentTimeMillis
    lda.inferTopics(opts.numIterations.value, opts.fitAlpha.value, 10)
    
    // Clear memory of parameters; we only need the documents (their words and zs)
    var documents1 = lda.documents.toSeq
    val summaries1 = Seq.tabulate(numTopics)(i => lda.topicSummary(i, 10))
    lda = null
    for (doc <- documents1) doc.theta = null
    System.gc()
    
    var documents2 = new ArrayBuffer[RecursiveDocument]
        
    // Create next-level LDA models, each with its own GenerativeModel, in chunks of size opts.numThreads
    if (opts.numLayers.value > 1) {
      for (topicRange <- Range(0, numTopics).grouped(opts.numThreads.value)) {
        val topicRangeStart: Int = topicRange.head
        var lda2 = Seq.tabulate(topicRange.size)(i => new RecursiveLDA(WordSeqDomain, numTopics, opts.alpha.value, opts.beta.value)(GenerativeModel()))
        for (i <- topicRange) lda2(i-topicRangeStart).diagnosticName = "Super-topic: "+summaries1(i) // So that the super-topic gets printed before each diagnostic list of subtopics
        for (doc <- documents1) {
          for (ti <- doc.zs.uniqueIntValues) if (topicRange.contains(ti)) {
            val rdoc = new RecursiveDocument(doc, ti)
            if (rdoc.ws.length > 0) { lda2(ti - topicRangeStart).addDocument(rdoc); documents2 += rdoc }
          }
        }
        println("Starting second-layer parallel inference for "+topicRange)
        lda2.par.foreach(_.inferTopics(opts.numIterations.value, opts.fitAlpha.value, 10))
      }
      documents1 = null // To allow garbage collection, but note that we now loose word order in lda3 documents 
    } else if (false) {
      var lda2 = Seq.tabulate(numTopics)(i => new RecursiveLDA(WordSeqDomain, numTopics, opts.alpha.value, opts.beta.value)(GenerativeModel()))
      for (i <- 0 until numTopics) lda2(i).diagnosticName = "Super-topic: "+summaries1(i) // So that the super-topic gets printed before each diagnostic list of subtopics
      for (doc <- documents1) {
        for (ti <- doc.zs.uniqueIntValues) {
          val rdoc = new RecursiveDocument(doc, ti)
          if (rdoc.ws.length > 0) { lda2(ti).addDocument(rdoc); documents2 += rdoc }
        }
      }
      documents1 = null // To allow garbage collection, but note that we now loose word order in lda3 documents 
      println("Starting second-layer parallel inference.")
      lda2.par.foreach(_.inferTopics(opts.numIterations.value, opts.fitAlpha.value, 10))
      println("Second-layer topics")
      for (i <- 0 until lda2.length) {
        println()
        println(summaries1(i))
        println("  "+lda2(i).topicsSummary().replace("\n", "\n  "))
      }
    }
    println("Finished in "+(System.currentTimeMillis - startTime)+" ms.")
    if (opts.numLayers.value == 1) System.exit(0)
    
    // Build single flat LDA
    val bigNumTopics = numTopics * numTopics
    val lda3 = new RecursiveLDA(WordSeqDomain, bigNumTopics, opts.alpha.value, opts.beta.value)(GenerativeModel())
    while (documents2.size > 0) {
      val doc = documents2.last
      val doc3 = lda3.documentMap.getOrElse(doc.name, { val d = new Document(WordSeqDomain, doc.name, Nil); d.zs = new lda3.Zs; d })
      val ws = doc.ws; val zs = doc.zs
      var i = 0; val len = doc.length
      while (i < len) {
        val zi = doc.superTopic * numTopics + zs.intValue(i)
        val wi = ws.intValue(i)
        lda3.phis(zi).tensor.+=(wi, 1.0) // This avoids? the need for estimating phis later; note that we are not setting thetas here.
        doc3.ws.appendInt(wi)
        doc3.zs.appendInt(zi)
        i += 1
      }
      if (!lda3.documentMap.contains(doc3.name)) lda3.addDocument(doc3)
      documents2.remove(documents2.size-1) // Do this to enable garbage collection as we create more doc3's
    }
//    for (ldaIndex <- 0 until numTopics; doc <- lda2(ldaIndex).documents) {
//      doc.theta = null
//      val oldZs = doc.zs
//      val innerLda = lda2(ldaIndex)
//      doc.zs = new lda3.Zs(oldZs.map(i => i.intValue + ldaIndex*numTopics))
//      lda3.addDocument(doc)
//    }
//    lda2 = null // To allow garbage collection
//    lda3.maximizePhisAndThetas
    println("Flat LDA")
    println(lda3.topicsSummary(10))
    
    if (opts.writeDocs.wasInvoked) {
      val file = new File(opts.writeDocs.value)
      val pw = new PrintWriter(file)
      lda3.documents.foreach(_.writeNameWordsZs(pw))
      pw.close()
    }
    
    if (opts.printTopics.wasInvoked) 
      println(lda3.topicsSummary(opts.printTopics.value))
    if (opts.printPhrases.wasInvoked) 
      println(lda3.topicsWordsAndPhrasesSummary(opts.printPhrases.value, opts.printPhrases.value))

  }
  
}
