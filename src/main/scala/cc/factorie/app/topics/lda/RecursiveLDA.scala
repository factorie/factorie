package cc.factorie.app.topics.lda
import cc.factorie._
import scala.collection.mutable.HashMap
import java.io.{PrintWriter, FileWriter, File, BufferedReader, InputStreamReader, FileInputStream}
import collection.mutable.{ArrayBuffer, HashSet, HashMap}
import cc.factorie.directed._
import cc.factorie.variable.{DiscreteSeqDomain, DiscreteSeqVariable, DiscreteDomain, CategoricalSeqDomain}

// Name here must match superDoc exactly, in order to enable stitching back together again at the end 
class RecursiveDocument(superDoc:Doc, val superTopic:Int) extends Document(superDoc.ws.domain, superDoc.name, Nil)
{
  {
    var prevInTopic = false
    time = superDoc.time
    for (i <- 0 until superDoc.ws.length)
      if (superDoc.zs.intValue(i) == superTopic) {
        ws.appendInt(superDoc.ws.intValue(i))
        if (superDoc.breaks.contains(i) || !prevInTopic) this.breaks += (ws.length-1) // preserve phrase boundary breaks, which go at the end of each word that begins a phrase
        prevInTopic = true
      } else prevInTopic = false
    ws.trimCapacity
  }
}

// Example command-lines:
// Multi-threaded, but all on one machine:
// --read-docs mytextdatadir --num-iterations=30 --fit-alpha-interval=10  --diagnostic-phrases --print-topics-phrases --write-docs recursive-lda-docs.txt
// Serialized per recursive division
// --read-docs mytextdatadir --num-iterations=30 --num-layers 1 --fit-alpha-interval=10  --diagnostic-phrases --print-topics-phrases --write-docs recursive-lda-docs.txt
// --read-docs recursive-lda-docs.txt --read-docs-topic-index 5 --num-layers 1 --num-iterations=30 --fit-alpha-interval=10  --diagnostic-phrases --print-topics-phrases --write-docs recursive-lda-docs5.txt


class RecursiveLDA(wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, beta1:Double = 0.01)(implicit model:MutableDirectedModel, override implicit val random: scala.util.Random) extends LDA(wordSeqDomain, numTopics, alpha1, beta1)(model, random)


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
      val readDocsTopicIndex = new CmdOption("read-docs-topic-index", 0, "N", "Only include in this model words that were assigned to the given topic index.  (Used for disk-based parallelism.)")
      val writeDocs =     new CmdOption("write-docs", "lda-docs.txt", "FILENAME", "Save LDA state, writing document names, words and z assignments") 
      val maxNumDocs =    new CmdOption("max-num-docs", Int.MaxValue, "N", "The maximum number of documents to read.")
      val printTopics =   new CmdOption("print-topics", 20, "N", "Just before exiting print top N words for each topic.")
      val printPhrases =  new CmdOption("print-topics-phrases", 20, "N", "Just before exiting print top N phrases for each topic.")
      val verboseOpt =    new CmdOption("verbose", "Turn on verbose output") { override def invoke = verbose = true }
      // TODO Add stopwords option
    }
    opts.parse(args)
    implicit val random = new scala.util.Random(0)
    val numTopics = opts.numTopics.value
    println("numTopics="+numTopics)
    object WordSeqDomain extends CategoricalSeqDomain[String]
    //implicit val model = DirectedModel()
    var lda = new RecursiveLDA(WordSeqDomain, numTopics, opts.alpha.value, opts.beta.value)(DirectedModel(),random)
    val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(opts.tokenRegex.value.r)
    if (opts.readDirs.wasInvoked) {
      for (directory <- opts.readDirs.value) {
        val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
        println("Reading files from directory " + directory)
        breakable { for (file <- new File(directory).listFiles; if file.isFile) {
          if (lda.documents.size == opts.maxNumDocs.value) break()
          val doc = Document.fromFile(WordSeqDomain, file, "UTF-8", segmenter = mySegmenter)
          doc.time = file.lastModified
          if (doc.length >= minDocLength) lda.addDocument(doc, random)
          if (lda.documents.size % 1000 == 0) { print(" "+lda.documents.size); Console.flush() }; if (lda.documents.size % 10000 == 0) println()
        }}
        //println()
      }
      // Now that we have the full min-max range of dates, set the doc.stamps values to a 0-1 normalized value
      val dates = lda.documents.map(_.time)
      maxDate = dates.max
      minDate = dates.min
      dateRange = maxDate - minDate
      //lda.documents.foreach(doc => doc.stamps.foreach(_ := (doc.date - minDate) / dateRange)) 
    }
    if (opts.readNIPS.wasInvoked) {
      // A temporary hack for McCallum's development/debugging
      val directories = Range(0,13).reverse.map(i => "%02d".format(i)).take(8).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      for (directory <- directories) {
        val year = directory.takeRight(2).toInt
        //println("RecursiveLDA directory year "+year)
        val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
        println("Reading NIPS files from directory " + directory)
        for (file <- new File(directory).listFiles; if file.isFile) {
          val doc = Document.fromFile(WordSeqDomain, file, "UTF-8", segmenter = mySegmenter)
          doc.time = year
          if (doc.length >= 3) lda.addDocument(doc, random)
          print("."); Console.flush()
        }
        println()
      }
      val dates = lda.documents.map(_.time)
      maxDate = dates.max
      minDate = dates.min
      dateRange = maxDate - minDate
    }
    if (opts.readLines.wasInvoked) {
      val name = if (opts.readLines.value == "-") "stdin" else opts.readLines.value
      val source = if (opts.readLines.value == "-") scala.io.Source.stdin else scala.io.Source.fromFile(new File(opts.readLines.value))
      var count = 0
      breakable { for (line <- source.getLines()) {
        if (lda.documents.size == opts.maxNumDocs.value) break()
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
        if (doc.length >= minDocLength) lda.addDocument(doc, random)
        count += 1
        if (count % 1000 == 0) { print(" "+count); Console.flush() }; if (count % 10000 == 0) println()
      }}
      source.close()
    }
    // On-disk representation for RecursiveLDA input/output
    if (opts.readDocs.wasInvoked) {
      val file = new File(opts.readDocs.value)
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
      reader.mark(512)
      val alphasName = reader.readLine()
      if (alphasName == "/alphas") { // If they are present, read the alpha parameters.
        val alphasString = reader.readLine(); lda.alphas.value := alphasString.split(" ").map(_.toDouble) // set lda.alphas
        reader.readLine() // consume delimiting newline
        println("Read alphas "+lda.alphas.value.mkString(" "))
      } else reader.reset() // Put the reader back to the read position when reader.mark was called
      breakable { while (true) {
        if (lda.documents.size == opts.maxNumDocs.value) break()
        val doc = new Document(WordSeqDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
        doc.zs = lda.newZs
        val filterTopicIndex = opts.readDocsTopicIndex.value
        // If readDocsTopicIndex.wasInvoked then only read in words that had been assigned readDocsTopicIndex.value, and reassign them random Zs
        val numWords = if (opts.readDocsTopicIndex.wasInvoked) doc.readNameWordsMapZs(reader, ti => if (ti == filterTopicIndex) random.nextInt(numTopics) else -1) else doc.readNameWordsZs(reader)
        if (numWords < 0) break()
        else if (numWords >= minDocLength) lda.addDocument(doc, random) // Skip documents that have only one word because inference can't handle them
        else if (!opts.readDocsTopicIndex.wasInvoked) System.err.println("--read-docs skipping document %s: only %d words found.".format(doc.name, numWords))
      }}
      reader.close()
      lda.maximizePhisAndThetas
    }
    if (lda.documents.size == 0) { System.err.println("You must specific either the --input-dirs or --input-lines options to provide documents."); System.exit(-1) }
    println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    //lda.documents.filter(_.name.endsWith("0003.txt")).foreach(d => println(d.toString)) // print example documents

    // Fit top-level LDA model    
    val startTime = System.currentTimeMillis
    lda.inferTopics(opts.numIterations.value, opts.fitAlpha.value, 10)
    
    // Clear memory of parameters; we only need the documents (their words and zs)
    var documents1 = lda.documents.toSeq
    val summaries1 = Seq.tabulate(numTopics)(i => lda.topicSummary(i, 10))
    lda = null
    for (doc <- documents1) doc.theta = null
    System.gc()
    
    // Create next-level LDA models, each with its own DirectedModel, in chunks of size opts.numThreads
    if (opts.numLayers.value > 1) {
      var documents2 = new ArrayBuffer[RecursiveDocument]
      for (topicRange <- Range(0, numTopics).grouped(opts.numThreads.value)) {
        val topicRangeStart: Int = topicRange.head
        var lda2 = Seq.tabulate(topicRange.size)(i => new RecursiveLDA(WordSeqDomain, numTopics, opts.alpha.value, opts.beta.value)(DirectedModel(),random))
        for (i <- topicRange) lda2(i-topicRangeStart).diagnosticName = "Super-topic: "+summaries1(i) // So that the super-topic gets printed before each diagnostic list of subtopics
        for (doc <- documents1) {
          for (ti <- doc.zs.uniqueIntValues) if (topicRange.contains(ti)) {
            val rdoc = new RecursiveDocument(doc, ti)
            //if (rdoc.name.endsWith("0003.txt")) { println("topic="+ti+" super: "+doc.toString+"\nsub: "+rdoc.toString+"\n\n") }
            if (rdoc.ws.length > 0) { lda2(ti - topicRangeStart).addDocument(rdoc, random); documents2 += rdoc }
          }
        }
        println("Starting second-layer parallel inference for "+topicRange)
        lda2.par.foreach(_.inferTopics(opts.numIterations.value, opts.fitAlpha.value, 10))
        println("Ended second-layer parallel inference for "+topicRange)
        for (subLda <- lda2; ti <- 0 until numTopics) {
          val tp = timeMeanAlphaBetaForTopic(subLda.documents, ti) // time parameters
          println("%s  mean=%g variance=%g".format(subLda.topicSummary(ti), tp._1, tp._2))
        }
      }
      documents1 = null // To allow garbage collection, but note that we now loose word order in lda3 documents 
      println("Finished in "+(System.currentTimeMillis - startTime)+" ms.")
      // Re-assemble the documents, and optionally write them out.
      val documents3 = new HashMap[String,Document]
      object ZDomain3 extends DiscreteDomain(numTopics * numTopics)
      object ZSeqDomain3 extends DiscreteSeqDomain { def elementDomain = ZDomain3 }
      class Zs3 extends DiscreteSeqVariable { def domain = ZSeqDomain3 }
      while (documents2.size > 0) {
        val doc = documents2.last
        val doc3 = documents3.getOrElseUpdate(doc.name, { val d = new Document(WordSeqDomain, doc.name, Nil); d.zs = new Zs3; d })
        val ws = doc.ws; val zs = doc.zs
        var i = 0; val len = doc.length
        while (i < len) {
          val zi = doc.superTopic * numTopics + zs.intValue(i)
          val wi = ws.intValue(i)
          doc3.ws.appendInt(wi)
          doc3.zs.appendInt(zi)
          if (doc.breaks.contains(i)) doc3.breaks += (doc3.ws.length-1) // preserve phrase boundaries
          i += 1
        }
        documents2.remove(documents2.size-1) // Do this to enable garbage collection as we create more doc3's
      }
      documents1 = documents3.values.toSeq // Put them back into documents1 so that they can be written out below.
    }
    
    if (opts.writeDocs.wasInvoked) {
      println("\nWriting state to "+opts.writeDocs.value)
      val file = new File(opts.writeDocs.value)
      val pw = new PrintWriter(file)
      pw.println("/alphas")
      pw.println(Seq.fill(numTopics * numTopics)(opts.alpha.value).mkString(" ")) // Just set all alphas to 1.0 // TODO can we do better?
      pw.println()
      documents1.foreach(_.writeNameWordsZs(pw))
      pw.close()
    }
    
  }

  // Code for time-stamp parameterization
  // Related to Topics-over-Time [Wang, McCallum, KDD 2006]
  
  // These globals are set above in main opts.readDirs.wasInvoked
  var maxDate: Long = 0
  var minDate: Long = 0
  var dateRange: Double = 0.0

  /** Convert from Long doc.time to stamp falling in 0...1 range */
  def time2Stamp(t:Long): Double = {
    val result = (t - minDate) / dateRange
    assert(result >= 0.0, "input=%d minDate=%d dateRange=%g result=%g".format(t, minDate, dateRange, result))
    assert(result <= 1.0, result)
    result
  }
  /** Calculate Beta distribution parameters (alpha, beta) for the topicIndex. */
  def timeMeanAlphaBetaForTopic(documents:Iterable[Doc], topicIndex:Int): (Double, Double, Double, Double) = {
    val stamps = new util.DoubleArrayBuffer
    for (d <- documents; z <- d.zs.intValues; if z == topicIndex) {
      if (d.time < 0) throw new Error(d.name+" has year "+d.time)
      stamps += time2Stamp(d.time) 
    }
    val mean = maths.sampleMean(stamps)
    val variance = maths.sampleVariance(stamps, mean)
    //println("RecursiveLDA.timeMeanAlphaBeta min=%d max=%d range=%g".format(minDate, maxDate, dateRange))
    //println("RecursiveLDA.timeMeanAlphaBeta mean=%g variance=%g".format(mean, variance))
    (mean, variance, MaximizeBetaByMomentMatching.maxAlpha(mean, variance), MaximizeBetaByMomentMatching.maxBeta(mean, variance))
  }


}
