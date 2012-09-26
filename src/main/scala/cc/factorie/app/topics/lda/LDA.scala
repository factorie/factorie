/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords
import scala.collection.mutable.HashMap
import java.io.{PrintWriter, FileWriter, File, BufferedReader, InputStreamReader, FileInputStream}
import collection.mutable.{ArrayBuffer, HashSet, HashMap, LinkedHashMap}

/** Typical recommended value for alpha1 is 50/numTopics. */
class LDA(val wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, val beta1:Double = 0.01,
           val burnIn: Int = 100)(implicit val model:MutableGenerativeModel) {
  def this(numTopics:Int, alpha1:Double, beta1:Double, burnIn:Int) = this(new CategoricalSeqDomain[String], numTopics, alpha1, beta1, burnIn)(GenerativeModel())
  var diagnosticName = ""
  /** The per-word variable that indicates which topic it comes from. */
  object ZDomain extends DiscreteDomain(numTopics)
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs extends DiscreteSeqVariable {
    def this(initial:Seq[Int]) = { this(); this.appendInts(initial) }
    def this(initial:Array[Int]) = { this(); this.appendInts(initial) }
    def this(len:Int) = this(new Array[Int](len)) // relies on new Array being filled with 0s 
    def domain = ZSeqDomain
    //def words: Document = childFactors.first.asInstanceOf[PlatedDiscreteMixture.Factor]._1.asInstanceOf[Document]
  }

  def wordDomain = wordSeqDomain.elementDomain
  /** The prior over per-topic word distribution */
  val betas = MassesVariable.growableUniform(wordDomain, beta1)
  /** The prior over per-document topic distribution */
  val alphas = MassesVariable.dense(numTopics, alpha1)
  var maxDocSize:Int = 0
  var docLengthCounts: Array[Int] = null

  /** The collection of all documents used to fit the parameters of this LDA model. */
  private val documentMap = new LinkedHashMap[String,Doc] { def +=(d:Document): Unit = this(d.name) = d }
  def documents: Iterable[Doc] = documentMap.values
  def getDocument(name:String) : Doc = documentMap.getOrElse(name, null)
  def nameDocumentMap: scala.collection.Map[String,Doc] = documentMap
  /** The per-topic distribution over words.  FiniteMixture is a Seq of Dirichlet-distributed Proportions. */
  val phis = Mixture(numTopics)(ProportionsVariable.growableDense(wordDomain) ~ Dirichlet(betas))
  
  protected def setupDocument(doc:Doc, m:MutableGenerativeModel): Unit = {
    require(wordSeqDomain eq doc.ws.domain)
    require(doc.ws.length > 0)  // was > 1
    if (doc.theta eq null) doc.theta = ProportionsVariable.sortedSparseCounts(numTopics)
    else require (doc.theta.length == numTopics)
    doc.theta.~(Dirichlet(alphas))(m)
    if (doc.zs eq null) doc.zs = new Zs(Array.tabulate(doc.ws.length)(i => random.nextInt(numTopics))) // Could also initialize to all 0 for more efficient sparse inference
    else {
      require(doc.zs.length == doc.ws.length, "doc.ws.length=%d != doc.zs.length=%d".format(doc.ws.length, doc.zs.length))
      require(doc.zs.domain.elementDomain.size == numTopics, "zs.domain.elementDomain.size=%d != numTopics=%d".format(doc.zs.domain.elementDomain.size, numTopics))
    }
    doc.zs.~(PlatedDiscrete(doc.theta))(m)
    doc.ws.~(PlatedCategoricalMixture(phis, doc.zs))(m)
  }

  /** Add a document to the LDA model. */
  def addDocument(doc:Doc): Unit = {
    if (documentMap.contains(doc.name)) throw new Error(this.toString+" already contains document "+doc.name)
    setupDocument(doc, model)
    documentMap(doc.name) = doc
    maxDocSize = math.max(maxDocSize, doc.ws.length)
  }
  
  def removeDocument(doc:Doc): Unit = {
    documentMap.remove(doc.name)
    model -= model.parentFactor(doc.theta)
    model -= model.parentFactor(doc.zs)
    model -= model.parentFactor(doc.ws)
  }

  /** Infer doc.theta.  If the document is not already part of this LDA, do not add it and do not collapse anything that would effect this LDA. */
  def inferDocumentTheta(doc:Doc, iterations:Int = 10): Unit = {
    if (model.parentFactor(doc.ws) ne null) {
      val sampler = new CollapsedGibbsSampler(Seq(doc.theta), model)
      for (i <- 1 to iterations) sampler.process(doc.zs)
    } else {
      val m = GenerativeModel()
      setupDocument(doc, m)
      //println("LDA.inferDocumentTheta: model factors = "+m.allFactors)
      //println("LDA.inferDocumentTheta: zs factors = "+m.factors(Seq(doc.zs)))
      val sampler = new CollapsedGibbsSampler(Seq(doc.theta), m)
      for (i <- 1 to iterations) sampler.process(doc.zs)
    }
  }

  /** Run a collapsed Gibbs sampler to estimate the parameters of the LDA model. */
  def inferTopics(iterations:Int = 60, fitAlphaInterval:Int = Int.MaxValue, diagnosticInterval:Int = 10, diagnosticShowPhrases:Boolean = false): Unit = {
    val sampler = SparseLDAInferencer(ZDomain, wordDomain, documents, alphas.tensor, beta1, model)
    if(fitAlphaInterval != Int.MaxValue) {
      sampler.initializeHistograms(maxDocSize)
      docLengthCounts = Array.fill[Int](maxDocSize+1)(0)
      for (doc <- documents) docLengthCounts(doc.ws.length) += 1
    }

    println("Collapsing finished.  Starting sampling iterations:")
    //sampler.debug = debug
    val startTime = System.currentTimeMillis
    for (i <- 1 to iterations) {
      val timeToEstAlpha = (i % fitAlphaInterval == 0) && i > burnIn

      val startIterationTime = System.currentTimeMillis
      for (doc <- documents) sampler.process(doc.zs.asInstanceOf[Zs], timeToEstAlpha)
      val timeSecs = (System.currentTimeMillis - startIterationTime)/1000.0
      if (timeSecs < 2.0) print(".") else print("%.0fsec ".format(timeSecs)); Console.flush
      if (i % diagnosticInterval == 0) {
        println ("\n"+diagnosticName+"\nIteration "+i)
        sampler.export(phis)
        if (diagnosticShowPhrases) println(topicsWordsAndPhrasesSummary(10,10)) else println(topicsSummary(10))
      }
      /*if (i % fitAlphaInterval == 0) {
        sampler.exportThetas(documents)
        MaximizeDirichletByMomentMatching(alphas, model)
        sampler.resetSmoothing(alphas.tensor, beta1)
        println("alpha = " + alphas.tensor.toSeq.mkString(" "))
      }*/

      if (timeToEstAlpha){
        LearnDirichletUsingFrequencyHistograms(alphas, sampler.topicDocCounts, docLengthCounts)
        sampler.resetSmoothing(alphas.tensor, beta1)
        sampler.initializeHistograms(maxDocSize)
        //println("alpha = " + alphas.tensor.toSeq.mkString(" "))
      }
    } 
    //println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
    // Set original uncollapsed parameters to mean of collapsed parameters
    sampler.export(phis)
    sampler.exportThetas(documents)
  }
  
  // Not finished
  def inferTopicsMultithreaded(numThreads:Int, iterations:Int = 60, fitAlphaInterval:Int = Int.MaxValue, diagnosticInterval:Int = 10, diagnosticShowPhrases:Boolean = false): Unit = {
    if (fitAlphaInterval != Int.MaxValue) throw new Error("LDA.inferTopicsMultithreaded.fitAlphaInterval not yet implemented.")
    val docSubsets = documents.grouped(documents.size/numThreads + 1).toSeq
    //println("Subsets = "+docSubsets.size)
    for (i <- 1 to iterations) {
      docSubsets.par.foreach(docSubset => {
        val sampler = SparseLDAInferencer(ZDomain, wordDomain, documents, alphas.tensor, beta1, model)
        for (doc <- docSubset) sampler.process(doc.zs.asInstanceOf[Zs])
      })
      if (i % diagnosticInterval == 0) {
        println ("Iteration "+i)
        maximizePhisAndThetas
        if (diagnosticShowPhrases) println(topicsWordsAndPhrasesSummary(10,10)) else println(topicsSummary(10))
      }
    }
    maximizePhisAndThetas
  }
  
  def topicWords(topicIndex:Int, numWords:Int = 10): Seq[String] = phis(topicIndex).tensor.top(numWords).map(dp => wordDomain.category(dp.index))
  def topicWordsArray(topicIndex:Int, numWords:Int): Array[String] = topicWords(topicIndex, numWords).toArray
  def topicSummary(topicIndex:Int, numWords:Int = 10): String = "Topic %3d %s  %d  %f".format(topicIndex, (topicWords(topicIndex, numWords).mkString(" ")), phis(topicIndex).tensor.massTotal.toInt, alphas(topicIndex))
  def topicsSummary(numWords:Int = 10): String = Range(0, numTopics).map(topicSummary(_, numWords)).mkString("\n")

  def topicsPhraseCounts = new TopicPhraseCounts(numTopics) ++= documents
  def topicsWordsAndPhrasesSummary(numWords: Int = 10, numPhrases: Int = 10): String = {
    val sb = new StringBuffer
    val tpc = topicsPhraseCounts
    for (i <- 0 until numTopics) {
      sb.append(topicSummary(i, numWords))
      sb.append("\n           ") // Matching "Topic 333  ", plus one extra space for indentation
      val tp = tpc.topicPhrases(i, numPhrases)
      sb.append(tp.mkString(" "))
      sb.append("\n")
    }
    sb.toString
  }
  
  @deprecated("Will be removed eventually")
  def printTopics : Unit = {
    phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => wordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
    println
  }

  def maximizePhisAndThetas: Unit = {
    phis.foreach(_.tensor.zero())
    // TODO What about the priors on phis and theta?? -akm
    for (doc <- documents) {
      val len = doc.ws.length
      var i = 0
      while (i < len) {
        val zi = doc.zs.intValue(i)
        phis(zi).tensor.+=(doc.ws.intValue(i), 1.0)
        doc.theta.tensor.+=(zi, 1.0)
        i += 1
      }
    }
  }
  
  def saveWordsZs(file:File): Unit = {
    val pw = new PrintWriter(file)
    for (doc <- documents) doc.writeNameWordsZs(pw)
  }
  
  def addDocumentsFromWordZs(file:File, minDocLength:Int): Unit = {
    import scala.util.control.Breaks._
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    reader.mark(512)
    val alphasName = reader.readLine()
    if (alphasName == "/alphas") { // If they are present, read the alpha parameters.
      val alphasString = reader.readLine(); alphas.tensor := alphasString.split(" ").map(_.toDouble) // set lda.alphas
      reader.readLine() // consume delimiting newline
      println("Read alphas "+alphas.tensor.mkString(" "))
    } else reader.reset // Put the reader back to the read position when reader.mark was called
    breakable { while (true) {
      val doc = new Document(wordSeqDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
      doc.zs = new Zs(Nil)
      val numWords = doc.readNameWordsZs(reader)
      if (numWords < 0) break
      else if (numWords >= minDocLength) addDocument(doc) // Skip documents that have only one word because inference can't handle them
      //else System.err.println("addDocumentsFromWordZs skipping document %s: only %d words found.".format(doc.name, numWords))
    }}
    reader.close()
    maximizePhisAndThetas
  }

  @deprecated("Should be removed eventually")
  def saveModel(fileName:String) {
    val file = new File(fileName)
    val dir = file.getParentFile()
    if(!dir.exists()) dir.mkdirs()

    val fileWriter = new FileWriter(file);

    fileWriter.write(numTopics + "\n") // what else to write to first line? alpha/beta?  should save wordDomain

    for(doc <- documents) {
      fileWriter.write(doc.name)
      for(i <- 0 until doc.ws.length) fileWriter.write(" " + doc.ws(i) + " " + doc.zs.intValue(i))
      fileWriter.write("\n");
    }

    fileWriter.flush
    fileWriter.close
  }
}

object LDA extends LDACmd

class LDACmd {
  /*
  Potentail stop topics:
     -Topic  47 formal verification model specification methods systems software checking ada analysis
     -Topic 35 programming sigplan java generation implementation language comp programs sys design
     -Topic  83 ieee trans syst expert circuits systems eng esa appl computers  1  0.100000
   */
  import scala.collection.mutable.ArrayBuffer
  import scala.util.control.Breaks._
  import java.io.Reader
  import cc.factorie.app.strings.StringSegmenter
  var verbose = false
  val minDocLength = 3
  def newDocument(domain:CategoricalSeqDomain[String], name:String, contents:Reader, segmenter:StringSegmenter): Doc = Document.fromReader(domain, name, contents, segmenter) 

  def main(args:Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val numTopics =     new CmdOption("num-topics", 't', 10, "N", "Number of topics.")
      val alpha =         new CmdOption("alpha", 0.1, "N", "Dirichlet parameter for per-document topic proportions.")
      val beta =          new CmdOption("beta", 0.01, "N", "Dirichlet parameter for per-topic word proportions.")
      val numThreads =    new CmdOption("num-threads", 1, "N", "Number of threads for multithreaded topic inference.")
      val numIterations = new CmdOption("num-iterations", 'i', 50, "N", "Number of iterations of inference.")
      val diagnostic =    new CmdOption("diagnostic-interval", 'd', 10, "N", "Number of iterations between each diagnostic printing of intermediate results.")
      val diagnosticPhrases= new CmdOption("diagnostic-phrases", false, "true|false", "If true diagnostic printing will include multi-word phrases.")
      val fitAlpha =      new CmdOption("fit-alpha-interval", Int.MaxValue, "N", "Number of iterations between each re-estimation of prior on per-document topic distribution.")
      val optimizeBurnIn =new CmdOption("optimize-burn-in", 100, "N", "Number of iterations to run before the first estimation of the alpha parameters")
      val tokenRegex =    new CmdOption("token-regex", "\\p{Alpha}+", "REGEX", "Regular expression for segmenting tokens.")
      val readDirs =      new CmdOption("read-dirs", List(""), "DIR...", "Space-(or comma)-separated list of directories containing plain text input files.")
      val readLines =     new CmdOption("read-lines", "", "FILENAME", "File containing lines of text, one for each document.")
      val readLinesRegex= new CmdOption("read-lines-regex", "", "REGEX", "Regular expression with parens around the portion of the line that should be read as the text of the document.")
      val readLinesRegexGroups= new CmdOption("read-lines-regex-groups", List(1), "GROUPNUMS", "The --read-lines-regex group numbers from which to grab the text of the document.")
      val readLinesRegexPrint = new CmdOption("read-lines-regex-print", false, "BOOL", "Print the --read-lines-regex match that will become the text of the document.")
      val writeDocs =     new CmdOption("write-docs", "lda-docs.txt", "FILENAME", "Save LDA state, writing document names, words and z assignments") 
      val readDocs =      new CmdOption("read-docs", "lda-docs.txt", "FILENAME", "Add documents from filename, reading document names, words and z assignments; store documents; can then add more documents or do more inference.") 
      val readPhis =      new CmdOption("read-phis", "lda-docs.txt", "FILENAME", "Read documents from filename, but only use them to increment topic word counts; does not store documents (conserving memory); cannot do more inference, nor print phrases") { override def invoke = { numIterations.value = 0; numIterations.defaultValue = 0 } }
      val maxNumDocs =    new CmdOption("max-num-docs", Int.MaxValue, "N", "The maximum number of documents to read.")
      val printTopics =   new CmdOption("print-topics", 20, "N", "Just before exiting print top N words for each topic.")
      val printPhrases =  new CmdOption("print-topics-phrases", 20, "N", "Just before exiting print top N phrases for each topic.")
      val thetaServer   = new CmdOption("theta-server", 50, "N", "Read from sdin newline-separated documents, and output a theta topic distribution for each, estimated by N iterations of sampling on the document.")
      val verbose =       new CmdOption("verbose", "Turn on verbose output") { override def invoke = LDACmd.this.verbose = true }
      // TODO Add stopwords option
      // TODO Add option to save alphas somewhere, perhaps first line of the writeDocs file, like a document containing only numbers (with extra newline at the end):
      // alphas
      // 0.23 0.15 0.62
      // 
    }
    opts.parse(args)
    /** The domain of the words in documents */
    object WordSeqDomain extends CategoricalSeqDomain[String]
    val model = GenerativeModel()
    val lda = new LDA(WordSeqDomain, opts.numTopics.value, opts.alpha.value, opts.beta.value, opts.optimizeBurnIn.value)(model)
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
    if (opts.readDocs.wasInvoked) {
      val file = new File(opts.readDocs.value)
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
      reader.mark(512)
      val alphasName = reader.readLine()
      if (alphasName == "/alphas") { // If they are present, read the alpha parameters.
        val alphasString = reader.readLine(); lda.alphas.tensor := alphasString.split(" ").map(_.toDouble) // set lda.alphas
        reader.readLine() // consume delimiting newline
        println("Read alphas "+lda.alphas.tensor.mkString(" "))
      } else reader.reset // Put the reader back to the read position when reader.mark was called
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
    }
    if (opts.readPhis.wasInvoked) {
      val file = new File(opts.readPhis.value)
      val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
      val alphasName = reader.readLine(); if (alphasName != "/alphas") throw new Error("/alphas not found")
      val alphasString = reader.readLine(); lda.alphas.tensor := alphasString.split(" ").map(_.toDouble) // set lda.alphas
      reader.readLine() // consume delimiting newline
      println("Read alphas "+lda.alphas.tensor.mkString(" "))
      breakable { while (true) {
        if (lda.documents.size == opts.maxNumDocs.value) break
        val doc = new Document(WordSeqDomain, "", Nil) // doc.name will be set in doc.readNameWordsZs
        doc.zs = new lda.Zs(Nil)
        val numWords = doc.readNameWordsZs(reader)
        if (numWords < 0) break
        else if (numWords >= minDocLength) {
          val len = doc.ws.length
          var i = 0
          while (i < len) {
            val zi = doc.zs.intValue(i)
            lda.phis(zi).tensor.+=(doc.ws.intValue(i), 1.0)
            i += 1
          }
        } else System.err.println("--read-docs skipping document %s: only %d words found.".format(doc.name, numWords))  // Skip documents that have only one word because inference can't handle them
      }}
      reader.close()
    }
    else if (lda.documents.size == 0) { System.err.println("You must specific either the --input-dirs or --input-lines options to provide documents."); System.exit(-1) }
    println("\nRead "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.ws.length).sum+" word tokens.")
    
    // Run inference to discover topics
    if (opts.numIterations.value > 0) {
      val startTime = System.currentTimeMillis
      if (opts.numThreads.value > 1) 
       lda.inferTopicsMultithreaded(opts.numThreads.value, opts.numIterations.value, diagnosticInterval = opts.diagnostic.value, diagnosticShowPhrases = opts.diagnosticPhrases.value) 
      else 
        lda.inferTopics(opts.numIterations.value, fitAlphaInterval = opts.fitAlpha.value, diagnosticInterval = opts.diagnostic.value, diagnosticShowPhrases = opts.diagnosticPhrases.value)
      println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  	}	

    //testSaveLoad(lda)
    //println("topics.LDA temporary test")
    //val doc1 = lda.documents.head
    //lda.removeDocument(doc1)
    //lda.inferDocumentTheta(doc1)
    //println(doc1.ws.categoryValues.take(10).mkString(" "))
    //println(doc1.theta)
    
    if (opts.writeDocs.wasInvoked) {
      val file = new File(opts.writeDocs.value)
      val pw = new PrintWriter(file)
      pw.println("/alphas")
      pw.println(lda.alphas.tensor.mkString(" "))
      pw.println()
      lda.documents.foreach(_.writeNameWordsZs(pw))
      pw.close()
    }
    
    if (opts.printTopics.wasInvoked) 
      println(lda.topicsSummary(opts.printTopics.value))
    if (opts.printPhrases.wasInvoked) 
      println(lda.topicsWordsAndPhrasesSummary(opts.printPhrases.value, opts.printPhrases.value))
      //println(lda.topicsPhraseCounts.topicsPhrasesSummary(opts.printPhrases.value))

    if (opts.thetaServer.wasInvoked) {
      lda.wordSeqDomain.elementDomain.freeze
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
      println("Reading documents, one per line.  To end, close by Control-D")
      var line = reader.readLine
      var count = 0
      while (line ne null) {
        val doc = Document.fromString(lda.wordSeqDomain, "<stdin>"+count, line)
        count += 1
        lda.inferDocumentTheta(doc, opts.thetaServer.value)
        println(doc.theta.tensor.mkString(" "))
        line = reader.readLine
      }
    }
  }

  @deprecated("Will be removed")
  def loadModel(fileName:String, wordSeqDomain:CategoricalSeqDomain[String] = new CategoricalSeqDomain[String]) : LDA = {
    val file = new File(fileName)
    if(!file.exists()) return null;

    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines()
    if(!lines.hasNext) new Error("File " + fileName + " had 0 lines")

    val startTime = System.currentTimeMillis()

    var line = lines.next()
    val numTopics = java.lang.Integer.parseInt(line.trim()) // first line has non-document details

    println("loading model with " + numTopics + " topics")

    val lda = new LDA(wordSeqDomain, numTopics)(GenerativeModel()) // do we have to create this here?  problem because we don't have topics/alphas/betas/etc beforehand to create LDA instance
    while(lines.hasNext) {
      line = lines.next()
      var tokens = new ArrayBuffer[String]
      var topicAssignments = new ArrayBuffer[Int]
      val fields = line.split(" +")

      assert(fields.length >= 3) // at least 1 token

      val docName = fields(0)
      for(i <- 1 until fields.length by 2) { // grab each pair of token/count
        tokens += fields(i)
        topicAssignments += java.lang.Integer.parseInt(fields(i+1))
      }

      val doc = Document.fromStringIterator(wordSeqDomain, docName, tokens.iterator, stopwords = cc.factorie.app.strings.EmptyStringSet) // create and add document
      lda.addDocument(doc)
      for(i <- 0 until doc.length) // put z's to correct values we found in loaded file
        doc.zs.set(i, topicAssignments(i))(null)

      //for(i <- 0 until doc.length) Console.print(doc(i) + " " + doc.zs.intValue(i) + " ")
      //Console.println("");
    }

    lda.maximizePhisAndThetas

    println("Load file time = " + (System.currentTimeMillis() - startTime)/1000.0 + " seconds")

    return lda
  }

  /*def testSaveLoad(lda:LDA) {
    val testLoc = "/Users/kschultz/dev/backedup/models/ldatestsave/ldatestsave"
    lda.saveModel(testLoc)

    val testLoad = loadModel(testLoc)
    val testLoadSameDomain = loadModel(testLoc, lda.wordSeqDomain)

    Console.println("Topics from pre-save model: \n")
    lda.printTopics; //debugTopics(lda)

    Console.println("**********************************\n")

    Console.println("Topics from loaded model (SAME WordSeqDomain): \n")
    testLoadSameDomain.printTopics; // debugTopics(testLoadSameDomain)

    Console.println("**********************************\n")

    Console.println("Topics from loaded model (NEW WordSeqDomain): \n")
    testLoad.printTopics; // debugTopics(testLoad)
    
    verifyPhis(lda, testLoad)
  }

  def verifyPhis(lda1:LDA, lda2:LDA) : Unit = {
    val topicPlusWordToCountMap = new HashMap[String, Double]
    
    for(t <- lda1.phis) {
      val topicId = lda1.phis.indexOf(t)
      for(i <- 0 until t.length) {
        val word = lda1.wordDomain.getCategory(i)
        val count = t.counts(i)
        topicPlusWordToCountMap(topicId + "_" + word) = count
      }
    }


    for(t2 <- lda2.phis) {
      val topicId = lda2.phis.indexOf(t2)
      for(i <- 0 until t2.length) {
        val word = lda2.wordDomain.getCategory(i)
        val count = t2.counts(i)
        if(topicPlusWordToCountMap(topicId + "_" + word) != count) Console.err.println("failed to match count for topic " + topicId + " and word " + word + " with count " + count)
      }
    }
  }

  def debugTopics(lda:LDA) {
    lda.phis.foreach(t => {
      print("Topic " + lda.phis.indexOf(t) + "  ");
      t.top(20).zipWithIndex.foreach(dp => print(dp._2 + "=" + lda.wordDomain.getCategory(dp._1.index) + "(" + t.counts(dp._1.index) + ", " + dp._1.index + ") "));
      println
    })
    println
  }*/
}

