package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._
import scala.collection.mutable.HashMap
import java.io.{FileWriter, File}

class LDA(val wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, val beta1:Double = 0.1)(implicit val model:GenerativeModel = defaultGenerativeModel) {
  /** The per-word variable that indicates which topic it comes from. */
  object ZDomain extends DiscreteDomain { def size = numTopics }
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(intValues:Seq[Int]) extends PlatedGateVariable(intValues) {
    def this(len:Int) = this(Seq.fill(len)(0))
    def domain = ZSeqDomain
    //def words: Document = childFactors.first.asInstanceOf[PlatedDiscreteMixture.Factor]._1.asInstanceOf[Document]
  }

  def wordDomain = wordSeqDomain.elementDomain
  /** The prior over per-topic word distribution */
  val betas = new GrowableUniformMasses(wordDomain, beta1)
  /** The prior over per-document topic distribution */
  val alphas = new DenseMasses(numTopics, alpha1)

  /** The collection of all documents used to fit the parameters of this LDA model. */
  private val documentMap = new HashMap[String,DocumentVar] { def +=(d:Document): Unit = this(d.name) = d }
  def documents: Iterable[DocumentVar] = documentMap.values
  /** The per-topic distribution over words.  FiniteMixture is a Seq of Dirichlet-distributed Proportions. */
  val phis = Mixture(numTopics)(new GrowableDenseCountsProportions(wordDomain) ~ Dirichlet(betas))

  /** Add a document to the LDA model. */
  def addDocument(doc:DocumentVar): Unit = {
    require(wordSeqDomain eq doc.domain)
    require(doc.length > 1)
    doc.theta = new SortedSparseCountsProportions(numTopics) ~ Dirichlet(alphas) // was DenseCountsProportions
    doc.zs = new Zs(doc.length) :~ PlatedDiscrete(doc.theta) // TODO Consider making this :~ because now all words start in topic 0!
    doc ~ PlatedDiscreteMixture(phis, doc.zs)
    documentMap(doc.name) = doc
  }
  
  def removeDocument(doc:DocumentVar): Unit = {
    defaultGenerativeModel -= defaultGenerativeModel.parentFactor(doc.theta)
    defaultGenerativeModel -= defaultGenerativeModel.parentFactor(doc.zs)
    defaultGenerativeModel -= defaultGenerativeModel.parentFactor(doc)
    //throw new Error("Not yet implemeneted")
  }

  /** Like addDocument, but does not register doc.theta or doc as children this.alphas or this.phis.
      Useful for a temporary connection for inference of doc.theta and doc.zs. */
  /*protected def connectDocument(doc:DocumentVar): Unit = {
    doc.theta = new SortedSparseCountsProportions(numTopics) ~< Dirichlet(alphas) // was DenseCountsProportions
    val zs = new Zs(doc.length) :~ PlatedDiscrete(doc.theta)
    doc ~< PlatedDiscreteMixture(phis, zs)
  }*/

  def inferDocumentTheta(doc:DocumentVar, iterations:Int = 10): Unit = {
    var tmp = false
    if (model.parentFactor(doc) eq null) { addDocument(doc); tmp = true }
    val sampler = new CollapsedGibbsSampler(Seq(doc.theta))
    for (i <- 1 to iterations) sampler.process(doc.zs)
    if (tmp) removeDocument(doc)
  }

    /** Run a collapsed Gibbs sampler to estimate the parameters of the LDA model. */
  def inferTopics(iterations:Int = 60, diagnosticInterval:Int = 10, fitAlphaInterval:Int = Int.MaxValue): Unit = {
    val sampler = new SparseLDAInferencer(ZDomain, wordDomain, documents, alphas, beta1)
    println("Collapsing finished.  Starting sampling iterations:")
    //sampler.debug = debug
    val startTime = System.currentTimeMillis
    for (i <- 1 to iterations) {
      val startIterationTime = System.currentTimeMillis
      for (doc <- documents) sampler.process(doc.zs.asInstanceOf[Zs])
      val timeSecs = (System.currentTimeMillis - startIterationTime)/1000.0
      if (timeSecs < 2.0) print(".") else print("%.0fsec ".format(timeSecs)); Console.flush
      if (i % diagnosticInterval == 0) {
        println ("\nIteration "+i)
        sampler.export(phis)
        printTopics
      }
      if (i % fitAlphaInterval == 0) {
        sampler.exportThetas(documents)
        DirichletMomentMatching.estimate(alphas)
        sampler.resetSmoothing(alphas, beta1)
        println("alpha = " + alphas.mkString(" "))
      }
    } 
    //println("Finished in "+((System.currentTimeMillis-startTime)/1000.0)+" seconds")
    // Set original uncollapsed parameters to mean of collapsed parameters
    sampler.export(phis)
    sampler.exportThetas(documents)
  }
  
  // Not finished
  def inferTopicsMultithreaded(numThreads:Int, iterations:Int = 60, diagnosticInterval:Int = 10): Unit = {
    val docSubsets = documents.grouped(documents.size/numThreads + 1).toSeq
    //println("Subsets = "+docSubsets.size)
    for (i <- 1 to iterations) {
      docSubsets.par.foreach(docSubset => {
        val sampler = new SparseLDAInferencer(ZDomain, wordDomain, documents, alphas, beta1)
        for (doc <- docSubset) sampler.process(doc.zs.asInstanceOf[Zs])
      })
      if (i % diagnosticInterval == 0) {
        println ("Iteration "+i)
        maximizePhisAndThetas
        printTopics
      }
    }
    maximizePhisAndThetas
  }

  def printTopics : Unit = {
    phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => wordDomain.getCategory(dp.index)).mkString(" ")+"  "+t.countsTotal.toInt+"  "+alphas(phis.indexOf(t))))
    println
  }

  def maximizePhisAndThetas: Unit = {
    phis.foreach(_.zero())
    for (doc <- documents; i <- 0 until doc.length) {
      val zi = doc.zs.intValue(i)
      phis(zi).increment(doc.intValue(i), 1.0)(null)
      doc.theta.increment(zi, 1.0)(null)
    }
  }

  def saveModel(fileName:String) {
    val file = new File(fileName)
    val dir = file.getParentFile()
    if(!dir.exists()) dir.mkdirs()

    val fileWriter = new FileWriter(file);

    fileWriter.write(numTopics + "\n") // what else to write to first line? alpha/beta?  should save wordDomain

    for(doc <- documents) {
      fileWriter.write(doc.name)
      for(i <- 0 until doc.length) fileWriter.write(" " + doc(i) + " " + doc.zs.intValue(i))
      fileWriter.write("\n");
    }

    fileWriter.flush
    fileWriter.close
  }
}


object LDA {
  import scala.collection.mutable.ArrayBuffer
  var verbose = false
  def main(args:Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val numTopics =     new CmdOption("num-topics", "N", 10, "Number of topics.")
      val numThreads =    new CmdOption("num-threads", "N", 1, "Number of threads for multithreaded topic inference.")
      val numIterations = new CmdOption("num-iterations", "N", 20, "Number of iterations of inference.")
      val diagnostic =    new CmdOption("diagnostic-interval", "N", 10, "Number of iterations between each diagnostic printing of intermediate results.")
      val inputDirs =     new CmdOption("input-dirs", "DIR,DIR", List(""), "Comma-separated list of directories containing plain text input files.")
      val inputLines =    new CmdOption("input-lines", "FILE", "", "File containing lines of text, one for each document.")
      val verbose =       new CmdOption("verbose", "Turn on verbose output") { override def invoke = LDA.this.verbose = true }
    }
    opts.parse(args)
    /** The domain of the words in documents */
    object WordSeqDomain extends CategoricalSeqDomain[String]
    val lda = new LDA(WordSeqDomain, opts.numTopics.value)
    if (opts.inputDirs.wasInvoked) {
      for (directory <- opts.inputDirs.value) {
        val dir = new File(directory); if (!dir.isDirectory) { System.err.println(directory+" is not a directory."); System.exit(-1) }
        println("Reading files from directory " + directory)
        for (file <- new File(directory).listFiles; if (file.isFile)) {
          val doc = Document(WordSeqDomain, file, "UTF-8")
          if (doc.length > 3) lda.addDocument(doc)
          //print("."); Console.flush
        }
        //println()
      }
    } 
    if (opts.inputLines.wasInvoked) {
      val file = new File(opts.inputLines.value)
      val source = scala.io.Source.fromFile(file)
      val name = file.getName
      var count = 0
      for (line <- source.getLines()) {
        val doc = Document(WordSeqDomain, name+":"+count, line)
        if (doc.length > 3) lda.addDocument(doc)
        count += 1
        if (count % 1000 == 0) { print(" "+count); Console.flush() }; if (count % 10000 == 0) println()
      }
    }
    if (lda.documents.size == 0) { System.err.println("You must specific either the --input-dirs or --input-lines options to provide documents."); System.exit(-1) }
    println("Read "+lda.documents.size+" documents, "+WordSeqDomain.elementDomain.size+" word types, "+lda.documents.map(_.length).sum+" word tokens.")
    
    val startTime = System.currentTimeMillis
    if (opts.numThreads.value > 1) 
      lda.inferTopicsMultithreaded(opts.numThreads.value, opts.numIterations.value, opts.diagnostic.value) 
    else 
      lda.inferTopics(opts.numIterations.value, opts.diagnostic.value)
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
    
    /*for (doc1 <- lda.documents.take(20)) {
      println(doc1.ws.map(dv => WordSeqDomain.elementDomain.getCategory(dv.intValue)).mkString(" "))
      println(doc1.theta)
      lda.removeDocument(doc1)
      doc1.theta.zero()
      lda.inferDocumentTheta(doc1, 50)
      println(doc1.theta)
      println()
    }*/
  }

  def loadModel(fileName:String, wordSeqDomain:CategoricalSeqDomain[String] = new CategoricalSeqDomain[String]) : LDA = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines()
    if(!lines.hasNext) new Error("File " + fileName + " had 0 lines")

    var line = lines.next()
    val numTopics = java.lang.Integer.parseInt(line.trim()) // first line has non-document details

    val lda = new LDA(wordSeqDomain, numTopics) // do we have to create this here?  problem because we don't have topics/alphas/betas/etc beforehand to create LDA instance
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

      val doc = Document(wordSeqDomain, docName, tokens.iterator) // create and add document
      lda.addDocument(doc)
      for(i <- 0 until doc.length) // put z's to correct values we found in loaded file
        doc.zs.set(i, topicAssignments(i))(null)

      //for(i <- 0 until doc.length) Console.print(doc(i) + " " + doc.zs.intValue(i) + " ")
      //Console.println("");
    }

    lda.maximizePhisAndThetas

    return lda
  }

  def testSaveLoad(lda:LDA) {
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
  }
}