package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._
import scala.collection.mutable.HashMap
import java.io.File

class LDA(val wordSeqDomain: CategoricalSeqDomain[String], numTopics: Int = 10, alpha1:Double = 0.1, val beta1:Double = 0.1) {
  /** The per-word variable that indicates which topic it comes from. */
  object ZDomain extends DiscreteDomain { def size = numTopics }
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(intValues:Seq[Int]) extends PlatedGate(intValues) {
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
    val zs = new Zs(doc.length) :~ PlatedDiscrete(doc.theta) // TODO Consider making this :~ because now all words start in topic 0!
    doc ~ PlatedDiscreteMixture(phis, zs)
    documentMap(doc.name) = doc
  }

  /** Like addDocument, but does not register doc.theta or doc as children this.alphas or this.phis.
      Useful for a temporary connection for inference of doc.theta and doc.zs. */
  protected def connectDocument(doc:DocumentVar): Unit = {
    doc.theta = new SortedSparseCountsProportions(numTopics) ~< Dirichlet(alphas) // was DenseCountsProportions
    val zs = new Zs(doc.length) :~ PlatedDiscrete(doc.theta)
    doc ~< PlatedDiscreteMixture(phis, zs)
  }

  def inferDocumentTheta(doc:Document, iterations:Int = 10): Unit = {
    if (doc.parentFactor eq null) connectDocument(doc)
    val sampler = new CollapsedGibbsSampler(Seq(doc.theta))
    for (i <- 1 to iterations) sampler.process(doc.zs)
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
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => wordDomain.getCategory(dp.index)).mkString(" ")+"  "+t.countsTotal.toInt+"  "+alphas(phis.indexOf(t))))
        println
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
        phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.top(10).map(dp => wordDomain.getCategory(dp.index)).mkString(" ")+"  "+t.countsTotal.toInt+"  "+alphas(phis.indexOf(t))))
        println
      }
    }
    maximizePhisAndThetas
  }

  def maximizePhisAndThetas: Unit = {
    phis.foreach(_.zero())
    for (doc <- documents; i <- 0 until doc.length) {
      val zi = doc.zs.intValue(i)
      phis(zi).increment(doc.intValue(i), 1.0)(null)
      doc.theta.increment(zi, 1.0)(null)
    }
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
  }
}