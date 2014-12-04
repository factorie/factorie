package cc.factorie.app.nlp.embedding
import cc.factorie.variable.CategoricalDomain
import cc.factorie.model._
import cc.factorie.la._
import cc.factorie.optimize._
import cc.factorie.util.{Alias, IntArrayBuffer, DoubleAccumulator}
import scala.util.Random
import java.io._
import scala.collection.mutable.{ArrayOps,ArrayBuffer}
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream

class WindowWordEmbedderOptions extends cc.factorie.util.CmdOptions {
  val input = new CmdOption("input", List("enwiki-latest-pages-articles.xml.bz2"), "TXTFILE", "Text files from which to read training data.  Works with *.txt.gz and Wikipedia enwiki*.xmlgz2.")
  val dims = new CmdOption("dims", 50, "INT", "Dimensionality of the embedding vectors.")
  //val margin = new CmdOption("margin", 0.1, "DOUBLE", "Margin for WSABIE training.")
  //val loss = new CmdOption("loss", "wsabie", "STRING", "Loss function; options are wsabie and log.")
  val seed = new CmdOption("seed", 0, "INT", "Seed for random number generator.")
  val minContext = new CmdOption("min-context", 2, "INT", "Skip training windows that end up with fewer context words after randomized removal of common context words.")
  val window =  new CmdOption("window", 5, "INT", "The number of words on either side of the target to include in the context window.")
  val normalizeX = new CmdOption("normalize-x", false, "BOOL", "If true, normalize input context by the number of context words in the training example.")
  val vocabulary = new CmdOption("vocabulary", "vocabulary.txt", "FILE", "Filename in which to find the words, one per line, each preceded by its count.")
  val negative = new CmdOption("negative", 3, "INT", "The number of NCE negative examples to use for each positive example.")
  val maxWikiPages = new CmdOption("max-wiki-pages", Int.MaxValue, "INT", "Read no more than this number of Wikipedia pages.  Default is Int.MaxValue.")
  val separateIO = new CmdOption("separate-io", false, "BOOLEAN", "If TRUE, parameterize input embeddings (U) separately from output embeddings (V).  Default is FALSE.")
  val checkGradient = new CmdOption("check-gradient", false, "BOOLEAN", "If TRUE, test the value/gradient calculation for every parameter for every example after the first 50000 example.  (Slow.)  Default is FALSE.")
  val outputExamples = new CmdOption("output-examples", "examples.txt.gz", "FILE", "Save the training targets/contexts in this file, one per line.")
  val useAliasSampling = new CmdOption("alias-sampling", false, "BOOLEAN", "Sample negative examples using alias sampling vs. power-law approximation.")
  val powerForCounts = new CmdOption("power-for-counts", 0.75, "DOUBLE", "Power to raise counts to when computing proportions for exact alias sampling.")
}

trait WindowWordEmbedderExample extends Example {
  def inputIndices: Array[Int]
  def outputIndices: Array[Int]
  def changedWeights: ArrayBuffer[Weights]
}

abstract class WordEmbedder(val opts:WindowWordEmbedderOptions) extends Parameters {
  val dims = opts.dims.value
  val random = new Random(opts.seed.value)
  val domain = new CategoricalDomain[String]
  lazy val sampler = new cc.factorie.util.Alias(domain.counts.asArray.map(_.toDouble).map(math.pow(_, opts.powerForCounts.value)))(random)
  def makeNegativeSamples: Array[Int] =
    if (opts.useAliasSampling.value) {
      val len = opts.negative.value
      val ret = new Array[Int](len)
      var i = 0
      while (i < len) {
        ret(i) = sampler.sample()
        i += 1
      }
      ret
    } else {
      val len = opts.negative.value
      val ret = new Array[Int](len)
      var i = 0
      while (i < len) {
        var r = random.nextDouble()
        r = r * r * r // Rely on fact that domain is ordered by frequency, so we want to over-sample the earlier entries
        ret(i) = (r * domain.size).toInt // TODO Make this better match a Ziph distribution!
        i += 1
      }
      ret
    }
  // Read in the vocabulary 
  for (splitLine <- io.Source.fromFile(opts.vocabulary.value).getLines().map(_.split(' '))) domain.indexWithCount(splitLine(1), splitLine(0).toInt)
  domain.freeze()
  println("Vocabulary size "+domain.size)
  println("Vocabulary total count "+domain.countsTotal)
  // Initialize both input and output embedding vectors
  private val _inputEmbedding = Array.fill(domain.size)(Weights(new DenseTensor1(dims).fill(() => random.nextDouble()/dims/10 - 0.5/dims/10))) // TODO How should vectors be initialized? /10 good?
  def inputEmbedding(i:Int) = _inputEmbedding(i).value
  def inputWeights(i:Int) = _inputEmbedding(i)
  def inputEmbedding(word:String) = { val index = domain.index(word); if (index >= 0) _inputEmbedding(index).value else null }
  private val _outputEmbedding = if (opts.separateIO.value) Array.fill(domain.size)(Weights(new DenseTensor1(dims).fill(() => random.nextDouble()/dims/10 - 0.5/dims/10))) else _inputEmbedding // TODO How should vectors be initialized? /10 good?
  def outputEmbedding(i:Int) = _outputEmbedding(i).value
  def outputWeights(i:Int) = _outputEmbedding(i)
  def outputEmbedding(word:String) = { val index = domain.index(word); if (index >= 0) _outputEmbedding(index).value else null }
  
  private val _discardProb = new Array[Double](domain.size)
  def discardProb(wordIndex: Int): Double = {
    var result = _discardProb(wordIndex)
    if (result != 0.0) return result
    result = 1.0 - math.sqrt(0.0001/(domain.count(wordIndex).toDouble / domain.countsTotal.toDouble))
    _discardProb(wordIndex) = result
    result
  }
  def discard(wordIndex:Int): Boolean = random.nextDouble() < discardProb(wordIndex)
  
  private def stringToWordIndices(string:String): cc.factorie.util.IntArrayBuffer = {
    val wordIndices = new cc.factorie.util.IntArrayBuffer(string.length/4) // guessing average word length > 4
    for (word <- Vocabulary.stringToWords(string)) {
      val wordIndex = domain.index(word)
      if (wordIndex >= 0) wordIndices += wordIndex
    }
    wordIndices
  }
  private def stringToExamples(string:String): Iterable[WindowWordEmbedderExample] = {
    val wordIndices = stringToWordIndices(string)
    if (wordIndices.length > opts.window.value) { // TODO was > 2*opts.window.value
      val array = wordIndices._rawArray
      val windowSize = opts.window.value
      // this is slow
      for (targetPosition <- 0 until wordIndices.length - windowSize; example <- newExample(this, array, targetPosition, 1 + random.nextInt(windowSize))) yield example
    } else Nil
  }
  def newExample(model:WordEmbedder, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[WindowWordEmbedderExample]
  
  def train(filenames:Seq[String]): Unit = {
    if (opts.outputExamples.wasInvoked) println("Writing data examples to "+opts.outputExamples.value)
    var wordCount = 0
    val wordCountLogIncrement = 1000000; var nextWordCountLog = wordCountLogIncrement
    val wordCountSnapshotIncrement = 10000000; var nextWordCountShapshot = wordCountSnapshotIncrement
    val optimizer = new AdaGrad()
    optimizer.initializeWeights(this.parameters)
    //val trainer = new HogwildTrainer(parameters, optimizer, logEveryN=50000)
    //val trainer = new cc.factorie.app.nlp.embeddings.LiteHogwildTrainer(parameters, optimizer)
    val trainer = new OnlineTrainer(parameters, optimizer, logEveryN=50000)
    for (filename <- filenames; string <- Vocabulary.fileToStringIterator(new File(filename))) {
      val examples = stringToExamples(string)
      //println("CBOW.train examples.size = "+examples.size)
      wordCount += examples.size
      if (opts.checkGradient.value && wordCount >= 10000) {
        print(s"CBOW testGradient ${examples.map(e => e.outputIndices.map(domain.category).mkString(" ")).mkString(" ")} ...")
        examples.foreach(e => Example.testGradient(parameters, parameters.keys, e, dx = 1e-7, verbose = true, returnOnFirstError = false)) // TODO Put back "assert"
        println("finished.")
      }
      trainer.processExamples(examples)
      if (wordCount > nextWordCountLog) { println(s"Trained on ${wordCount/1000000}m contexts."); nextWordCountLog += wordCountLogIncrement } 
      if (wordCount > nextWordCountShapshot && !opts.outputExamples.wasInvoked) {
        val fn = f"embeddings-${wordCount/1000000}%04dm"
        println("Writing intermediate embeddings to "+fn)
        writeInputEmbeddings(fn)
        nextWordCountShapshot += wordCountSnapshotIncrement
      }
    }
  }

  /** Return a ranked list of domain index/word by their gated output vector's similarity the query vector.
    The query vector is assumed to be already gated.  The output vectors are gated on the fly here. */
  def neighbors(query:DenseTensor1, count:Int = 10): cc.factorie.util.TopN[String] = {
    val top = new cc.factorie.util.TopN[String](count)
    for (i <- 0 until domain.size) top += (i, query.cosineSimilarity(_outputEmbedding(i).value), domain.category(i))
    top
  }
  
  /** Interactive browsing of nearest neighbors */
  def browse(): Unit = {
    var neighborCount = 10
    def printPrompt(): Unit = { print(s"\n$neighborCount> "); System.out.flush() }
    printPrompt()
    for (line <- io.Source.stdin.getLines()) {
      val query = new DenseTensor1(dims)
      var queryTerms = 0
      for (word <- line.split("\\s+")) word match {
        case "\\d+" => neighborCount = word.toInt
        case _ => {
          val index = domain.index(word)
          if (index < 0) println(s"'$word' is outside vocabulary")
          else { query += inputEmbedding(index); queryTerms += 1 }
        }
      }
      if (queryTerms > 0) {
        val top = neighbors(query, neighborCount)
        for (entry <- top) println(f"${entry.category}%-25s ${entry.score}%+08f")
      }
      printPrompt()
    }    
  }
  
    
  def writeExamples(inputFilenames:Array[String]): Unit = {
    val dataWriter = if (opts.outputExamples.wasInvoked) new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(opts.outputExamples.value)), "UTF-8") else null
    var examplesWrittenCount = 0
    for (filename <- inputFilenames; string <- Vocabulary.fileToStringIterator(new File(filename)); example <- stringToExamples(string)) {
      examplesWrittenCount += 1
      dataWriter.write(s"$examplesWrittenCount\t${example.outputIndices.map(domain.category(_)).mkString(" ")}\t${example.inputIndices.map(i => domain.category(i)).mkString("\t")}\n")
    }
    dataWriter.close()
  }

  // Writing to a file embeddings in textual format
  def writeInputEmbeddings(filename:String): Unit = {
    val out = new PrintWriter(filename)
    for (i <- 0 until domain.size)
      out.println("%s\t%s".format(domain.category(i), inputEmbedding(i).mkString(" ")))
    out.close()
  }
  def writeOutputEmbeddings(filename:String): Unit = {
    val out = new PrintWriter(filename)
    for (i <- 0 until domain.size)
      out.println("%s\t%s".format(domain.category(i), outputEmbedding(i).mkString(" ")))
    out.close()
  }
  
  // Save and load 
  def saveParameters(filename:String): Unit = {
    val out = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
    saveParameters(out)
    out.close()
  }
  def saveParameters(out:DataOutputStream): Unit = {
    out.writeInt(domain.size)
    out.writeInt(dims)
    out.writeBoolean(opts.separateIO.value)
    val size = domain.size; var i, j = 0
    i = 0; while (i < size) {
      val inputArray = inputEmbedding(i).asArray
      val outputArray = outputEmbedding(i).asArray
      j = 0; while (j < dims) { out.writeDouble(inputArray(j)); j += 1 }
      if (opts.separateIO.value) { j = 0; while (j < dims) { out.writeDouble(outputArray(j)); j += 1 } }
      i += 1
    }
  }
  def loadParameters(filename:String): Unit = {
    val in = new DataInputStream(new GZIPInputStream(new FileInputStream(filename)))
    loadParameters(in)
    in.close()
  }
  def loadParameters(in:DataInputStream): Unit = {
    val size = in.readInt(); assert(size == domain.size)
    val writtenDims = in.readInt(); assert(writtenDims == dims)
    val separateIO = in.readBoolean(); assert(opts.separateIO.value == separateIO)
    var i, j = 0
    i = 0; while (i < size) {
      val inputArray = inputEmbedding(i).asArray
      val outputArray = outputEmbedding(i).asArray
      j = 0; while (j < dims) { inputArray(j) = in.readDouble(); j += 1 }
      if (separateIO) { j = 0; while (j < dims) { outputArray(j) = in.readDouble(); j += 1 } }
      i += 1
    }
  }
    
}


