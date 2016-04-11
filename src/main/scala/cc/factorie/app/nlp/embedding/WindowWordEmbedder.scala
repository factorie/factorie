/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.nlp.embedding
import java.io._
import java.text.NumberFormat
import java.util.Locale
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.la._
import cc.factorie.model._
import cc.factorie.optimize._
import cc.factorie.variable.CategoricalDomain
import org.apache.commons.compress.compressors.CompressorStreamFactory

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.xml.pull._

class WindowWordEmbedderOptions extends cc.factorie.util.DefaultCmdOptions {
  val vocabInput = new CmdOption("vocab-input", List("enwiki-latest-pages-articles.xml.bz2"), "TXTFILE", "Text files from which to read documents and words for building the vocabulary.  Works with *.txt.gz, Wikipedia enwiki*.xmlgz2, and a few other formats.")
  val trainInput = new CmdOption("train-input", List("enwiki-latest-pages-articles.xml.bz2"), "TXTFILE", "Text files from which to read documents and words for training the embeddings.  Works with *.txt.gz, Wikipedia enwiki*.xmlgz2, and a few other formats.")
  val parametersSave = new CmdOption("parameters-save", "parameters.gz", "FILE", "If invoked, save the parameters after training to this filename in compressed binary format.")
  val parametersLoad = new CmdOption("parameters-load", "parameters.gz", "FILE", "If invoked, load the parameters at initialization time from this filename containing compressed binary embedding parameters.")
  val dims = new CmdOption("dims", 50, "INT", "Dimensionality of the embedding vectors.")
  val seed = new CmdOption("seed", 0, "INT", "Seed for random number generator.")
  val minContext = new CmdOption("min-context", 2, "INT", "Skip training windows that end up with fewer context words after randomized removal of common context words.")
  val window =  new CmdOption("window", 5, "INT", "The number of words on either side of the target to include in the context window.")
  val normalizeX = new CmdOption("normalize-x", false, "BOOL", "If true, normalize input context by the number of context words in the training example.")
  val vocabulary = new CmdOption("vocabulary", "vocabulary.txt", "FILE", "Filename from which to initialize or save the collection of all word types, one per line, each preceded by its count.")
  val negative = new CmdOption("negative", 3, "INT", "The number of NCE negative examples to use for each positive example.")
  val maxDocuments = new CmdOption("max-documents", Long.MaxValue, "LONG", "Read no more than this number of documents or Wikipedia pages.  Default is Int.MaxValue.")
  val separateIO = new CmdOption("separate-io", false, "BOOLEAN", "If TRUE, parameterize input embeddings (U) separately from output embeddings (V).  Default is FALSE.")
  val checkGradient = new CmdOption("check-gradient", false, "BOOLEAN", "If TRUE, test the value/gradient calculation for every parameter for every example after the first 50000 example.  (Slow.)  Default is FALSE.")
  val outputExamples = new CmdOption("output-examples", "examples.txt.gz", "FILE", "Save the training targets/contexts in this file, one per line.")
  val useAliasSampling = new CmdOption("alias-sampling", false, "BOOLEAN", "Sample negative examples using alias sampling vs. power-law approximation.")
  val powerForCounts = new CmdOption("power-for-counts", 0.75, "DOUBLE", "Power to raise counts to when computing proportions for exact alias sampling.")

  val vocabMinCount = new CmdOption("vocab-min-count", 200, "INT", "Words with count smaller than this will be discarded when building the vocabulary.  Default is 200.")
  val vocabSkipProb = new CmdOption("vocab-skip-prob", 0.0, "DOUBLE", "The probabilty that each word string will be skipped in the indexing and counting when building the vocabulary.  Helps efficiently cull words occurring ~1 times.")
}

trait WindowWordEmbedderExample extends Example {
  def inputIndices: Array[Int]
  def outputIndices: Array[Int]
  def changedWeights: ArrayBuffer[Weights]
}

abstract class WordEmbedder(val opts:WindowWordEmbedderOptions) extends Parameters {
  val dims = opts.dims.value
  val random = new Random(opts.seed.value)
  val domain = new CategoricalDomain[String]; domain.gatherCounts = true 

  val maxDomainSize = initDomain()
  // Initialize both input and output embedding vectors
  private val _inputEmbedding = Array.fill(maxDomainSize)(Weights(new DenseTensor1(dims).fill(() => random.nextDouble()/dims/10 - 0.5/dims/10))) // TODO How should vectors be initialized? /10 good?
  private val _outputEmbedding = if (opts.separateIO.value) Array.fill(maxDomainSize)(Weights(new DenseTensor1(dims).fill(() => random.nextDouble()/dims/10 - 0.5/dims/10))) else _inputEmbedding // TODO How should vectors be initialized? /10 good?
  private val _discardProb = new Array[Double](maxDomainSize)

  def inputEmbedding(i:Int) = _inputEmbedding(i).value
  def inputWeights(i:Int) = _inputEmbedding(i)
  def inputEmbedding(word:String) = { val index = domain.index(word); if (index >= 0) _inputEmbedding(index).value else null }
  def outputEmbedding(i:Int) = _outputEmbedding(i).value
  def outputWeights(i:Int) = _outputEmbedding(i)
  def outputEmbedding(word:String) = { val index = domain.index(word); if (index >= 0) _outputEmbedding(index).value else null }
  // TODO Should this be here, or should we let subclasses handle this? -akm
  if (opts.parametersLoad.wasInvoked) loadParameters(new File(opts.parametersLoad.value))
  
  def discardProb(wordIndex: Int): Double = {
    var result = _discardProb(wordIndex)
    //println("discardProb "+result)
    if (result != 0.0) return result
    result = 1.0 - math.sqrt(0.0001/(domain.count(wordIndex).toDouble / domain.countsTotal.toDouble))
    _discardProb(wordIndex) = result
    //println("discardProb again "+result)
    result
  }
  def discard(wordIndex:Int): Boolean = random.nextDouble() < discardProb(wordIndex)
  def discardProbReset(): Unit = { java.util.Arrays.fill(_discardProb, 0.0) }
    
  /** Initialize the vocabulary into this.domain, either by the incrementalVocabMaxSize or by reading the vocabulary from a file. */
  def initDomain(): Int = {
    // Read in the vocabulary 
    for (splitLine <- io.Source.fromFile(opts.vocabulary.value).getLines().map(_.split(' '))) domain.indexWithCount(splitLine(1), splitLine(0).toInt)
    println("Vocabulary size "+domain.size)
    println("Vocabulary total count "+domain.countsTotal)
    domain.freeze()
    domain.size
  }
  
  def stringToWordIndices(string:String): cc.factorie.util.IntArrayBuffer = {
    val wordIndices = new cc.factorie.util.IntArrayBuffer(string.length/4) // guessing average word length > 4
    for (word <- stringToWords(string)) {
      val wordIndex = domain.index(word)
      if (wordIndex >= 0) wordIndices += wordIndex
    }
    wordIndices
  }
  def stringToExamples(string:String): Iterable[WindowWordEmbedderExample] = {
    val wordIndices = stringToWordIndices(string)
    if (wordIndices.length > opts.window.value) { // TODO was > 2*opts.window.value
      val array = wordIndices._rawArray
      val windowSize = opts.window.value
      // this is slow
      for (targetPosition <- 0 until wordIndices.length - windowSize; example <- newExample(this, array, targetPosition, 1 + random.nextInt(windowSize))) yield example
    } else Nil
  }
  def newExample(model:WordEmbedder, wordIndices:Array[Int], centerPosition:Int, window:Int): Option[WindowWordEmbedderExample]
  
  def train(files:Seq[File]): Unit = {
    //val strings = for (filename <- filenames; string <- fileToStringIterator(new File(filename))) yield string
    val strings = files.iterator.flatMap(f => fileToStringIterator(f))
    train(strings)
  }

  
  /** Train the parameters of the embedding given an iterator over the string contents of "documents",
      which could be large textual documents like Wikipedia pages or short documents like tweets or titles.

      Every snapshotIncrement training examples, write the embedding parameters to a file 
      named, for example, embeddings-0010m, for the 10 millionth training window.
      If you don't want incremental saving of parameter snapshots, set snapshotIncrement to a negative number. 

      Every logIncrement print to stdout how many training windows we have trained on so far.
      If you don't want any logging, set logIncrement to a negative number.
*/
  def train(strings:Iterator[String], snapshotIncrement:Int = 10000000, logIncrement:Int = 1000000): Unit = {
    if (opts.outputExamples.wasInvoked) println("Writing data examples to "+opts.outputExamples.value)
    var wordCount = 0
    var nextWordCountLog = logIncrement
    var nextWordCountShapshot = snapshotIncrement
    val optimizer = new AdaGrad()
    optimizer.initializeWeights(this.parameters)
    val trainer = new OnlineTrainer(parameters, optimizer, logEveryN=50000)
    for (string <- strings) {
      val examples = stringToExamples(string)
      //println("CBOW.train examples.size = "+examples.size)
      wordCount += examples.size
      if (opts.checkGradient.value && wordCount >= 10000) {
        print(s"CBOW testGradient ${examples.map(e => e.outputIndices.map(domain.category).mkString(" ")).mkString(" ")} ...")
        examples.foreach(e => Example.testGradient(parameters, parameters.keys, e, dx = 1e-7, verbose = true, returnOnFirstError = false)) // TODO Put back "assert"
        println("finished.")
      }
      trainer.processExamples(examples)
      if (logIncrement > 0 && wordCount > nextWordCountLog) { println(s" Trained on ${wordCount/1000000}m contexts."); nextWordCountLog += logIncrement } 
      if (snapshotIncrement > 0 && wordCount > nextWordCountShapshot && !opts.outputExamples.wasInvoked) {
        val fn = f"embeddings-${wordCount/1000000}%04dm"
        println("Writing intermediate embeddings to "+fn)
        writeInputEmbeddings(fn)
        nextWordCountShapshot += snapshotIncrement
      }
    }
    if (opts.parametersSave.wasInvoked) saveParameters(new File(opts.parametersSave.value))
  }


  /** Return a ranked list of domain index/word by their gated output vector's similarity the query vector.
    The query vector is assumed to be already gated.  The output vectors are gated on the fly here. */
  def neighbors(query:DenseTensor1, count:Int = 10): cc.factorie.util.TopN[String] = {
    val top = new cc.factorie.util.TopN[String](count)
    for (i <- 0 until domain.size) top += (i, query.cosineSimilarity(_outputEmbedding(i).value), domain.category(i))
    top
  }
  
  /** Return a list of vocabulary indices to use a negative training examples, sampled according to a function of their counts. */
  def makeNegativeSamples: Array[Int] = {
    if (opts.useAliasSampling.value) {
      val len = opts.negative.value
      val ret = new Array[Int](len)
      var i = 0
      while (i < len) {
        ret(i) = negativeSampler.sample()
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
  }
  lazy val negativeSampler = new cc.factorie.util.Alias(domain.counts.asArray.map(_.toDouble).map(math.pow(_, opts.powerForCounts.value)))(random)

  
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

  var docCount = 0
  /** Read text to build up vocabulary and write the vocabulary to the filename specified by --vocabulary. */
  def buildVocabulary(filenames:Seq[String]): Unit = {
    // Recursively gather all files listed on command line
    val files = opts.vocabInput.value.flatMap(filename => recurseFiles(new File(filename)))
    // Set up a String map to gather counts
    val domain = new CategoricalDomain[String]
    domain.gatherCounts = true
    val printInterval = 100
    val random = new scala.util.Random(0)
    val skipThreshold = 1.0 - opts.vocabSkipProb.value
    var printAtCount = printInterval
    var wordCount = 0
    //var docCount = 0
    // From all files, segment contents into words, and count them
    files.foreach(file => {
      println("Vocabulary reading "+file.getName())
      for (line <- fileToStringIterator(file)) {
        if (docCount >= printAtCount) {
          print("\r"+NumberFormat.getNumberInstance(Locale.US).format(docCount)+" articles, "+NumberFormat.getNumberInstance(Locale.US).format(wordCount)+" tokens, "+NumberFormat.getNumberInstance(Locale.US).format(domain.size)+" vocabulary")
          //print(s"\r$wikipediaArticleCount articles\t$wordCount words") ; 
          printAtCount += printInterval
        }
        for (word <- stringToWords(line)) {
          //println("Vocabulary read word "+word)
          wordCount += 1
          if (skipThreshold == 0.0 || random.nextDouble() < skipThreshold) // Randomly sample to avoid words appearing only ~1 times.
            domain.index(new String(word)) // to avoid memory leak: http://stackoverflow.com/questions/15612157/substring-method-in-string-class-causes-memory-leak
        }
      }
    })
    println(s"Read $docCount documents")
    println(s"Read ${domain.countsTotal} tokens, ${domain.size} types.")
    // Get rid of words occurring less than 10 times
    domain.trimBelowCount(opts.vocabMinCount.value, preserveCounts = true)
    println(s"Trimed to ${domain.countsTotal} tokens, ${domain.size} types.")
    // Serialize the results
    println("Sorting...")
    //the line below results in writing an empty vocabulary
    //writeVocabulary(opts.vocabulary.value)
    val sorted = domain.categories.map(c => (domain.count(c), c)).sortBy(-_._1)
    val out = new PrintWriter(opts.vocabulary.value)
    for ((count, word) <- sorted)
      out.println("%d %s".format(count, word))
    out.close()
    println("Done writing vocabulary.")
  }
  
  def writeVocabulary(filename:String): Unit = {
    val sorted = domain.categories.map(c => (domain.count(c), c)).sortBy(-_._1)
    val out = new PrintWriter(filename)
    for ((count, word) <- sorted)
      out.println("%d %s".format(count, word))
    out.close()
  }
  
  /** Given a document's string contents, return an iterator over the individual word tokens in the document */
  def stringToWords(string:String): Iterator[String] = {
    //alphaSegmenter(string).filter(word => !Stopwords.contains(word.toLowerCase))
    alphaSegmenter(string)
  }
  
  /** Given a file, return an iterator over the string contents of each "document".
      Returns nil if the filename suffix is not handled.
   */
  def fileToStringIterator(file:File, encoding:String = "UTF8"): Iterator[String] = {
    file.getName match {
      // Plain text file
      case name if name.endsWith(".txt") => Source.fromFile(file, encoding).getLines()
      // Compressed text
      case name if name.endsWith(".txt.gz") => {
        val lineIterator = Source.fromInputStream(new GZIPInputStream(new FileInputStream(file)), encoding).getLines()
        var lineCount = 0
        new Iterator[String] {
          def hasNext: Boolean = lineIterator.hasNext && lineCount < opts.maxDocuments.value
          def next(): String = {
            lineCount += 1
            val sb = new StringBuffer
            var emptyLine = false
            while (lineIterator.hasNext && !emptyLine) {
              val line = lineIterator.next()
              if (line.length > 0) sb.append(line)
              else emptyLine = true
            }
            sb.toString
          }
        }
      }
      case name if name.endsWith(".csv") =>
        val charset = java.nio.charset.Charset.forName(encoding)
        val codec = new scala.io.Codec(charset)
        codec.onMalformedInput(java.nio.charset.CodingErrorAction.IGNORE)
        val lineIterator = Source.fromFile(file)(codec).getLines()
        val cleaningRegex = "\"\\d+\"|<[^>]+>|&[a-z]{3,4};".r
        new Iterator[String] {
          var stringCount:Long = 0L
          var lineCount:Long = 0L
          def hasNext: Boolean = lineIterator.hasNext && stringCount < opts.maxDocuments.value //maxStringCount
          def next(): String = {
            val sb = new StringBuffer
            var lineEnd = false
            try {
              while (lineIterator.hasNext && !lineEnd) {
                val line = lineIterator.next()
                lineCount += 1
                if (line.length > 0) sb.append(line)
                if (!line.endsWith("\\")) lineEnd = true
              }
            } catch {
              case e:Exception => System.err.println("Caught at line "+lineCount+" exception\n"+e)
            }
            stringCount += 1
            if (stringCount % 1000 == 0) print("\r"+stringCount)
            cleaningRegex.replaceAllIn(sb.toString, " ")
          }
        }
      case name if name.startsWith("enwiki") && name.endsWith(".xml.bz2") =>
        //var wikipediaArticleCount = 0
        val docIterator = cc.factorie.app.nlp.load.LoadWikipediaPlainText.fromCompressedFile(file, opts.maxDocuments.value)
        new Iterator[String] {
          def hasNext: Boolean = docIterator.hasNext
          def next(): String = { docCount += 1; val doc = docIterator.next(); /*println(doc.name);*/ doc.string }
        }
      // bz2 compress wikipedia XML
      case name if name.startsWith("deprecated enwiki") && name.endsWith(".xml.bz2") => {
        var wikipediaArticleCount = 0
        val input = new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(file))
        val xml = new scala.xml.pull.XMLEventReader(Source.fromInputStream(input))
        //val xml = XMLInputFactory.newInstance().createXMLEventReader(new InputStreamReader(input))
        val cleaningRegex = List(
            "\\{\\{[^\\}]*\\}\\}", // Remove everything {{inside}}
            "\\{\\|[^\\}]*\\|\\}", // Remove everything {|inside|}
            "(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
            "\\[\\[(?:File|Image):[^\\]]+\\|", // Remove [[File:WilliamGodwin.jpg|left|thumb|[
            "wikt:|nbsp;|ndash;|br/",
            "Category:"
            ).mkString("|").r
        new Iterator[String] {
          def hasNext: Boolean = {
            val result = xml.hasNext && wikipediaArticleCount < opts.maxDocuments.value // Artificially stopping early
            if (!result) { /*xml.close();*/ xml.stop(); input.close(); println(getClass.getName+": fileToStringIterator closing.") }
            result
          }
          def next(): String = {
            var done = false
            var insidePage = false
            var insideText = false
            var insideRef = false
            var insideComment = false
            val sb = new StringBuffer
            while (xml.hasNext && !done) {
              xml.next() match {
                //case e => println(e)
                case EvElemStart(_, "page", _, _) => { insidePage = true }
                case EvElemEnd(_, "page") => { insidePage = false; done = true }
                case EvElemStart(_, "text", _, _) => { insideText = true }
                case EvElemEnd(_, "text") => { insideText = false; done = true }
                case EvText(t) if insideText => {
                  if (t.startsWith("!--") && !t.endsWith("--")) insideComment = true
                  else if (t.endsWith("--")) insideComment = false
                  else if (t.startsWith("ref") && !t.endsWith("/")) insideRef = true
                  else if (t == "/ref") insideRef = false
                  else if (!insideRef && !insideComment && !t.startsWith("ref ") && !t.startsWith("#REDIRECT")) { sb append t; sb append ' ' }
                }
                case _ => // ignore all other tags
              }
            }
            var s = cleaningRegex.replaceAllIn(sb.toString, " ")
            s = s.trim; if (s.length > 0) { wikipediaArticleCount += 1; return s } else { /*println(s"Skipping at $wikipediaArticleCount");*/ return next() }
            s
          }
        }
      }
      case _ => throw new Error("Unknown suffix on document name "+file.getName())
    }
  }

  // Recursively descend directory, returning a list of files.
  def recurseFiles(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new scala.collection.mutable.ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= recurseFiles(entry)
    }
    result
  }
  
    
  def writeExamples(inputFilenames:Array[String]): Unit = {
    val dataWriter = if (opts.outputExamples.wasInvoked) new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(opts.outputExamples.value)), "UTF-8") else null
    var examplesWrittenCount = 0
    for (filename <- inputFilenames; string <- fileToStringIterator(new File(filename)); example <- stringToExamples(string)) {
      examplesWrittenCount += 1
      dataWriter.write(s"$examplesWrittenCount\t${example.outputIndices.map(domain.category(_)).mkString(" ")}\t${example.inputIndices.map(i => domain.category(i)).mkString("\t")}\n")
    }
    dataWriter.close()
  }

  // Writing embedding parameters to a file in textual format
  def writeInputEmbeddings(filename:String): Unit = {
    val out = new PrintWriter(filename)
    for (i <- 0 until domain.size)
      out.println("%s\t%s".format(domain.category(i), inputEmbedding(i).mkString(" ")))
    out.close()
  }
  def writeOutputEmbeddings(filename:String): Unit = {
    val os = new FileOutputStream(filename)
    writeOutputEmbeddings(os)
    os.close()
  }
  def writeOutputEmbeddings(out:OutputStream): Unit = {
    val pw = new PrintWriter(out)
    for (i <- 0 until domain.size)
      pw.println("%s\t%s".format(domain.category(i), outputEmbedding(i).mkString(" ")))
    pw.flush()
  }
  
  // Save and load embedding parameters in a compressed binary format
  def saveParameters(file:File): Unit = {
    val out = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(file)))
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
  def loadParameters(file:File): Unit = {
    val in = new DataInputStream(new GZIPInputStream(new FileInputStream(file)))
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


trait IncrementalVocabularyOptions extends WindowWordEmbedderOptions {
  val incrementalVocabMaxSize = new CmdOption("incremental-vocab-max-size", 100000, "INT", "When invoked this turns on incremental vocabulary building during training, allowing the vocabulary to grow up to this limit.")
  val incrementalVocabMinCount = new CmdOption("incremental-vocab-min-count", 100, "INT", "When doing incremental vocabulary building, don't actually assign a word an embedding vector until it has been seen this many times.")
}


trait IncrementalVocabulary extends WordEmbedder {
  lazy val incrementalVocabulary = new CategoricalDomain[String]
  incrementalVocabulary.gatherCounts = true
  
  override  def initDomain(): Int = {
    domain.freeze()
    opts.asInstanceOf[IncrementalVocabularyOptions].incrementalVocabMaxSize.value
  }
  
  // Since the vocabulary and counts are growing, we can't just always use the same cached word counts, but must recalcuate them every once in a while
  private var _lastDiscardProbResetCount = 0L
  override def discardProb(wordIndex: Int): Double = {
    if (domain.countsTotal < 1000000) return 0.0000001
    if (domain.countsTotal - _lastDiscardProbResetCount > 10000) {
      discardProbReset()
      _lastDiscardProbResetCount = domain.countsTotal
    }
    return super.discardProb(wordIndex)
  }


  override def stringToWordIndices(string:String): cc.factorie.util.IntArrayBuffer = {
    val wordIndices = new cc.factorie.util.IntArrayBuffer(string.length/4) // guessing average word length > 4
    for (word <- stringToWords(string)) {
      var wordIndex = domain.index(word)
      if (wordIndex == -1) {
        // The word doesn't yet have an embedding vector
        val vi = incrementalVocabulary.index(word) // increment its count in the vocabulary of words that don't yet have an embedding
        if (incrementalVocabulary.count(vi) > opts.asInstanceOf[IncrementalVocabularyOptions].incrementalVocabMinCount.value && domain.size < opts.asInstanceOf[IncrementalVocabularyOptions].incrementalVocabMaxSize.value) {
          domain.unfreeze()
          wordIndex = domain.index(word) // If the count is now above threshold and there is room to grow, then put then give the word an embedding
          domain.freeze()
        }
      }
      if (wordIndex >= 0) wordIndices += wordIndex
    }
    //println("IncrementalVocabulary "+wordIndices.toSeq)
    wordIndices
  }

}
