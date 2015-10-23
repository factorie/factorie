/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.app.nlp.embeddings
import java.io.{BufferedOutputStream, File, FileInputStream, FileOutputStream, OutputStreamWriter}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import cc.factorie.la.DenseTensor1
import cc.factorie.model.{Parameters, Weights}
import cc.factorie.optimize.AdaGradRDA
import cc.factorie.util.Threading

abstract class WordEmbeddingModel(val opts: EmbeddingOpts) extends Parameters {

  // Algo related
  val D = opts.dimension.value // default value is 200
  var V: Int = 0 // vocab size. Will computed in buildVocab() section 
  protected val threads = opts.threads.value //  default value is 12
  protected val adaGradDelta = opts.delta.value // default value is 0.1
  protected val adaGradRate = opts.rate.value //  default value is 0.025 
  protected val minCount = opts.minCount.value // default value is 5
  protected val ignoreStopWords = if (opts.ignoreStopWords.value == true) 1 else 0 // default value is 0
  protected val vocabHashSize = opts.vocabHashSize.value // default value is 20 M. load factor is 0.7. So, Vocab size = 0.7 * 20M = 14M vocab supported which is sufficient enough for large amounts of data
  protected val samplingTableSize = opts.samplingTableSize.value // default value is 100 M
  protected val maxVocabSize = opts.vocabSize.value

  // IO Related
  val corpus = opts.corpus.value // corpus input filename. Code takes cares of .gz extension 
  protected val outputFilename = opts.output.value // embeddings output filename
  private val storeInBinary = if(opts.binary.value == true) 1 else 0 // binary=1 will make both vocab file (optional) and embeddings in .gz file
  private val loadVocabFilename = opts.loadVocabFile.value // load the vocab file. Very useful for large corpus should you run multiple times 
  private val saveVocabFilename = opts.saveVocabFile.value // save the vocab into a file. Next time for the same corpus, load it . Saves lot of time on large corpus
  private val encoding = opts.encoding.value // Default is UTF8
  // data structures
  protected var vocab: VocabBuilder = null
  protected var trainer: LiteHogwildTrainer = null // modified version of factorie's hogwild trainer for speed by removing logging and other unimportant things. Expose processExample() instead of processExamples()
  protected var optimizer: AdaGradRDA = null

  var weights: Seq[Weights] = null // EMBEDDINGS . Will be initialized in learnEmbeddings() after buildVocab() is called first
  private var train_words: Long = 0 // total # of words in the corpus. Needed to calculate the distribution of the work among threads and seek points of corpus file 

  // Component-1
  def buildVocab(): Unit = {
    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor 
    if (loadVocabFilename.size == 0) {
      println("Building Vocab")
      val corpusLineItr = corpus.endsWith(".gz") match {
        case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(corpus)), encoding).getLines
        case false => io.Source.fromInputStream(new FileInputStream(corpus), encoding).getLines
      }
      while (corpusLineItr.hasNext) {
        val line = corpusLineItr.next
        line.stripLineEnd.split(' ').foreach(word => vocab.addWordToVocab(word)) // addWordToVocab() will incr by count. TODO : make this also parallel ? but it is an one time process, next time use load-vocab option
      }
    } else {
      println("Loading Vocab")
      vocab.loadVocab(loadVocabFilename, encoding)
    }
    vocab.sortVocab(minCount, ignoreStopWords, maxVocabSize) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    vocab.buildSubSamplingTable(opts.sample.value) // precompute subsampling table
    V = vocab.size()
    train_words = vocab.trainWords()
    println("Corpus Stat - Vocab Size :" + V + " Total words (effective) in corpus : " + train_words)
    // save the vocab if the user provides the filename save-vocab 
    if (saveVocabFilename.size != 0) {
      println("Saving Vocab into " + saveVocabFilename)
      vocab.saveVocab(saveVocabFilename, storeInBinary, encoding) // for every word, <word><space><count><newline> 
      println("Done Saving Vocab")
    }

  }

  // Component-2
  def learnEmbeddings(): Unit = {
    println("Learning Embeddings")
    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    weights = (0 until V).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)
    trainer = new LiteHogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
    val threadIds = (0 until threads).map(i => i)
    val fileLen = new File(corpus).length
    Threading.parForeach(threadIds, threads)(threadId => workerThread(threadId, fileLen))
    println("Done learning embeddings. ")
    //store()
  }

  // Component-3
  def store(): Unit = {
    println("Now, storing the embeddings .... ")
    val out = storeInBinary match {
      case 0 => new java.io.PrintWriter(outputFilename, encoding)
      case 1 => new OutputStreamWriter(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(outputFilename))), encoding)
    }
    // format :
    // <vocabsize><space><dim-size><newline>
    // <word>[<space><embedding(word)(d)>]*dim-size<newline>
    out.write("%d %d\n".format(V, D))
    for (v <- 0 until V) {
      out.write(vocab.getWord(v) + " " )
      val embedding = weights(v).value
      for (d <- 0 until D)
        out.write(embedding(d) + " ")
      out.write("\n")
      out.flush()
    }
    out.close()
    println("Done storing embeddings")
  }

  protected def workerThread(id: Int, fileLen: Long, printAfterNDoc: Long = 100): Unit = {
    val skipBytes: Long = fileLen / threads * id // fileLen now pre-computed before pasing to all threads. skip bytes. skipped bytes is done by other workers
    val lineItr = new FastLineReader(corpus, skipBytes, encoding)
    var word_count: Long = 0
    var work = true
    var ndoc = 0
    val total_words_per_thread = train_words / threads // worker amount . 
    while (lineItr.hasNext && work) {
      word_count += process(lineItr.next) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?. 
      ndoc += 1
      if (id == 1 && ndoc % printAfterNDoc == 0) { // print the process after processing 100 docs in 1st thread. It approx reflects the total progress
        println("Progress : " + word_count / total_words_per_thread.toDouble * 100 + " %")
      }
      if (word_count > total_words_per_thread) work = false // Once, word_count reaches this limit, ask worker to end
    }
  }

  // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
  protected def process(doc: String): Int
}
