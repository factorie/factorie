package cc.factorie.app.nlp.embeddings
import cc.factorie.model.{Parameters, Weights}
import cc.factorie.optimize.{Trainer, AdaGradRDA}
import cc.factorie.la.DenseTensor1
import cc.factorie.util.Threading
import java.io.{File, PrintWriter}

abstract class WordEmbeddingModel(val opts: EmbeddingOpts) extends Parameters {

  // Algo related
  val D = opts.dimension.value // default value is 200
  protected val threads = opts.threads.value //  default value is 12
  protected val adaGradDelta = opts.delta.value // default value is 0.1
  protected val adaGradRate = opts.rate.value //  default value is 0.025 
  protected val minCount = opts.minCount.value // default value is 5
  protected val maxCount = opts.maxCount.value // default value is 0
  protected val vocabHashSize = opts.vocabHashSize.value // default value is 20 M
  protected val samplingTableSize = opts.samplingTableSize.value // default value is 100 M

  // IO Related
  val corpus = opts.corpus.value
  protected val outputFile = opts.output.value

  // data structures
  protected var vocab: VocabBuilder = null
  protected var trainer: LiteHogwildTrainer = null
  protected var optimizer: AdaGradRDA = null
  private var corpusLineItr: Iterator[String] = null
  var V: Int = 0
  var weights: Seq[Weights] = null
  private var train_words: Long = 0


  // Component-1
  def buildVocab(minFreq: Int = 5): Unit = {
    vocab = new VocabBuilder(vocabHashSize, samplingTableSize, 0.7) // 0.7 is the load factor 
    println("Building Vocab")
    for (line <- io.Source.fromFile(corpus).getLines) {
      line.stripLineEnd.split(' ').foreach(word => vocab.addWordToVocab(word))
    }
    vocab.sortVocab(minCount, maxCount) // removes words whose count is less than minCount and sorts by frequency
    vocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    vocab.buildSubSamplingTable(opts.sample.value) // building the subsampling table
    V = vocab.size()
    train_words = vocab.trainWords()
    println("Vocab Size :" + V)
    if (opts.saveVocabFile.hasValue) {
      println("Saving Vocab")
      vocab.saveVocab(opts.saveVocabFile.value)
    }

  }

  // Component-2
  def learnEmbeddings(): Unit = {
    println("Learning Embeddings")
    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    weights = (0 until V).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)
    trainer = new LiteHogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
    val files = (0 until threads).map(i => i)
    Threading.parForeach(files, threads)(workerThread(_))
    println("Done learning embeddings. ")
    //store()
  }

  // Component-3
  def store(): Unit = {
    println("Now, storing into output... ")
    val out = new PrintWriter(new File(outputFile))
    out.println("%d %d".format(V, D))
    for (v <- 0 until V) {
      out.print(vocab.getWord(v))
      val embedding = weights(v).value
      for (d <- 0 until D)
        out.print(" " + embedding(d))
      out.print("\n")
      out.flush()
    }
    out.close()
    println("Done storing")
  }

  protected def workerThread(id: Int): Unit = {
    val fileLen = new File(corpus).length
    val skipBytes: Long = fileLen / threads * id // skip bytes. skipped bytes is done by other workers
    val lineItr = new FastLineReader(corpus, skipBytes)
    var word_count: Long = 0
    var work = true
    var ndoc = 0
    val total_words_per_thread = train_words / threads // worker amount . 
    while (lineItr.hasNext && work) {
      word_count += process(lineItr.next) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?. 
      ndoc += 1
      if (id == 1 && ndoc % 5 == 0) {
        println("Progress : " + word_count / total_words_per_thread.toDouble * 100 + " %")
      }
      if (word_count > train_words / threads) work = false // Once, word_count reaches this limit, ask worker to end
    }
  }

  // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
  protected def process(doc: String): Int
}
