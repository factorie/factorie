package cc.factorie.app.nlp.embeddings
import cc.factorie.util.CmdOptions

class EmbeddingOpts extends CmdOptions {

  // Algorithm related
  val dimension = new CmdOption("size", 200, "INT", "size of word vectors")
  val window = new CmdOption("window", 5, "INT", "use <int> skip length between words")
  val threads = new CmdOption("threads", 12, "INT", "use <int> threads")
  val negative = new CmdOption("negative", 1, "INT", "use <int> number of negative examples")
  val minCount = new CmdOption("min-count", 5, "INT", "This will discard words that appear less than <int> times; default is 5")
  val maxCount = new CmdOption("max-count", 0, "INT", "This will discard words top <int> words")
  val cbow = new CmdOption("cbow", 0, "INT", "This will will run skip gram with negative sampling") // 1 would be SkipGram // default method is skipgram 
  val sample = new CmdOption("sample", 0.001, "DOUBLE", "use <double> subsampling")

  // Optimization related (Don't change if you do not understand how vectors are initialized)
  val rate = new CmdOption("rate", 0.025, "DOUBLE", "learning rate for adaGrad")
  val delta = new CmdOption("delta", 0.1, "DOUBLE", "delta for adaGrad")

  // IO Related (MUST GIVE Options)
  // val readVocabFile = new CmdOption("read-vocab", "NONE", "STRING", "vocab file") TODO : Small code change
  val saveVocabFile = new CmdOption("save-vocab", "NONE", "STRING", "save vocab file")
  val corpus = new CmdOption("train", "NONE", "STRING", "train file")
  val output = new CmdOption("output", "NONE", "STRING", "Use <file> to save the resulting word vectors")

  // Vocabulary related
  // Maximum 20 * 0.7 = 14M words in the vocabulary (Don;t change if you understand how vocabBuilder works)
  val vocabHashSize = new CmdOption("vocab-hash-size", 20e6.toInt, "INT", "Vocabulary hash size")
  val samplingTableSize = new CmdOption("sampling-table-size", 1e8.toInt, "INT", "Sampling Table size")

  // Debug (Print more information on terminal for debugging)
  val debug = new CmdOption("debug", 2, "INT", "debug mode <int> : 2 for debug and 1 for without debug")

}
