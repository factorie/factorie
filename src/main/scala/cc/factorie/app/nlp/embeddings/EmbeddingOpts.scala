package cc.factorie.app.nlp.embeddings
import cc.factorie.util.CmdOptions

class EmbeddingOpts extends CmdOptions {

  // Algorithm related
  val dimension = new CmdOption("size", 200, "INT", "use <int> size of word vectors")
  val window = new CmdOption("window", 5, "INT", "use <int> skip length between words")
  val threads = new CmdOption("threads", 12, "INT", "use <int> threads")
  val negative = new CmdOption("negative", 1, "INT", "use <int> number of negative examples")
  val minCount = new CmdOption("min-count", 5, "INT", "This will discard words that appear less than <int> times; default is 5")
  val ignoreStopWords = new CmdOption("ignore-stopwords", 0, "INT", "use <int> to include or discard stopwords. Use 1 for discarding stopwords")
  val cbow = new CmdOption("cbow", 0, "INT", "user cbow=1 for cbow and cbow=0 for skip-gram") // 1 would be SkipGram // default method is skipgram 
  val sample = new CmdOption("sample", 0.001, "DOUBLE", "use <double> subsampling")
  
  // Optimization related (Don't change if you do not understand how vectors are initialized)
  val rate = new CmdOption("rate", 0.025, "DOUBLE", "learning rate for adaGrad")
  val delta = new CmdOption("delta", 0.1, "DOUBLE", "delta for adaGrad")

  // IO Related (MUST GIVE Options)
  val encoding = new CmdOption("encoding", "ISO-8859-15", "STRING", "use <string> for encoding option. ISO-8859-15 is default")
  val saveVocabFile = new CmdOption("save-vocab", "", "STRING", "save vocab file")
  val loadVocabFile = new CmdOption("load-vocab", "", "STRING", "load the vocab file")
  val corpus = new CmdOption("train", "", "STRING", "train file")
  val output = new CmdOption("output", "", "STRING", "Use <file> to save the resulting word vectors")
  val binary = new CmdOption("binary", 0, "INT", "use 1 for storing .gz format and 0 for plain txt format. Both stores in ISO-8859-15 Encoding")

  // Vocabulary related
  // Maximum 14.3M * 0.7 = 10M words in the vocabulary (Don;t change if you understand how vocabBuilder works)
  val vocabSize = new CmdOption("max-vocab-size", 2e6.toInt, "INT", "Max Vocabulary Size. Default Value is 2M . Reduce to 200k or 500k is you learn embeddings on small-data-set")
  val vocabHashSize = new CmdOption("vocab-hash-size", 14.3e6.toInt, "INT", "Vocabulary hash size")
  val samplingTableSize = new CmdOption("sampling-table-size", 1e8.toInt, "INT", "Sampling Table size")

  
}
