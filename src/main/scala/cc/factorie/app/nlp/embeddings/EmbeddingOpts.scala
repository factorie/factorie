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
package cc.factorie.app.nlp.embeddings
import cc.factorie.util.CmdOptions

class EmbeddingOpts extends CmdOptions {

  // Algorithm related
  val dimension = new CmdOption("size", 200, "INT", "use <int> size of word vectors")
  val window = new CmdOption("window", 5, "INT", "use <int> skip length between words")
  val threads = new CmdOption("threads", 12, "INT", "use <int> threads")
  val negative = new CmdOption("negative", 1, "INT", "use <int> number of negative examples")
  val minCount = new CmdOption("min-count", 5, "INT", "This will discard words that appear less than <int> times; default is 5")
  val ignoreStopWords = new CmdOption("ignore-stopwords", false, "BOOLEAN", "use <bool> to include or discard stopwords. Use 1 for discarding stopwords")
  val cbow = new CmdOption("cbow", false, "BOOLEAN", "user cbow=true for cbow and cbow=false for skip-gram") // 1 would be SkipGram // default method is skipgram 
  val sample = new CmdOption("sample", 0.001, "DOUBLE", "use <double> subsampling")
  val numIterations = new CmdOption("num-iterations", 1,"INT", "The number of iterations of training to run")
  
  // Optimization related (Don't change if you do not understand how vectors are initialized)
  val rate = new CmdOption("rate", 0.025, "DOUBLE", "learning rate for adaGrad")
  val delta = new CmdOption("delta", 0.1, "DOUBLE", "delta for adaGrad")

  // IO Related (MUST GIVE Options)
  val encoding = new CmdOption("encoding", "UTF8", "STRING", "use <string> for encoding option. ISO-8859-15 is default")
  val saveVocabFile = new CmdOption("save-vocab", "", "STRING", "save vocab file")
  val loadVocabFile = new CmdOption("load-vocab", "", "STRING", "load the vocab file") // atleast one of them  should be given. save-vocab or load-vocab
  val corpus = new CmdOption("train", "", "STRING", "train file")
  val output = new CmdOption("output", "", "STRING", "Use <file> to save the resulting word vectors")
  val binary = new CmdOption("binary", false, "BOOLEAN", "use true for storing .gz format and false for plain txt format. Both stores in ISO-8859-15 Encoding")
  
  // Reading embeddings and finding close neighbors.  (Not training embeddings.)
  val explore = new CmdOption("explore", "embeddings.txt", "FILE", "Filename from which to read already-learned embeddings; the result of --output in a --train run.")

  // Vocabulary related
  // Maximum 14.3M * 0.7 = 10M words in the vocabulary (Don;t change if you understand how vocabBuilder works)
  val vocabSize = new CmdOption("max-vocab-size", 2e6.toInt, "INT", "Max Vocabulary Size. Default Value is 2M . Reduce to 200k or 500k is you learn embeddings on small-data-set")
  val vocabHashSize = new CmdOption("vocab-hash-size", 14.3e6.toInt, "INT", "Vocabulary hash size")
  val samplingTableSize = new CmdOption("sampling-table-size", 1e8.toInt, "INT", "Sampling Table size")

  
}
