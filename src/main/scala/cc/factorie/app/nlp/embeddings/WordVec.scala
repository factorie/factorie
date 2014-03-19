package cc.factorie.app.nlp.embeddings;
object WordVec {
    def main(args : Array[String]) {
         val opts = new EmbeddingOpts
         opts.parse(args)
         val skipgram = if (opts.cbow.value == 1) new CBOWNegSamplingEmbeddingModel(opts) else new SkipGramNegSamplingEmbeddingModel(opts)
         skipgram.buildVocab()
         val st = System.currentTimeMillis()
         skipgram.learnEmbeddings
         val en = System.currentTimeMillis() - st 
         println("time taken : " + en/1000.0)
    }
 }
