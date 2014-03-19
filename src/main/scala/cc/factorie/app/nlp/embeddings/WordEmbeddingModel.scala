package cc.factorie.app.nlp.embeddings;
import cc.factorie.model.Parameters
import cc.factorie.app.nlp.segment.DeterministicTokenizer
import cc.factorie.app.nlp.Document
import cc.factorie.optimize.Trainer
import cc.factorie.optimize.HogwildTrainer
import cc.factorie.optimize.AdaGrad
import cc.factorie.optimize.Example
import cc.factorie.model.Weights
import cc.factorie.model.WeightsMap
import cc.factorie.optimize.AdaGradRDA
import cc.factorie.la.DenseTensor1
import java.io.PrintWriter
import java.io.File
import scala.util.Random

abstract class WordEmbeddingModel(val opts : EmbeddingOpts) extends Parameters {
    
      // Algo related
      val D = opts.dimension.value // default value is 200
      private val threads = opts.threads.value  //  default value is 12
      private val adaGradDelta = opts.delta.value // default value is 0.1
      private val adaGradRate = opts.rate.value //  default value is 0.025 
      private val minCount = opts.minCount.value
      
      // IO Related
      private val corpus = opts.corpus.value
      private val outputFile = opts.output.value 
      
      // data structures
      var vocab : VocabBuilder = null
      private var trainer : HogwildTrainer = null
      private var optimizer : AdaGradRDA = null
      private var corpusLineItr : Iterator[String] = null
      var V : Int = 0
      var weights : Seq[Weights] = null
     
      
      // debug info
      private var nLines  = 0 // aka nDoc
      private var totalLines = 0 // aka totalDocs 
      
      
      // Component-1
      def buildVocab(minFreq : Int = 5) : Unit = {
            vocab = new VocabBuilder
            println("Building Vocab")
            for (line <- io.Source.fromFile(corpus).getLines) {
               /*  SUPER SLOW . WHY ?
                *  val doc = new Document(line)
                 new DeterministicTokenizer.process(doc)
                 doc.tokens.foreach(token => vocab.addWordToVocab(token.string))
                 * 
                 */
                 line.stripLineEnd.split(' ').foreach(word => vocab.addWordToVocab(word))
                 totalLines += 1
            }
            vocab.sortVocab(minCount) // removes words whose count is less than minCount and sorts by frequency
            vocab.buildSamplingTable // for getting random word from vocab in O(1) otherwise would O(log |V|)
            V = vocab.size
            println("Vocab Size :" + V)
            if (opts.saveVocabFile.hasValue) {
              println("Saving Vocab")
              vocab.saveVocab(opts.saveVocabFile.value)
            }
            
      }
      // Component-2
      def learnEmbeddings() : Unit = {
          println("Learning Embeddings")
          optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)    
          weights  =  (0 until V).map(i =>  Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
          optimizer.initializeWeights(this.parameters)
          trainer = new HogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue,
                                    logEveryN = -1, locksForLogging = false)
          
           corpusLineItr = io.Source.fromFile(corpus).getLines
           var examples = getExamplesInBatch()
           while (examples.size > 0 && !trainer.isConverged) {
             println("# documents (lines) done : %d, Progress %f".format(nLines, nLines/totalLines.toDouble * 100) + "%")
             trainer.processExamples(examples)
             examples = getExamplesInBatch()
           }
          store()
      }
      // Component-3
      def store() {
         val out = new PrintWriter(new File(outputFile))
         out.println( "%d %d".format(V, D) )
         for (v <- 0 until V) {
              out.print(vocab.getWord(v))
              val embedding = weights(v).value
              for (d <- 0 until D)
                 out.print( " " + embedding(d))
              out.print("\n")
              out.flush()
         } 
         out.close()
      }
      
      // TODO : make this process parallel 
      def getExamplesInBatch(maxEg : Int = 1e5.toInt) : Seq[Example] = {
           var examples = new collection.mutable.ArrayBuffer[Example]
           while (examples.size < maxEg && corpusLineItr.hasNext) {
                  examples ++= getExamplesFromSingleDocument(corpusLineItr.next)
                  nLines += 1
           }
           examples
      }
      // override this function in your Embedding Model like SkipGramEmbedding or CBOWEmbedding
      def getExamplesFromSingleDocument(doc : String) : Seq[Example]
      
}
