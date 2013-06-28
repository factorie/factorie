package cc.factorie.app.topics.lda

import cc.factorie._
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import cc.factorie.maths.digamma
import cc.factorie.directed._
import cc.factorie.la.DenseTensor1

/**
 * Created with IntelliJ IDEA.
 * User: vineet
 * Date: 6/23/13
 * Time: 6:03 PM
 * To change this template use File | Settings | File Templates.
 */

/* Implementation for Sparse Stochastic inference by Mimno et.al */
class SparseOnlineLDA(val docProvider: WordSeqProvider,
                      val numTopics: Int,
                      val alpha: Double,
                      val beta: Double,
                      val batchSize: Int,
                      val numSamples: Int,
                      val burninSamples: Int,
                      val learningRate: Double,
                      val kappa: Double = 0.6,
                      val numBatches: Int)
{
  implicit val random = new scala.util.Random(0)
  implicit val model = DirectedModel()

  val wordDomain: CategoricalDomain[String] = docProvider.getWordDomain
  val numTypes = wordDomain.length
  val numDocs = docProvider.numDocs

  val betas = MassesVariable.growableUniform(wordDomain, beta)
  val phis = Mixture(numTopics)(ProportionsVariable.growableDense(wordDomain) ~ Dirichlet(betas))

  var typeWeights:Array[Array[Double]] = null
  var typeTopics:Array[Array[Int]] = null //n_tw
  val topicTokenTotals = new Array[Double](numTopics) // n_t

  def approximateExpDigamma(x: Double) = {
    var correction = 0.0;
    var y = x

    while (y < 5.0) {
      correction += 1.0 / y
      y += 1.0
    }

    (y - 0.5) * Math.exp(-correction)
  }

  def export(): Unit = {
    phis.foreach(_.tensor.zero())
    for (wi <- 0 until wordDomain.size) {
      val weights = typeWeights(wi)
      val topics = typeTopics(wi)

      var index = 0
      while(index < numTopics && weights(index) > 0.0) {
        val topic = topics(index)
        phis(topic).value.masses.+=(wi, weights(index))
        index += 1
      }
    }
  }

  def train() {
    println("numTypes: "+ numTypes+ " numTopics: "+ numTopics + " batchSize: "+ batchSize)

    var wordGradientSize = 500000
    var docTopicsSize = 50000

    //Store the constants
    val expDiGammaBeta:Double = Math.exp(digamma(beta)) // exp(digamma(beta))
    val betaSum = beta * numTypes // (V * beta)
    val wordWeightConstant = numDocs / (batchSize * numSamples) // D / (S * B)

    //Global Almost-sorted arrays representing N_kw
    typeWeights = Array.tabulate(numTypes)(t => new Array[Double](numTopics))
    typeTopics = Array.tabulate(numTypes)(t => new Array[Int](numTopics))

    val topicNormalizers = new Array[Double](numTopics)  // represent expDiGamma(beta * V + Nk)
    val topicCoefficients = new Array[Double](numTopics) // represent alpha + Ndk * topicNormalizer

    val samplingWeights = new Array[Double](numTopics)  // represent Part 1 of Eq15
    val topicTokenTotals = new Array[Double](numTopics) // represent Nk

    //Arrays to note what we sampled for the current batch
    var wordGradientQueueTopics = new Array[Int](wordGradientSize)
    var wordGradientQueueTypes = new Array[Int](wordGradientSize)
    var wordGradientLimit = 0 //Current pointer, reset after each batch

    //Counters, for fun!
    var docsProcessed = 0
    var numSparseBucket = 0
    var numDocBucket = 0
    def totalSamples = numSparseBucket + numDocBucket

    //Samples changes for last run, this should go down with #of iterations
    var currSamples = 0
    var currChanges = 0

    var scale = 1.0   // pi_t
    var iteration = 0 // t

    //Don't measure time for first 10 iterations
    val skipIterations = 10

    var startTime:Long = 0
    var docTopics = new Array[Int](docTopicsSize) // z assignment for the tokens
    val docTopicCounts = new Array[Int](numTopics) //Ndk, local to the document

    while (iteration < numBatches) {
      if (iteration == skipIterations) startTime = System.currentTimeMillis()

      //This depends upon scale, hence needs to be update for every iteration
      for (topic <- 0 until numTopics) topicNormalizers(topic) = 1.0 / (betaSum + scale * topicTokenTotals(topic) - 0.5)

      for (d <- 0 until batchSize) {
        val ws: CategoricalSeqVariable[String] = docProvider.getRandomDocument()
        val numTokens = ws.length

        //Do we have enough space in docTopics?
        if (numTokens > docTopicsSize) {
          docTopicsSize *= 2
          docTopics = new Array[Int](docTopicsSize)
          println("resize docTopics: "+ numTokens + " "+ docTopicsSize)
        } else {
          java.util.Arrays.fill(docTopics, 0)
        }

        if (wordGradientLimit + numTokens > wordGradientSize) {
          wordGradientSize *= 2
          val newWordGradientQueueTopics = new Array[Int](wordGradientSize)
          val newWordGradientQueueTypes = new Array[Int](wordGradientSize)
          System.arraycopy(wordGradientQueueTopics, 0, newWordGradientQueueTopics, 0, wordGradientQueueTopics.length)
          System.arraycopy(wordGradientQueueTypes, 0, newWordGradientQueueTypes, 0, wordGradientQueueTypes.length)
          println("resize wordGradQueue: "+ wordGradientSize + " "+ wordGradientLimit)
        }

        java.util.Arrays.fill(docTopicCounts, 0)

        var coefficientSum = 0.0
        for (topic <- 0 until numTopics) {
          topicCoefficients(topic) = (alpha + docTopicCounts(topic)) * topicNormalizers(topic)
          coefficientSum += topicCoefficients(topic)
        }

        for (sweep <- 0 until numSamples) {
          var wp = 0
          while (wp < ws.length) {
            val word = ws.intValue(wp)
            val oldTopic = docTopics(wp)

            // We only get Ndk after the first sweep
            if (sweep > 0) {
              docTopicCounts(oldTopic) -= 1
              coefficientSum -= topicCoefficients(oldTopic)
              topicCoefficients(oldTopic) = (alpha + docTopicCounts(oldTopic)) * topicNormalizers(oldTopic)
              coefficientSum += topicCoefficients(oldTopic)
            }

            // typeWeights is sparse, almost-sorted list
            val currentTypeWeights = typeWeights(word)
            val currentTypeTopics = typeTopics(word)

            var samplingLimit = 0
            var sparseSamplingSum = 0.0
            while(samplingLimit < currentTypeWeights.length && currentTypeWeights(samplingLimit) > 0.0) {
              val topic = currentTypeTopics(samplingLimit)

              //We have a big value, approximate exp(digamma(x)) by x - 0.5
              if (scale * currentTypeWeights(samplingLimit) > 5.0) {
                samplingWeights(samplingLimit) = (beta + scale * currentTypeWeights(samplingLimit) - 0.5 - expDiGammaBeta) * topicNormalizers(topic)
              }
              else {
                val appx = approximateExpDigamma(beta + scale * currentTypeWeights(samplingLimit))
                samplingWeights(samplingLimit) = (appx - expDiGammaBeta) * topicNormalizers(topic)
              }
              sparseSamplingSum += samplingWeights(samplingLimit)
              samplingLimit += 1
            }

            val Z = sparseSamplingSum + expDiGammaBeta * coefficientSum
            val origSample = Z * random.nextDouble()
            var sample = origSample
            var newTopic = 0

            if (sample < sparseSamplingSum) {
              var index = 0

              while (sample > samplingWeights(index)) {
                sample -= samplingWeights(index)
                index += 1
              }

              newTopic = currentTypeTopics(index)
              numSparseBucket += 1
            }
            else {
              numDocBucket += 1
              sample = (sample - sparseSamplingSum) / expDiGammaBeta

              newTopic = 0
              while (sample > topicCoefficients(newTopic)) {
                sample -= topicCoefficients(newTopic)
                newTopic += 1
              }
            }

            //Keeps track of how many changes are happening per samples
            if (sweep > burninSamples) {
              currSamples += 1
              if (oldTopic != newTopic) currChanges += 1
            }

            //We already decremented for oldTopic, thus update here no matter what
            docTopicCounts(newTopic) +=1
            coefficientSum -= topicCoefficients(newTopic)
            topicCoefficients(newTopic) = (alpha + docTopicCounts(newTopic)) * topicNormalizers(newTopic)
            coefficientSum += topicCoefficients(newTopic)

            if (oldTopic != newTopic) {
              docTopics(wp) = newTopic
              wordGradientQueueTopics(wordGradientLimit) = newTopic
              wordGradientQueueTypes(wordGradientLimit) = word
              wordGradientLimit +=1
            }
            wp += 1
          }
        }
      }

      var rowT: Double = math.pow(learningRate + iteration, -kappa)
      scale *=  (1.0 - rowT)

      //wordWeight is contribution of one member, this does not depend on any other value
      val wordWeight:Double = (rowT * wordWeightConstant) / scale // Nkw += (p_t * D) / (B * pi_t * numSamples) Nkw^s

      //Update word weights, go through all the samples.
      for (s <- 0 until wordGradientLimit) {
        val word = wordGradientQueueTypes(s)
        val topic = wordGradientQueueTopics(s)

        val weights = typeWeights(word)
        val topics = typeTopics(word)

        var index = 0
        while (topics(index) != topic && weights(index) > 0.0) index += 1

        topics(index) = topic
        weights(index) += wordWeight
        topicTokenTotals(topic) += wordWeight
      }

      //Reset!
      wordGradientLimit = 0

      if (scale < 0.01) {
        println("Rescale: "+ iteration)
        rescale(scale)
        scale = 1.0
        sortAndPrune(0.1)
      }

      //if ((iteration % 10) == 1) println("iteration: "+ iteration)

      if (iteration % 100 == 0) {
        println("iteration: "+ iteration +" Changes:"+ currChanges + " / "+ currSamples + " TotalSamples: " + numSparseBucket + "," +numDocBucket + "( "+ totalSamples+ ")")
        export()
        println(topicsSummary(10))
        currChanges = 0
        currSamples = 0
      }

      iteration += 1
      docsProcessed += batchSize
    }

    val endTime = System.currentTimeMillis()
    println("Time for "+ (numBatches - skipIterations)+ " iterations: "+ (endTime - startTime) / 1000 + "s")
  }

  def topicWords(topicIndex:Int, numWords:Int = 10): Seq[String] = phis(topicIndex).tensor.top(numWords).map(dp => wordDomain.category(dp.index))
  def topicWordsArray(topicIndex:Int, numWords:Int): Array[String] = topicWords(topicIndex, numWords).toArray
  def topicSummary(topicIndex:Int, numWords:Int = 10): String = "Topic %3d %s  %d".format(topicIndex, (topicWords(topicIndex, numWords).mkString(" ")), phis(topicIndex).tensor.massTotal.toInt)
  def topicsSummary(numWords:Int = 10): String = Range(0, numTopics).map(topicSummary(_, numWords)).mkString("\n")

  def sortAndPrune(cutoff: Double) {
    for (word <- 0 until numTypes) {
      val weights = typeWeights(word)
      val topics = typeTopics(word)

      var sortedLimit = 0

      //TODO: These counts always increase! Why?
      while (sortedLimit < numTopics && weights(sortedLimit) > 0.0) {
        if (weights(sortedLimit) < cutoff) {
          weights(sortedLimit) = 0.0
          topics(sortedLimit) = 0
        }
        else {
          var i = sortedLimit - 1
          while (i >= 0 && weights(i+1) > weights(i)) {
            val tempTopic = topics(i)
            val tempWeight = weights(i)

            weights(i) = weights(i+1)
            topics(i) = topics(i+1)

            weights(i+1) = tempWeight
            topics(i+1) = tempTopic
            i = i - 1
          }
        }
        sortedLimit += 1
      }
    }
  }

  //This is to prevent underflow,
  def rescale(scale: Double) {
    for (word <- 0 until numTypes) {
      val weights = typeWeights(word)
      for (topic <- 0 until numTopics) weights(topic) *= scale
    }
    for (topic <- 0 until numTopics) topicTokenTotals(topic) *= scale
  }
}

object SparseOnlineLDA {

  def main(args:Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val numTopics =     new CmdOption("num-topics", 't', 1500, "N", "Number of topics.")
      val batchSize =     new CmdOption("batch-size", 'b', 200, "N", "Number of documents to be process in one go")
      val alpha =         new CmdOption("alpha", 0.1, "N", "Dirichlet parameter for per-document topic proportions.")
      val beta =          new CmdOption("beta", 0.1, "N", "Dirichlet parameter for per-topic word proportions.")
      val numSamples =    new CmdOption("num-samples", 'i', 5, "N", "Number of sweeps.")
      val readLines =     new CmdOption("read-lines", "", "FILENAME", "File containing lines of text, one for each document.")
      val maxNumDocs =    new CmdOption("max-num-docs", Int.MaxValue, "N", "The maximum number of documents to read.")
      val printTopics =   new CmdOption("print-topics", 20, "N", "Just before exiting print top N words for each topic.")
      val numBatches =    new CmdOption("num-batches", 5000, "N", "Num batches to process")
      val burninSamples = new CmdOption("burn-in-samples", 2, "N", "Burn in samples")
      val learningRate =  new CmdOption("learning-rate", 100.0, "N", "Burn in samples")
      val kappa =         new CmdOption("kappa", 0.6, "N", "learning rate exponent")
    }

    opts.parse(args)
    val numTopics     = opts.numTopics.value
    val alpha         = opts.alpha.value
    val beta          = opts.beta.value
    val batchSize     = opts.batchSize.value
    val numSamples    = opts.numSamples.value
    val burninSamples = opts.burninSamples.value
    val learningRate  = opts.learningRate.value
    val kappa         = opts.kappa.value

    val docProvider = new DocumentProvider(opts.readLines.value)
    docProvider.processDocuments()

    val lda = new SparseOnlineLDA(docProvider, numTopics, alpha, beta, batchSize, numSamples, burninSamples, learningRate, kappa, opts.numBatches.value)
    lda.train()
  }
}