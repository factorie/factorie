package cc.factorie.app.topics.lda

import cc.factorie._
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import cc.factorie.maths.digamma
import cc.factorie.directed._
import cc.factorie.la.DenseTensor1
import java.io.{File, PrintWriter}
import scala.util.matching.Regex
import cc.factorie.variable._

/* Implementation for Sparse Stochastic inference by Mimno et.al */
class SparseOnlineLDA(val wordDomain: CategoricalDomain[String],
                      val numDocs: Int,
                      val numTopics: Int = 10,
                      val alpha: Double = 0.1,
                      val beta: Double = 0.1,
                      val batchSize: Int = 100,
                      val numSamples: Int = 5,
                      val burninSamples: Int = 2,
                      val initLearningRate: Double = 100.0,
                      val kappa: Double = 0.6,
                      val maxIterations: Int=2000,
                      val printTopicInterval: Int=10,
                      val topicsFileName: String = "lda.topics")(implicit val random:scala.util.Random)
{
  implicit val model = DirectedModel()

  val numTypes = wordDomain.length

  val betas = MassesVariable.growableUniform(wordDomain, beta)
  val phis = Mixture(numTopics)(ProportionsVariable.growableDense(wordDomain) ~ Dirichlet(betas))

  val typeWeights = Array.tabulate(numTypes)(t => new Array[Double](numTopics))
  val typeTopics = Array.tabulate(numTypes)(t => new Array[Int](numTopics))

  val Nk = new Array[Double](numTopics) // n_t
  val infoMsg = "numTopics: %d numTypes: %d numDocs: %d maxIterations: %d"
  println(infoMsg.format(numTopics, numTypes, numDocs, maxIterations))

  var wordGradientSize = 500000
  var maxTokens = 50000

  //Store the constants
  val expDiGammaBeta:Double = Math.exp(digamma(beta)) // exp(digamma(beta))
  val betaSum = beta * numTypes // (V * beta)
  val wordWeightConstant = numDocs / (batchSize * numSamples) // D / (S * B)

  val topicNormalizers = new Array[Double](numTopics)  // represent expDiGamma(beta * V + Nk)
  val topicCoefficients = new Array[Double](numTopics) // represent alpha + Ndk * topicNormalizer

  val samplingWeights = new Array[Double](numTopics)  // represent Part 1 of Eq15

  //Arrays to note what we sampled for the current batch
  var wordGradientQueueTopics = new Array[Int](wordGradientSize)
  var wordGradientQueueTypes = new Array[Int](wordGradientSize)
  var wordGradientLimit = 0 //Current pointer, reset after each batch

  var zs = new Array[Int](maxTokens) // z assignment for the tokens
  val Ndk = new Array[Int](numTopics) //Ndk, local to the document

  var docsProcessed = 0
  //Don't measure time for first 10 iterations
  val skipIterations = 10
  var scale = 1.0

  var currSamples = 0
  var currChanges = 0

  def approximateExpDigamma(x: Double) = {
    var correction = 0.0
    var y = x

    while (y < 5.0) {
      correction += 1.0 / y
      y += 1.0
    }

    (y - 0.5) * Math.exp(-correction)
  }

  def export(): Unit = {
    phis.foreach(_.value.zero())
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

  def train(ws:CategoricalSeqVariable[String]) {
    val numTokens = ws.length

    //Do we have enough space in docTopics?
    if (numTokens > maxTokens) {
      maxTokens *= 2
      zs = new Array[Int](maxTokens)
      println("resize docTopics: "+ numTokens + " "+ maxTokens)
    } else {
      java.util.Arrays.fill(zs, 0)
    }

    if (wordGradientLimit + numTokens > wordGradientSize) {
      wordGradientSize *= 2
      val newWordGradientQueueTopics = new Array[Int](wordGradientSize)
      val newWordGradientQueueTypes = new Array[Int](wordGradientSize)
      System.arraycopy(wordGradientQueueTopics, 0, newWordGradientQueueTopics, 0, wordGradientQueueTopics.length)
      System.arraycopy(wordGradientQueueTypes, 0, newWordGradientQueueTypes, 0, wordGradientQueueTypes.length)
      println("resize wordGradQueue: "+ wordGradientSize + " "+ wordGradientLimit)
    }

    java.util.Arrays.fill(Ndk, 0)

    var coefficientSum = 0.0
    var currTopic = 0
    while (currTopic < numTopics) {
      topicCoefficients(currTopic) = (alpha + Ndk(currTopic)) * topicNormalizers(currTopic)
      coefficientSum += topicCoefficients(currTopic)
      currTopic += 1
    }

    for (sweep <- 0 until numSamples) {
      var currentTokenIndex = 0
      while (currentTokenIndex < ws.length) {
        val word = ws.intValue(currentTokenIndex)
        val oldTopic = zs(currentTokenIndex)

        // We only get Ndk after the first sweep
        if (sweep > 0) {
          Ndk(oldTopic) -= 1
          coefficientSum -= topicCoefficients(oldTopic)
          topicCoefficients(oldTopic) = (alpha + Ndk(oldTopic)) * topicNormalizers(oldTopic)
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
            samplingWeights(samplingLimit) = (beta + scale * currentTypeWeights(samplingLimit) - 0.5 - expDiGammaBeta) * topicCoefficients(topic)
          }
          else {
            val appx = approximateExpDigamma(beta + scale * currentTypeWeights(samplingLimit))
            samplingWeights(samplingLimit) = (appx - expDiGammaBeta) * topicCoefficients(topic)
          }
          sparseSamplingSum += samplingWeights(samplingLimit)
          samplingLimit += 1
        }

        val Z = sparseSamplingSum + coefficientSum * expDiGammaBeta
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
        }
        else {
          sample = (sample - sparseSamplingSum) / expDiGammaBeta

          newTopic = 0
          while (sample > topicCoefficients(newTopic)) {
            sample -= topicCoefficients(newTopic)
            newTopic += 1
          }
        }

        //Keeps track of how many changes are happening per samples
        if (sweep >= burninSamples) {
          currSamples += 1
          if (oldTopic != newTopic) currChanges += 1
        }

        //We already decremented for oldTopic, thus update here no matter what
        Ndk(newTopic) +=1
        coefficientSum -= topicCoefficients(newTopic)
        topicCoefficients(newTopic) = (alpha + Ndk(newTopic)) * topicNormalizers(newTopic)
        coefficientSum += topicCoefficients(newTopic)

        zs(currentTokenIndex) = newTopic
        wordGradientQueueTopics(wordGradientLimit) = newTopic
        wordGradientQueueTypes(wordGradientLimit) = word
        wordGradientLimit +=1

        currentTokenIndex += 1
      }
    }
  }

  def train(batchDocs:Array[CategoricalSeqVariable[String]], iteration:Int) {
    wordGradientLimit = 0
    currSamples = 0
    currChanges = 0

    var currTopic = 0
    while (currTopic < numTopics) {
      topicNormalizers(currTopic) = 1.0 / (betaSum + scale * Nk(currTopic) - 0.5)
      currTopic += 1
    }

    var doc = 0
    while(doc < batchSize) {
      train(batchDocs(doc))
      doc += 1
    }

    val learningRate: Double = math.pow(initLearningRate + iteration, -kappa)
    scale *=  (1.0 - learningRate)
    val wordWeight:Double = (learningRate * wordWeightConstant) / scale // Nkw += (p_t * D) / (B * pi_t * numSamples) Nkw^s

    //Update word weights, go through all the samples.
    var sampleNum = 0
    while (sampleNum < wordGradientLimit) {
      val word = wordGradientQueueTypes(sampleNum)
      val topic = wordGradientQueueTopics(sampleNum)

      val weights = typeWeights(word)
      val topics = typeTopics(word)

      var index = 0
      while (topics(index) != topic && weights(index) > 0.0) index += 1

      topics(index) = topic
      weights(index) += wordWeight
      Nk(topic) += wordWeight

      sampleNum += 1
    }
  }

  def train(docs: Stream[CategoricalSeqVariable[String]]) {
    var iteration = 0
    var startTime:Long = 0

    val batchDocs = new Array[CategoricalSeqVariable[String]](batchSize)
    var docNum = 0

    while (iteration < maxIterations) {
      if (iteration == skipIterations) startTime = System.currentTimeMillis()

      var batchIndex = 0
      while (batchIndex < batchSize) {
        batchDocs(batchIndex) = docs(docNum)
        docNum += 1
        batchIndex += 1
      }

      train(batchDocs, iteration)

      if (scale < 0.01) {
        println("Rescale: "+ iteration)
        rescale(scale)
        scale = 1.0
        sortAndPrune(0.1)
      }

      if (iteration % printTopicInterval == 1) {
        val iterationMsg = "Changes/Samples: %d/%d Rate:%f scale: "
        println(iterationMsg.format(currChanges, currSamples, (currChanges + 0.0)/currSamples) + scale)
        export()
        println(topicsSummary(10))
      }

      iteration += 1
      docsProcessed += batchSize
    }

    val endTime = System.currentTimeMillis()
    val timeMsg = "Total time for %d iterations:%d s Docs Processed: %d\n"
    println(timeMsg.format(maxIterations - skipIterations, (endTime - startTime)/1000, docsProcessed))

    export()
    val topicsWriter = new PrintWriter(topicsFileName)
    topicsWriter.write(topicsSummary(50))
    topicsWriter.close()
  }

  def topicWords(topicIndex:Int, numWords:Int = 10): Seq[String] = phis(topicIndex).value.top(numWords).map(dp => wordDomain.category(dp.index))
  def topicWordsArray(topicIndex:Int, numWords:Int): Array[String] = topicWords(topicIndex, numWords).toArray
  def topicSummary(topicIndex:Int, numWords:Int = 10): String = "Topic %3d %s  %d".format(topicIndex, topicWords(topicIndex, numWords).mkString(" "), phis(topicIndex).value.massTotal.toInt)
  def topicsSummary(numWords:Int = 10): String = Range(0, numTopics).map(topicSummary(_, numWords)).mkString("\n")

  def sortAndPrune(cutoff: Double) {
    var currWord = 0

    while (currWord < numTypes) {
      val weights = typeWeights(currWord)
      val topics = typeTopics(currWord)

      var sortedLimit = 0

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
      currWord += 1
    }
  }

  //This is to prevent underflow,
  def rescale(scale: Double) {
    var currWord = 0
    while (currWord < numTypes) {
      val weights = typeWeights(currWord)

      var currTopic = 0
      while (weights(currTopic) > 0.0 && currTopic < numTopics) {
        weights(currTopic) *= scale
        currTopic +=1
      }
      currWord +=1
    }

    var currTopic = 0
    while (currTopic < numTopics)  {
      Nk(currTopic) *= scale
      currTopic += 1
    }
  }
}

object SparseOnlineLDA {
  val docBuffer = new ArrayBuffer[Doc]
  val minDocLength = 3
  var numDocs = 0

  object WordSeqDomain extends CategoricalSeqDomain[String]
  val tokenRegex = new Regex("\\p{Alpha}+")
  val mySegmenter = new cc.factorie.app.strings.RegexSegmenter(tokenRegex)

  def nextDocument(): Stream[CategoricalSeqVariable[String]] = {
    Stream.cons(getRandomDocument(), nextDocument())
  }

  //Adds documents and returns the word domain
  def initializeDocuments(fileName:String): Stream[CategoricalSeqVariable[String]] = {
    val source = scala.io.Source.fromFile(new File(fileName))
    var count = 0
    for (line <- source.getLines()) {
      val text: String = line
      val doc = Document.fromString(WordSeqDomain, fileName +":"+count, text, segmenter = mySegmenter)
      if (doc.length >= minDocLength) docBuffer += doc
      count += 1
      if (count % 1000 == 0) { print(" "+count); Console.flush() }; if (count % 10000 == 0) println()
    }
    source.close()
    numDocs = docBuffer.length
    nextDocument()
  }

  def getRandomDocument(): CategoricalSeqVariable[String] = {
    val docIndex = random.nextInt(numDocs)
    docBuffer(docIndex).ws
  }

  def main(args:Array[String]): Unit = {
    object opts extends cc.factorie.util.DefaultCmdOptions {
      val numTopics =     new CmdOption("num-topics", 't', 10, "N", "Number of topics.")
      val batchSize =     new CmdOption("batch-size", 'b', 100, "N", "Number of documents to be process in one go")
      val alpha =         new CmdOption("alpha", 0.1, "N", "Dirichlet parameter for per-document topic proportions.")
      val beta =          new CmdOption("beta", 0.1, "N", "Dirichlet parameter for per-topic word proportions.")
      val numSamples =    new CmdOption("num-samples", 'i', 5, "N", "Number of sweeps.")
      val readLines =     new CmdOption("read-lines", "", "FILENAME", "File containing lines of text, one for each document.")
      val maxNumDocs =    new CmdOption("max-num-docs", Int.MaxValue, "N", "The maximum number of documents to read.")
      val numBatches =    new CmdOption("num-batches", 5000, "N", "Num batches to process")
      val burninSamples = new CmdOption("burn-in-samples", 2, "N", "Burn in samples")
      val initLearningRate =  new CmdOption("init-learning-rate", 100.0, "N", "initial learning Rate: 1.0 / [this value] + iteration")
      val kappa =         new CmdOption("kappa", 0.6, "N", "learning rate exponent: exp(rowT, kappa)")
    }

    opts.parse(args)
    val numTopics     = opts.numTopics.value
    val alpha         = opts.alpha.value
    val beta          = opts.beta.value
    val batchSize     = opts.batchSize.value
    val numSamples    = opts.numSamples.value
    val burninSamples = opts.burninSamples.value
    val learningRate  = opts.initLearningRate.value
    val kappa         = opts.kappa.value

    implicit val random = new scala.util.Random(1)

    val docs:Stream[CategoricalSeqVariable[String]] = initializeDocuments(opts.readLines.value)
    val wordDomain = WordSeqDomain.elementDomain

    val lda = new SparseOnlineLDA(wordDomain, numDocs, numTopics, alpha, beta, batchSize, numSamples, burninSamples, learningRate, kappa, opts.numBatches.value)
    lda.train(docs)
  }
}