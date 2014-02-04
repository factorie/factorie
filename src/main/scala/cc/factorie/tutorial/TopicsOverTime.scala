package cc.factorie.tutorial
import scala.collection.mutable.ArrayBuffer
import java.io.File
import cc.factorie._
import cc.factorie.la._
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.app.topics.lda.SparseLDAInferencer
import cc.factorie.directed._
import cc.factorie.variable._

// A fast approximation to Topics-over-Time that leverages SparseLDAInferencer.
// Estimate a per-topic Beta distribution over normalized time stamps.
// Change alpha on a per-document basis to reflect a scaled version of this Beta distribution.
object TopicsOverTime {
  val numTopics = 100
  val beta1 = 0.1
  val alpha1 = 0.1
  val fitDirichlet = false
  implicit val random = new scala.util.Random(0)

  object ZDomain extends DiscreteDomain(numTopics)
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(len:Int) extends DiscreteSeqVariable(len) { def domain = ZSeqDomain }
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val WordDomain = WordSeqDomain.elementDomain
  class Document(name:String, myTheta:ProportionsVariable, myZs:Zs, words:Seq[String]) extends cc.factorie.app.topics.lda.Document(WordSeqDomain, name, words) {
    this.theta = myTheta
    this.zs = myZs
    var timeStamp: Double = -1.0
  }
  val beta = MassesVariable.growableUniform(WordDomain, beta1)
  val alphas = MassesVariable.dense(numTopics, alpha1)
  val timeAlphas = new Array[Double](numTopics)
  val timeBetas  = new Array[Double](numTopics)
  val timeMeans = new DenseTensor1(numTopics)
  implicit val model = DirectedModel()
  
  def estimateTopicTimes(documents:Seq[Document]): Unit = {
    val topic2times = Array.tabulate(numTopics)(i => new cc.factorie.util.DoubleArrayBuffer)
    for (doc <- documents) {
      for (i <- 0 until doc.length) {
        if (!doc.timeStamp.isNaN)
          topic2times(doc.zs.intValue(i)) += doc.timeStamp
      }
    }
    val topic2mean = Array.tabulate(numTopics)(i => if (topic2times(i).length > 1) maths.sampleMean(topic2times(i)) else 0.5)
    val topic2variance = Array.tabulate(numTopics)(i => if (topic2times(i).length > 1) maths.sampleVariance(topic2times(i), topic2mean(i)) else 0.25)
    timeMeans := topic2mean
    for (i <- 0 until numTopics) {
      timeAlphas(i) = MaximizeBetaByMomentMatching.maxAlpha(topic2mean(i), topic2variance(i))
      timeBetas(i) =  MaximizeBetaByMomentMatching.maxBeta(topic2mean(i), topic2variance(i))
    }
  }

  def main(args: Array[String]): Unit = {
    val directories = 
      if (args.length > 0) args.toList 
      else if (true) List("12", "11", "10", "09", "08", "07").take(99).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      else if (false) List("acq", "earn", "money-fx").map("/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/"+_)
      else List("comp.graphics", "comp.os.ms-windows.misc", "comp.sys.ibm.pc.hardware", "comp.sys.mac.hardware").map("/Users/mccallum/research/data/text/20_newsgroups/"+_)
    val phis = Mixture(numTopics)(ProportionsVariable.growableDense(WordDomain) ~ Dirichlet(beta))
    val documents = new ArrayBuffer[Document]
    val stopwords = new Stopwords; stopwords += "rainbownum"
    for (directory <- directories) {
      for (file <- new File(directory).listFiles; if file.isFile) {
        val theta = ProportionsVariable.sortedSparseCounts(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_.toLowerCase).filter(!stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        val doc = new Document(file.toString, theta, zs, tokens) ~ PlatedCategoricalMixture(phis, zs)
        doc.time = file.lastModified
        documents += doc
      }
    }

    // Now that we have the full min-max range of dates, set the doc.stamps values to a 0-1 normalized value
    val times = documents.map(_.time)
    val maxTime = times.max
    val minTime: Double = times.min
    val timeRange: Double = maxTime- minTime
    // given 0<=x<=1, return a value y<= <=(1-y) 
    def squeeze(x:Double, y:Double): Double = x * (1-y) + y
    documents.foreach(doc => doc.timeStamp = squeeze((doc.time - minTime) / timeRange, .2))
    estimateTopicTimes(documents)
        
    val sampler = SparseLDAInferencer(ZDomain, WordDomain, documents, alphas.value, beta1, model)

    for (i <- 1 to 30) {
      for (doc <- documents) {
        val timeSmoothing = Tensor.tabulate(numTopics)(i => { val m = timeMeans(i) + 0.5; m*m*m*m*m*m })
        sampler.resetSmoothing(alphas.value + (timeSmoothing * 3.0), beta1)
        sampler.process(doc.zs)
      }
      if (i % 5 == 0) {
        sampler.export(phis)
        if (fitDirichlet) {
          sampler.exportThetas(documents)
          MaximizeDirichletByMomentMatching(alphas, model)
          sampler.resetSmoothing(alphas.value, beta1)
        } else {
          estimateTopicTimes(documents)
        }
      }
    }
  }
  
  
}

