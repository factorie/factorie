package cc.factorie.tutorial
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
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
object LDA5 {
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
    // println("documents "+documents.length)
    for (doc <- documents) {
      //val bs = new scala.collection.mutable.BitSet
      for (i <- 0 until doc.length) {
        //if (!bs.contains(doc.zs.intValue(i)))
        if (!doc.timeStamp.isNaN)
          topic2times(doc.zs.intValue(i)) += doc.timeStamp
        //bs += doc.zs.intValue(i)
      }
      //if (doc.length > 0) doc.theta.tensor.activeDomain.foreach(i => topic2times(i).+=(doc.timeStamp))
      //for (i <- 0 until numTopics) if (doc.theta.tensor.masses(i) > 0) { println(doc.name +" "+i); topic2times(i) += doc.timeStamp } }
    }
    // println("topic2times counts "+topic2times.map(_.length).toSeq)
    // topic2times.foreach(a => println(topic2times.indexOf(a)+"  "+a.toSeq.groupBy(a=>a).map(t => (t._1, t._2.length)).toSeq.sortBy(t => t._1)))
    val topic2mean = Array.tabulate(numTopics)(i => if (topic2times(i).length > 1) maths.sampleMean(topic2times(i)) else 0.5)
    val topic2variance = Array.tabulate(numTopics)(i => if (topic2times(i).length > 1) maths.sampleVariance(topic2times(i), topic2mean(i)) else 0.25)
    timeMeans := topic2mean
    for (i <- 0 until numTopics) {
      timeAlphas(i) = MaximizeBetaByMomentMatching.maxAlpha(topic2mean(i), topic2variance(i))
      timeBetas(i) =  MaximizeBetaByMomentMatching.maxBeta(topic2mean(i), topic2variance(i))
    }
    // println("alphas "+timeAlphas.toSeq)
    // println("betas  "+timeBetas.toSeq)
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
      // println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if file.isFile) {
        // print("."); Console.flush
        val theta = ProportionsVariable.sortedSparseCounts(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_.toLowerCase).filter(!stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        val doc = new Document(file.toString, theta, zs, tokens) ~ PlatedCategoricalMixture(phis, zs)
        doc.time = file.lastModified
        documents += doc
      }
      // println()
    }
    // println("Read "+documents.size+" documents, "+WordDomain.size+" word types, "+documents.map(_.length).sum+" word tokens.")

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

    val startTime = System.currentTimeMillis
    for (i <- 1 to 30) {
      for (doc <- documents) {
        //val timeSmoothing = Tensor.tabulate(numTopics)(i => Beta.pr(doc.timeStamp, timeAlphas(i), timeBetas(i)))
        val timeSmoothing = Tensor.tabulate(numTopics)(i => { val m = timeMeans(i) + 0.5; m*m*m*m*m*m })
        // if (doc == documents.head) println(timeSmoothing)
        sampler.resetSmoothing(alphas.value + (timeSmoothing * 3.0), beta1)
        sampler.process(doc.zs)
      }
      if (i % 5 == 0) {
        // println("Iteration " + i)
        sampler.export(phis)
        if (fitDirichlet) {
          sampler.exportThetas(documents)
          MaximizeDirichletByMomentMatching(alphas, model)
          sampler.resetSmoothing(alphas.value, beta1)
          //println("alpha = " + alphas.tensor.toSeq.mkString(" "))
          //phis.zipWithIndex.map({case (phi:ProportionsVar, index:Int) => (phi, alphas(index))}).sortBy(_._2).map(_._1).reverse.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.masses.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
        } else {
          estimateTopicTimes(documents)
          //phis.foreach(t => { val ti = phis.indexOf(t); println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.masses.massTotal.toInt+"  "+alphas(phis.indexOf(t))+" tmode="+Beta.mode(timeAlphas(ti), timeBetas(ti))+" tvariance="+Beta.variance(timeAlphas(ti), timeBetas(ti))+" tmean="+Beta.mean(timeAlphas(ti), timeBetas(ti))) })
        }
        // println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    // println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
  
  
}

