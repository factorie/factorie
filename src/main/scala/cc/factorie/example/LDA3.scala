package cc.factorie.example
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}
import scala.util.matching.Regex
import scala.io.Source
import java.io.File
import cc.factorie._
import cc.factorie.generative._
import cc.factorie.app.strings.Stopwords
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.util.DoubleSeq

object LDA3 {
  val numTopics = 15
  val beta1 = 0.1
  val alpha1 = 0.1
  val fitDirichlet = false
  cc.factorie.random.setSeed(0)
  
  implicit val model = GenerativeModel()
  object ZDomain extends DiscreteDomain(numTopics)
  object ZSeqDomain extends DiscreteSeqDomain { def elementDomain = ZDomain }
  class Zs(len:Int) extends DiscreteSeqVariable(len) { 
    def domain = ZSeqDomain
    def words: Words = model.childFactors(this).first.asInstanceOf[PlatedDiscreteMixture.Factor]._1.asInstanceOf[Words]
  }
  object WordSeqDomain extends CategoricalSeqDomain[String]
  val WordDomain = WordSeqDomain.elementDomain
  class Words(strings:Seq[String]) extends CategoricalSeqVariable(strings) {
    def domain = WordSeqDomain
    def zs = model.parentFactor(this).asInstanceOf[PlatedDiscreteMixture.Factor]._3.asInstanceOf[Zs]
  }
  class Document(val file:String, val theta:ProportionsVar, strings:Seq[String]) extends Words(strings)
  val beta = MassesVariable.growableUniform(WordDomain, beta1)
  val alphas = MassesVariable.dense(numTopics, alpha1)

  def main(args: Array[String]): Unit = {
    val directories = 
      if (args.length > 0) args.toList 
      else if (true) List("11", "12", "10", "09", "08").take(4).map("/Users/mccallum/research/data/text/nipstxt/nips"+_)
      else if (false) List("acq", "earn", "money-fx").map("/Users/mccallum/research/data/text/reuters/reuters-parsed/modapte/"+_)
      else List("comp.graphics", "comp.os.ms-windows.misc", "comp.sys.ibm.pc.hardware", "comp.sys.mac.hardware").map("/Users/mccallum/research/data/text/20_newsgroups/"+_)
    val phis = Mixture(numTopics)(ProportionsVariable.growableDense(WordDomain) ~ Dirichlet(beta))
    val documents = new ArrayBuffer[Document]
    val stopwords = new Stopwords; stopwords += "rainbownum"
    for (directory <- directories) {
      println("Reading files from directory " + directory)
      for (file <- new File(directory).listFiles; if (file.isFile)) {
        print("."); Console.flush
        val theta = ProportionsVariable.sortedSparseCounts(numTopics) ~ Dirichlet(alphas)
        val tokens = alphaSegmenter(file).map(_ toLowerCase).filter(!stopwords.contains(_)).toSeq
        val zs = new Zs(tokens.length) :~ PlatedDiscrete(theta)
        documents += new Document(file.toString, theta, tokens) ~ PlatedDiscreteMixture(phis, zs)
      }
      println()
    }
    println("Read "+documents.size+" documents, "+WordDomain.size+" word types, "+documents.map(_.length).sum+" word tokens.")
    
    //val collapse = new ArrayBuffer[Variable]
    //collapse += phis
    //collapse ++= documents.map(_.theta)
    //val sampler = new CollapsedGibbsSampler(collapse) { def export(m:Seq[Proportions]): Unit = {} }
    val sampler = new SparseLDAInferencer(numTopics, documents, alphas.tensor, beta1)

    val startTime = System.currentTimeMillis
    for (i <- 1 to 30) {
      for (doc <- documents) sampler.process(doc.zs)
      if (i % 5 == 0) {
        println("Iteration " + i)
        sampler.export(phis)
        if (fitDirichlet) {
          sampler.exportThetas(documents)
          MaximizeDirichletByMomentMatching(alphas, model)
          sampler.resetSmoothing(alphas.tensor, beta1)
          println("alpha = " + alphas.tensor.toSeq.mkString(" "))
          phis.zipWithIndex.map({case (phi:ProportionsVar, index:Int) => (phi, alphas(index))}).sortBy(_._2).map(_._1).reverse.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
        } else {
          phis.foreach(t => println("Topic " + phis.indexOf(t) + "  " + t.tensor.top(10).map(dp => WordDomain.category(dp.index)).mkString(" ")+"  "+t.tensor.massTotal.toInt+"  "+alphas(phis.indexOf(t))))
        }
        println
      }
    }
    //phis.foreach(t => {println("\nTopic "+phis.indexOf(t)); t.top(20).foreach(x => println("%-16s %f".format(x.value,x.pr)))})
    println("Finished in " + ((System.currentTimeMillis - startTime) / 1000.0) + " seconds")
  }
  
  class SparseLDAInferencer(val numTopics:Int, docs:Iterable[Document], initialAlphas:DoubleSeq, initialBeta1:Double) {
    var verbosity = 0
    var smoothingOnlyCount = 0; var topicBetaCount = 0; var topicTermCount = 0 // Just diagnostics
    def samplesCount = smoothingOnlyCount + topicBetaCount + topicTermCount
    val betaSum = beta1 * WordDomain.size
    private var alphas: Array[Double] = null
    private var beta1: Double = 0.1
    var smoothingMass: Double = 0.0
    private val cachedCoefficients = new Array[Double](numTopics)

    // Create and populate the (word,topic) counts
    val phiCounts = new DiscreteMixtureCounts(WordDomain, ZDomain)

    for (doc <- docs) addDocument(doc) 
    if (verbosity > 0) println("Finished initializing phiCounts")
    if (verbosity > 0) println("nt "+phiCounts.mixtureCounts.mkString(" "))
    // Initialize alphas, beta1, smoothingMass and cachedCoefficients; must be done after phiCounts initialized
    resetSmoothing(initialAlphas, initialBeta1)

    def addDocument(doc:Document): Unit =
      phiCounts.incrementFactor(model.parentFactor(doc).asInstanceOf[PlatedDiscreteMixture.Factor], 1)

    def resetSmoothing(newAlphas:DoubleSeq, newBeta1:Double): Unit = {
      require(numTopics == newAlphas.length)
      alphas = newAlphas.toArray
      beta1 = newBeta1
      // s = \sum_t ( \alpha_t \beta ) / ( |V| \beta + n_t )  [Mimno "Sparse LDA"]
      smoothingMass = (0 until numTopics).foldLeft(0.0)((sum,t) => sum + (alphas(t) * beta1 / (phiCounts.mixtureCounts(t) + betaSum)))
      // The first term in the per-topic summand for q, just missing *n_{w|t}  [Mimno "Sparse LDA"]
      forIndex(numTopics)(t => cachedCoefficients(t) = alphas(t) / (phiCounts.mixtureCounts(t) + betaSum))
    }
    
    private def recalcSmoothingMass: Double =  
      (0 until numTopics).foldLeft(0.0)((sum,t) => sum + (alphas(t) * beta1 / (phiCounts.mixtureCounts(t) + betaSum)))

    def export(phis:Seq[ProportionsVar]): Unit = {
      phis.foreach(_.tensor.zero())
      for (wi <- 0 until WordDomain.size)
        phiCounts(wi).forCounts((ti,count) => phis(ti).tensor.+=(wi, count))
    }
    
    def exportThetas(docs:Seq[Document]): Unit = {
      for (doc <- docs) {
        val theta = doc.theta
        theta.tensor.zero()
        val zis = doc.zs.intValues
        var i = zis.length - 1
        while (i >= 0) theta.tensor.+=(zis(i), 1.0)
        //for (dv <- doc.zs) theta.increment(dv.intValue, 1.0)(null)
      }
    }
    
    /** Sample the Zs for one document. */
    def process(zs:Zs): Unit = {
      // TODO In some cases "smoothingMass" seems to drift low, perhaps due to math precision in its adjustments
      // So reset it here for each document
      //val newSmoothingMass = recalcSmoothingMass
      //println(smoothingMass+" "+newSmoothingMass)
      //smoothingMass = newSmoothingMass
      //println("process doc "+zs.words.asInstanceOf[Document].file)
      val ws = zs.words
      assert(ws.length == zs.length)
      val docTopicCounts = new cc.factorie.util.SortedSparseCounts(numTopics, zs.intValues, keepDense = true)
      // r = sum_t ( \beta n_{t|d} ) ( n_t + |V| \beta )  [Mimno "Sparse LDA"]
      var topicBetaMass = 0.0
      forIndex(docTopicCounts.numPositions)(p => {
        val ti = docTopicCounts.indexAtPosition(p) // topic index
        val ntd = docTopicCounts.countAtPosition(p) // n_{t|d}
        val nt = phiCounts.mixtureCounts(ti) // {n_t}
        topicBetaMass += beta1 * ntd / (nt + betaSum)
        cachedCoefficients(ti) = (alphas(ti) + ntd) / (nt + betaSum)
      })

      //println("\ndocTopicCounts "+docTopicCounts.counts+"  "+List(smoothingOnlyCount, topicBetaCount, topicTermCount).mkString(","))
      //println("cachedCoefficients "+cachedCoefficients.mkString(" "))
      //println("nt "+phiCounts.mixtureCounts.mkString(" "))
      
      // sample each z
      forIndex(zs.length)(zp => { // z position
        val ti = zs.intValue(zp) // intValue of z, "topic index"
        //assert(ti < numTopics)
        val wi = ws.intValue(zp) // intValue of word, "word index"
        //assert(wi < WordDomain.size)
        val ntd = docTopicCounts.countOfIndex(ti) // n_{t|d}
        val nt = phiCounts.mixtureCounts(ti)
        val phiCountsWi = phiCounts(wi)
        assert(phiCountsWi.countOfIndex(ti) > 0, "ti="+ti)
        
        // Remove this token from various sufficient statistics
        docTopicCounts.incrementCountAtIndex(ti, -1)
        //phiCountsWi.incrementCountAtIndex(ti, -1)
        phiCounts.increment(wi, ti, -1)
        // Don't yet remove counts from docTopicCounts or phiCounts b/c it might involve shifting holes
        // and we are fairly likely to just re-sample zi to match its old value anyway.
        val origSmoothingMass = smoothingMass // Remember for later in case newTi == ti
        val origTopicBetaMass = topicBetaMass
        val origCachedCoefficientTi = cachedCoefficients(ti)
        smoothingMass -= alphas(ti) * beta1 / (nt + betaSum)
        smoothingMass += alphas(ti) * beta1 / ((nt-1) + betaSum); assert(smoothingMass > 0.0)
        topicBetaMass -= beta1 * ntd / (nt + betaSum)
        topicBetaMass += beta1 * (ntd-1) / ((nt-1) + betaSum)
        // Reset cachedCoefficients
        // If this is the last ti in this document and newTi != ti, then this value matches the "smoothing only" cachedCoefficient we want when we are done with this document
        cachedCoefficients(ti) = (alphas(ti) + (ntd-1)) / ((nt-1) + betaSum)

        // q = \sum_t ...  [Mimno "Sparse LDA"]
        var topicTermMass = 0.0
        val topicTermScores = new Array[Double](phiCountsWi.numPositions)
        forIndex(phiCountsWi.numPositions)(tp => {
          val ti2 = phiCountsWi.indexAtPosition(tp)
          val score = cachedCoefficients(ti2) * phiCountsWi.countAtPosition(tp)
          assert(score >= 0)
          topicTermScores(tp) = score
          topicTermMass += score
        })
        
        assert(smoothingMass > 0.0)
        assert(topicBetaMass > 0.0)
        assert(topicTermMass >= 0.0)
        val r = cc.factorie.random.nextDouble()
        var sample = r * (smoothingMass + topicBetaMass + topicTermMass)
        val origSample = sample
        //println("smoothingMass="+smoothingMass+" topicBetaMass="+topicBetaMass+" topicTermMass="+topicTermMass+" sample="+sample)
        
        var newTi = -1 // new topic index
        if (sample < topicTermMass) { // Sampling from within q  [Mimno]
          topicTermCount += 1
          var i = -1
          while (sample > 0) {
            i += 1
            sample -= topicTermScores(i)
          }
          newTi = phiCountsWi.indexAtPosition(i)
        } else if (sample < topicTermMass + topicBetaMass) { // Sampling from within r [Mimno]
          topicBetaCount += 1
          sample -= topicTermMass
          sample /= beta1
          var np = docTopicCounts.numPositions
          var i = -1 // TODO Start at 0 instead and simplify below
          while (sample > 0.0 && i+1 < np) {
            i += 1
            if (i >= docTopicCounts.numPositions) throw new Error("np="+np+" Too much mass sample="+sample+" origSample="+origSample+" smoothingMass="+smoothingMass+" topicBetaMass="+topicBetaMass+" topicTermMass="+topicTermMass)
            newTi = docTopicCounts.indexAtPosition(i)
            sample -= /*beta1 * */ docTopicCounts.countAtPosition(i) / (betaSum + phiCounts.mixtureCounts(newTi))
          }
          // Allow for rounding error, but not too much
          if (sample * beta1 > 0.00001) throw new Error("Too much mass sample="+sample+" origSample="+origSample+" smoothingMass="+smoothingMass+" topicBetaMass="+topicBetaMass+" topicTermMass="+topicTermMass)
        } else {
          smoothingOnlyCount += 1
          sample -= topicTermMass + topicBetaMass
          //sample /= beta1
          newTi = -1
          while (sample > 0) {
            newTi += 1
            if (newTi == numTopics) throw new Error("Too much mass sample="+sample+" r="+r)
            sample -= alphas(newTi) * beta1 / (betaSum + phiCounts.mixtureCounts(newTi))
          }
        }
        /*newTi = ti // TODO Remove this!!!! It undoes all sampling.
        docTopicCounts.incrementCountAtIndex(ti, -1)
        docTopicCounts.incrementCountAtIndex(newTi, 1)
        phiCountsWi.incrementCountAtIndex(ti, -1)
        phiCountsWi.incrementCountAtIndex(newTi, 1)*/
        
        if (ti != newTi) {
          //println("ti="+newTi)
          //docTopicCounts.incrementCountAtIndex(ti, -1)
          docTopicCounts.incrementCountAtIndex(newTi, 1)
          //phiCountsWi.incrementCountAtIndex(ti, -1)
          //phiCountsWi.incrementCountAtIndex(newTi, 1)
          phiCounts.increment(wi, newTi, 1)
          val newNt = phiCounts.mixtureCounts(newTi)
          val newNtd = docTopicCounts.countOfIndex(newTi) // n_{t|d}
          smoothingMass -= alphas(ti) * beta1 / ((newNt-1) + betaSum)
          smoothingMass += alphas(ti) * beta1 / (newNt + betaSum); assert(smoothingMass > 0.0)
          topicBetaMass -= beta1 * (newNtd-1) / ((newNt-1) + betaSum)
          topicBetaMass += beta1 * newNtd / (newNt + betaSum)
          cachedCoefficients(newTi) = (alphas(newTi) + newNtd) / (newNt + betaSum)
          //cachedCoefficients(ti) = (alphas(ti) + (ntd-1)) / ((nt-1) + betaSum) // Already done above
          zs.set(zp, newTi)(null)  // Set the new value of z!
        } else {
          docTopicCounts.incrementCountAtIndex(ti, 1)
          //phiCountsWi.incrementCountAtIndex(ti, 1)
          phiCounts.increment(wi, ti, 1)
          smoothingMass = origSmoothingMass
          topicBetaMass = origTopicBetaMass
          cachedCoefficients(ti) = origCachedCoefficientTi
        }
        
        //val cachedCoefficients = Array.tabulate(numTopics)(t => alphas(t) / (phiCounts.mixtureCounts(t) + betaSum))

        //println("countsTotal = "+phiCounts.countsTotal)
      })

      // Put back cachedCoefficients to only smoothing
      //forIndex(docTopicCounts.numPositions)(p => {
      forIndex(numTopics)(ti => {
        //val ti = docTopicCounts.indexAtPosition(p) // topic index
        val nt = phiCounts.mixtureCounts(ti) // {n_t}
        cachedCoefficients(ti) = alphas(ti) / (nt + betaSum)
      })

    }
    
  }
  
  
}

