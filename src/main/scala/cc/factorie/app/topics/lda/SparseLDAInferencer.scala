package cc.factorie.app.topics.lda
import cc.factorie._
import cc.factorie.generative._

class SparseLDAInferencer(
    val zDomain:DiscreteDomain, 
    val wordDomain:CategoricalDomain[String], 
    docs:Iterable[Doc], 
    initialAlphas:Seq[Double], 
    initialBeta1:Double,
    model:GenerativeModel = defaultGenerativeModel) 
{
  var verbosity = 0
  var smoothingOnlyCount = 0; var topicBetaCount = 0; var topicTermCount = 0 // Just diagnostics
  def samplesCount = smoothingOnlyCount + topicBetaCount + topicTermCount
  private var alphas: Array[Double] = null
  private var beta1: Double = 0.01
  private var betaSum = beta1 * wordDomain.size
  var smoothingMass: Double = 0.0
  private val cachedCoefficients = new Array[Double](numTopics)
  def numTopics = zDomain.size

  // Create and populate the (word,topic) counts
  val phiCounts = new DiscreteMixtureCounts { // phi counts, indexed by (wi,ti)
    def discreteDomain = wordDomain
    def mixtureDomain = zDomain
  }
  //if (verbosity > 0) println("Starting initializing phiCounts")
  for (doc <- docs) addDocument(doc) 
  if (verbosity > 0) println("Finished initializing phiCounts")
  if (verbosity > 5) println("nt "+phiCounts.mixtureCounts.mkString(" "))
  // Initialize alphas, beta1, smoothingMass and cachedCoefficients; must be done after phiCounts initialized
  resetSmoothing(initialAlphas, initialBeta1)

  def addDocument(doc:Doc): Unit =
    phiCounts.incrementFactor(model.parentFactor(doc.ws).asInstanceOf[PlatedDiscreteMixture.Factor], 1)

  def resetSmoothing(newAlphas:Seq[Double], newBeta1:Double): Unit = {
    require(numTopics == newAlphas.length)
    require(newBeta1 > 0.0)
    require(newAlphas.forall(_ >= 0.0))
    alphas = newAlphas.toArray
    beta1 = newBeta1
    betaSum = beta1 * wordDomain.size
    // s = \sum_t ( \alpha_t \beta ) / ( |V| \beta + n_t )  [Mimno "Sparse LDA"]
    //smoothingMass = (0 until numTopics).foldLeft(0.0)((sum,t) => sum + (alphas(t) * beta1 / (phiCounts.mixtureCounts(t) + betaSum)))
    smoothingMass = recalcSmoothingMass
    assert(smoothingMass > 0.0, smoothingMass)
    // The first term in the per-topic summand for q, just missing *n_{w|t}  [Mimno "Sparse LDA"]
    forIndex(numTopics)(t => cachedCoefficients(t) = alphas(t) / (phiCounts.mixtureCounts(t) + betaSum))
  }
    
  private def recalcSmoothingMass: Double = {
    // s = \sum_t ( \alpha_t \beta ) / ( |V| \beta + n_t )  [Mimno "Sparse LDA"]
    var i = 0; var sm = 0.0
    while (i < numTopics) { sm += alphas(i) * beta1 / (phiCounts.mixtureCounts(i) + betaSum); i += 1 }
    sm
  }
      
  def export(phis:Seq[DenseCountsProportions]): Unit = {
    phis.foreach(_.zero())
    for (wi <- 0 until wordDomain.size)
      phiCounts(wi).forCounts((ti,count) => phis(ti).increment(wi, count)(null))
  }
    
  def exportThetas(docs:Iterable[Doc]): Unit = {
    for (doc <- docs) {
      val theta = doc.theta
      theta.zero()
      for (dv <- doc.zs) theta.increment(dv.intValue, 1.0)(null)
    }
  }
    
  /** Sample the Zs for one document. */
  def process(zs:DiscreteSeqVariable): Unit = {
    // TODO In some cases "smoothingMass" seems to drift low, perhaps due to math precision in its adjustments
    // So reset it here for each document
    //println(smoothingMass+" "+recalcSmoothingMass)
    smoothingMass = recalcSmoothingMass
    assert(smoothingMass > 0.0)
    //println("process doc "+zs.words.asInstanceOf[Document].file)
    val ws = model.childFactors(zs).head.asInstanceOf[PlatedDiscreteMixture.Factor]._1 //words
    assert(ws.length == zs.length)
    val docTopicCounts = new SortedSparseCounts(numTopics, zs.intValues, keepDense = true)
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
      val ntd = docTopicCounts.countOfIndex(ti); assert(ntd > 0) // n_{t|d}
      val nt = phiCounts.mixtureCounts(ti); assert(nt > 0)
      val phiCountsWi = phiCounts(wi)
      assert(phiCountsWi.countOfIndex(ti) > 0)
        
      // Remove this token from various sufficient statistics
      docTopicCounts.incrementCountAtIndex(ti, -1)
      //phiCountsWi.incrementCountAtIndex(ti, -1)
      phiCounts.increment(wi, ti, -1)
      // Don't yet remove counts from docTopicCounts or phiCounts b/c it might involve shifting holes
      // and we are fairly likely to just re-sample zi to match its old value anyway.
      val origSmoothingMass = smoothingMass; assert(smoothingMass > 0.0) // Remember for later in case newTi == ti
      val origTopicBetaMass = topicBetaMass
      val origCachedCoefficientTi = cachedCoefficients(ti)
      smoothingMass -= alphas(ti) * beta1 / (nt + betaSum)
      smoothingMass += alphas(ti) * beta1 / ((nt-1) + betaSum)
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
        
      assert(smoothingMass > 0)
      assert(topicBetaMass > 0, "topicBetaMass="+topicBetaMass+" doc.length="+zs.length)
      assert(topicTermMass >= 0)
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
        smoothingMass -= alphas(newTi) * beta1 / ((newNt-1) + betaSum)
        smoothingMass += alphas(newTi) * beta1 / (newNt + betaSum)
        if (smoothingMass <= 0.0) {
          println("smoothingMass="+smoothingMass+" alphas(ti)=%f beta1=%f newNt=%d betaSum=%f".format(alphas(ti), beta1, newNt, betaSum))
          val smoothingMass2 = (0 until numTopics).foldLeft(0.0)((sum,t) => sum + (alphas(t) * beta1 / (phiCounts.mixtureCounts(t) + betaSum)))
          println("recalc smoothingMass="+smoothingMass2)
        }
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
  