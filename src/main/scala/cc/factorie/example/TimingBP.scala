package cc.factorie.example

/**
 * User: apassos
 * Date: 2/25/12
 */

import cc.factorie._
import cc.factorie.bp._
import cc.factorie.app.nlp._
import cc.factorie.bp.optimized._
import cc.factorie.app.nlp.pos._

object TestModel extends TemplateModel {
  // Bias term on each individual label
  val biasTemplate =  new TemplateWithDotStatistics1[PosLabel] { override def statisticsDomains = Seq(PosDomain) }
  // Factor between label and observed token
  val localTemplate = new TemplateWithDotStatistics2[PosLabel,PosFeatures] {
    override def statisticsDomains = Seq(PosDomain, PosFeaturesDomain)
    def unroll1(label: PosLabel) = Factor(label, label.token.attr[PosFeatures])
    def unroll2(tf: PosFeatures) = Factor(tf.token.posLabel, tf)
  }
  // Transition factors between two successive labels
  val transTemplate = new TemplateWithDotStatistics2[PosLabel, PosLabel] { // the sparse weights are kind of worthless here
    override def statisticsDomains = Seq(PosDomain, PosFeaturesDomain)
    def unroll1(label: PosLabel) = if (label.token.hasPrev) Factor(label.token.prev.posLabel, label) else Nil
    def unroll2(label: PosLabel) = if (label.token.hasNext) Factor(label, label.token.next.posLabel) else Nil
  }

  this += localTemplate
  this += transTemplate
}


object TimingBP {
  def main(args: Array[String]) {
    val nTokenFeatures = 100
    val tokenFeaturesPerToken = 10
    val nStates = 30
    val nDocuments = 50
    val nTokensPerDocument = 35

    val burnIn = 5 // iterations to skip before measuring performance
    val nIterations = 30 // number of iterations

    val rng = new java.util.Random()

    println("Creating the documents")
    val documents = (0 until nDocuments).map(i => {
      val d = new Document("noname")
      (0 until  nTokensPerDocument).foreach(i => {
        val t = new Token(d, "a")
        val label = rng.nextInt(nStates).toString
        t.attr += new PosLabel(t, label)
        val feats = new PosFeatures(t)
        t.attr += feats
        (0 until tokenFeaturesPerToken).foreach(i => {
          feats += rng.nextInt(nTokenFeatures).toString
        })
      })
      d
    })

    val labels = documents.map(_.map(_.posLabel))

    println("Initializing the weights")
    List(TestModel.localTemplate,TestModel.transTemplate).foreach(t => {
      t.freezeDomains
      (0 until t.weights.length).foreach(t.weights(_) = rng.nextGaussian())
    })

    def test(name: String, f: Seq[PosLabel] => Unit) {
      println("Testing "+name)
      val times = (0 until burnIn + nIterations).map(i => {
        val t0 = System.currentTimeMillis()
        labels.foreach(l => f(l))
        System.currentTimeMillis()-t0
      }).drop(burnIn)
      val mean = times.sum/nIterations
      val meanSq = times.map(x => x*x).sum / nIterations
      val std = math.sqrt(meanSq - mean*mean)
      println("Time was: "+mean+"+-"+2*(std/math.sqrt(nIterations)))
    }

    val searcher = new BeamSearch with FullBeam
    test("viterbi (max)", l => {
      searcher.search(TestModel.localTemplate, TestModel.transTemplate, l)
    })

    test("old BP (max)", l => {
      new BPInferencer[PosLabel](TestModel).inferTreewiseMax(l)
    })

    test("old BP (sum)", l => {
      new BPInferencer[PosLabel](TestModel).inferTreewise(l)
    })

    test("new BP (max)", l => {
      assert(l.head.token.hasNext)
      val fg = new LatticeBP(TestModel,  l.toSet) with MaxProductLattice
      new InferencerBPWorker(fg).inferTreewise()
    })

    test("new BP (sum)", l => {
      assert(l.head.token.hasNext)
      val fg = new LatticeBP(TestModel,  l.toSet) with SumProductLattice
      new InferencerBPWorker(fg).inferTreewise()
    })

  }
}