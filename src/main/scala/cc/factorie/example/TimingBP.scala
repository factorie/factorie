package cc.factorie.example

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 2/25/12
 * Time: 6:30 PM
 * To change this template use File | Settings | File Templates.
 */


import cc.factorie._
import cc.factorie.bp._
import cc.factorie.app.nlp._
import cc.factorie.app.chain.Observations.addNeighboringFeatureConjunctions
import cc.factorie.app.nlp.pos._
import cc.factorie.bp.optimized
import cc.factorie.bp.optimized.FullBeam
import cc.factorie.bp.optimized.BeamSearch
import cc.factorie.bp.optimized.FullBeam
import cc.factorie.TemplateModel
import cc.factorie.TemplateWithDotStatistics1
import cc.factorie.app.nlp.pos.PosLabel
import cc.factorie.app.nlp.pos.PosDomain
import cc.factorie.TemplateWithDotStatistics2
import cc.factorie.app.nlp.pos.PosFeatures
import cc.factorie.app.nlp.pos.PosFeaturesDomain


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
    val nDocuments = 100
    val nTokensPerDocument = 50

    val burnIn = 5 // iterations to skip before measuring performance
    val nIterations = 20 // number of iterations

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
      println("Time was: "+ (0 until burnIn + nIterations).map(i => {
        val t0 = System.currentTimeMillis()
        labels.foreach(l => f(l))
        System.currentTimeMillis()-t0
      }).drop(burnIn).sum / nIterations)
    }

    val searcher = new BeamSearch with FullBeam
    test("viterbi", l => {
      searcher.searchAndSetToMax(TestModel.localTemplate, TestModel.transTemplate, l)
    })

    test("new BP", l => {
      assert(l.head.token.hasNext)
      val fg = new LatticeBP(TestModel,  l.toSet) with SumProductLattice
      new InferencerBPWorker(fg).inferTreewise()
      fg.setToMaxMarginal(l)
    })

    test("old BP", l => {
      assert(l.head.token.hasNext)
      new BPInferencer[PosLabel](TestModel).inferTreewise(l)
    })

  }
}