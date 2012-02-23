package cc.factorie.bp

/**
 * @author sameer
 * @date 2/18/12
 */

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._
import cc.factorie._
import optimize.LimitedMemoryBFGS

class TestPiecewiseTrainer extends JUnitSuite {

  object FeaturesDomain extends CategoricalVectorDomain[String]

  class Features(ftrs: Seq[String], labelStr: String)
        extends BinaryFeatureVectorVariable[String](ftrs) {
    val label = new Label(labelStr, this)

    def domain = FeaturesDomain
  }

  object LabelDomain extends CategoricalDomain[String]

  class Label(labelString: String, val features: Features)
        extends LabelVariable[String](labelString) {
    def domain = LabelDomain
  }

  class LocalTemplate extends TemplateWithDotStatistics2[Label, Features] {
    override def statisticsDomains = Seq(LabelDomain, FeaturesDomain)

    def unroll1(label: Label) = Factor(label, label.features)

    def unroll2(features: Features) = throw new Error("Features shouldn't change")
  }

  @Test
  def testTwoPieceOneVariableEach() {
    val features1 = new Features(Seq("a"), "A")
    val features2 = new Features(Seq("b"), "B")
    val localTemplate = new LocalTemplate
    val model = new TemplateModel(localTemplate)
    val piece1 = ModelPiece(model, Seq(features1.label))
    val piece2 = ModelPiece(model, Seq(features2.label))
    val optimizer = new LimitedMemoryBFGS(new Trainer(model, Seq(piece1, piece2), Seq(localTemplate)))
    optimizer.optimize(10)
    println("FD: " + FeaturesDomain.dimensionDomain.values)
    println("LD: " + LabelDomain.values)
    for (ftr <- FeaturesDomain.dimensionDomain.values)
      for (label <- LabelDomain.values) {
        println(ftr.category + " " + label.category + " : " + localTemplate.weight(label.intValue, ftr.intValue))
      }

    assertTrue("a A should be > a B",
      localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("a")) > localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("a")))
    assertTrue("b B should be > b A",
      localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("b")) > localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("b")))
  }

  @Test
  def testTwoPieceOneVariableEachRegularized() {
    val features1 = new Features(Seq("a"), "A")
    val features2 = new Features(Seq("b"), "B")
    val localTemplate = new LocalTemplate
    val model = new TemplateModel(localTemplate)
    val piece1 = ModelPiece(model, Seq(features1.label))
    val piece2 = ModelPiece(model, Seq(features2.label))
    val optimizer = new LimitedMemoryBFGS(new Trainer(model, Seq(piece1, piece2), Seq(localTemplate)) with L2Regularizer {
      override def sigmaSq = 1
    })
    optimizer.optimize(10)
    println("FD: " + FeaturesDomain.dimensionDomain.values)
    println("LD: " + LabelDomain.values)
    for (ftr <- FeaturesDomain.dimensionDomain.values)
      for (label <- LabelDomain.values) {
        println(ftr.category + " " + label.category + " : " + localTemplate.weight(label.intValue, ftr.intValue))
      }

    assertTrue("a A should be > a B",
      localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("a")) > localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("a")))
    assertTrue("b B should be > b A",
      localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("b")) > localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("b")))
  }

  @Test
  def testTwoPieceOneVariableEachParallel() {
    val features1 = new Features(Seq("a"), "A")
    val features2 = new Features(Seq("b"), "B")
    val localTemplate = new LocalTemplate
    val model = new TemplateModel(localTemplate)
    val piece1 = ModelPiece(model, Seq(features1.label))
    val piece2 = ModelPiece(model, Seq(features2.label))
    val optimizer = new LimitedMemoryBFGS(new ParallelTrainer(model, Seq(piece1, piece2), Seq(localTemplate)))
    optimizer.optimize(10)
    println("FD: " + FeaturesDomain.dimensionDomain.values)
    println("LD: " + LabelDomain.values)
    for (ftr <- FeaturesDomain.dimensionDomain.values)
      for (label <- LabelDomain.values) {
        println(ftr.category + " " + label.category + " : " + localTemplate.weight(label.intValue, ftr.intValue))
      }
    assertTrue("a A should be > a B",
      localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("a")) > localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("a")))
    assertTrue("b B should be > b A",
      localTemplate.weight(LabelDomain.index("B"), FeaturesDomain.dimensionDomain.index("b")) > localTemplate.weight(LabelDomain.index("A"), FeaturesDomain.dimensionDomain.index("b")))
  }

  @Test
  def testManyPieceOneVariableEachParallel() {
    val numVariables = 500
    val features = (0 until numVariables).map(i => new Features(if (i % 2 == 0) Seq("a") else Seq("b"), if (i % 2 == 0 || random.nextDouble() > 0.9) "A" else "B"))

    // Parallel
    val localTemplatePar = new LocalTemplate
    val modelPar = new TemplateModel(localTemplatePar)
    val piecesPar = features.map(f => ModelPiece(modelPar, Seq(f.label)))
    val optimizerPar = new LimitedMemoryBFGS(new ParallelTrainer(modelPar, piecesPar, Seq(localTemplatePar)))
    var initTime = System.currentTimeMillis()
    optimizerPar.optimize(1)
    println("Parallel Time: " + (System.currentTimeMillis() - initTime))
    for (ftr <- FeaturesDomain.dimensionDomain.values)
      for (label <- LabelDomain.values) {
        println(ftr.category + " " + label.category + " : " + localTemplatePar.weight(label.intValue, ftr.intValue))
      }

    // Seqeuntial
    val localTemplateSeq = new LocalTemplate
    val modelSeq = new TemplateModel(localTemplateSeq)
    val piecesSeq = features.map(f => ModelPiece(modelSeq, Seq(f.label)))
    val optimizerSeq = new LimitedMemoryBFGS(new Trainer(modelSeq, piecesSeq, Seq(localTemplateSeq)))
    initTime = System.currentTimeMillis()
    optimizerSeq.optimize(1)
    println("Sequential Time: " + (System.currentTimeMillis() - initTime))
    for (ftr <- FeaturesDomain.dimensionDomain.values)
      for (label <- LabelDomain.values) {
        println(ftr.category + " " + label.category + " : " + localTemplateSeq.weight(label.intValue, ftr.intValue))
        assertEquals(localTemplateSeq.weight(label.intValue, ftr.intValue), localTemplatePar.weight(label.intValue, ftr.intValue), 0.01)
      }

  }

}