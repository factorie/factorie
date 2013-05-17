package cc.factorie.optimize

import cc.factorie._
import org.junit.Test
import org.junit.Assert._
import scala.util.Random
import cc.factorie.la._
import cc.factorie.util.LocalDoubleAccumulator

/**
 * @author sameer
 */
class LearningTest {

  val random = new Random(0)

  object LabelDomain extends CategoricalDomain[String]

  object FeatureDomain extends CategoricalDimensionTensorDomain[String]

  class Features(val label: Label) extends BinaryFeatureVectorVariable[String] {
    def domain = FeatureDomain
  }

  class Label(val id: Int, labelStr: String) extends LabeledCategoricalVariable[String](labelStr) {

    def domain = LabelDomain

    val features = new Features(this)
  }

  def createData(n: Int): Seq[Label] = {
    (0 until n) map (i => {
      val l = new Label(i, (i < n/2).toString)
      l.features += ((l.intValue + 1) * (i % 5)).toString
      l
    })
  }

  def createModel(): TemplateModel = {
    val model = new TemplateModel()
    val biasTemplate = new DotTemplateWithStatistics1[Label] {
      lazy val weightsTensor = new DenseTensor1(LabelDomain.size)

      for (i <- 0 until LabelDomain.size)
        weightsTensor(i) = random.nextDouble - 0.5

      override def toString = "bias"
    }
    model += biasTemplate

    val observationTemplate = new DotTemplateWithStatistics2[Label, Features] {
      lazy val weightsTensor = new DenseTensor2(LabelDomain.size, FeatureDomain.dimensionSize)

      for (i <- 0 until LabelDomain.size)
        for (j <- 0 until FeatureDomain.dimensionSize)
          weightsTensor(i, j) = random.nextDouble - 0.5

      def unroll1(l: Label) = Factor(l, l.features)

      def unroll2(f: Features) = Factor(f.label, f)

      override def toString = "obs"
    }
    model += observationTemplate
    model
  }

  @Test
  def testPseudolikelihood() {
    val data = createData(10)
    val model = createModel()

    val plExamples = data.map(d => new PseudolikelihoodExample(Seq(d)))
    val plgrad = new LocalTensorsAccumulator(model.weights.blankDenseCopy)
    val plvalue = new LocalDoubleAccumulator(0.0)

    val llExamples = data.map(d => new LikelihoodExample(Seq(d), InferByBPTreeSum))
    val llgrad = new LocalTensorsAccumulator(model.weights.blankDenseCopy)
    val llvalue = new LocalDoubleAccumulator(0.0)

    for ((ple, lle) <- plExamples.zip(llExamples)) {
      val localPLgrad = new LocalTensorsAccumulator(model.weights.blankDenseCopy)
      val localPLvalue = new LocalDoubleAccumulator(0.0)
      ple.accumulateExampleInto(model, localPLgrad, localPLvalue)
      ple.accumulateExampleInto(model, plgrad, plvalue)

      val localLLgrad = new LocalTensorsAccumulator(model.weights.blankDenseCopy)
      val localLLvalue = new LocalDoubleAccumulator(0.0)
      lle.accumulateExampleInto(model, localLLgrad, localLLvalue)
      lle.accumulateExampleInto(model, llgrad, llvalue)

      // check local
      assertEquals("local value does not match", localPLvalue.value, localLLvalue.value, 1.0e-7)
      assertEquals("local tensors size does not match", localPLgrad.tensor.size, localLLgrad.tensor.size)
      for ((a, llt) <- localLLgrad.tensor) {
        val plt = localPLgrad.tensor(a)
        assertEquals("local tensor size for " + a + " does not match", plt.size, llt.size)
        for (i <- 0 until llt.size) {
          assertEquals("local tensor value for " + a + "(" + i + ") does not match", plt(i), llt(i), 1.0e-7)
        }
      }
    }
    // check global
    assertEquals("global value does not match", plvalue.value, llvalue.value, 1.0e-7)
    assertEquals("global tensors size does not match", plgrad.tensor.size, llgrad.tensor.size)
    for ((a, llt) <- llgrad.tensor) {
      val plt = plgrad.tensor(a)
      assertEquals("global tensor size for " + a + " does not match", plt.size, llt.size)
      for (i <- 0 until llt.size) {
        assertEquals("global tensor value for " + a + "(" + i + ") does not match", plt(i), llt(i), 1.0e-7)
      }
    }

  }

}
