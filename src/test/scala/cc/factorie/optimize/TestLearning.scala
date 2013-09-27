package cc.factorie.optimize

import cc.factorie._
import org.junit.Test
import org.junit.Assert._
import scala.util.Random
import cc.factorie.la._
import cc.factorie.util.LocalDoubleAccumulator
import cc.factorie.variable.{LabeledCategoricalVariable, BinaryFeatureVectorVariable, CategoricalVectorDomain, CategoricalDomain}
import cc.factorie.model.{Parameters, DotTemplateWithStatistics2, DotTemplateWithStatistics1, TemplateModel}
import cc.factorie.infer.InferByBPTree

/**
 * @author sameer
 */
class TestLearning {

  val random = new Random(0)

  object LabelDomain extends CategoricalDomain[String]

  object FeatureDomain extends CategoricalVectorDomain[String]

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

  def createModel(): TemplateModel with Parameters =
    new TemplateModel with Parameters {
      this += new DotTemplateWithStatistics1[Label] {
        val weights = Weights(new DenseTensor1(LabelDomain.size))

        for (i <- 0 until LabelDomain.size)
          weights.value(i) = random.nextDouble - 0.5

        override def toString = "bias"
      }
      this += new DotTemplateWithStatistics2[Label, Features] {
        val weights = Weights(new DenseTensor2(LabelDomain.size, FeatureDomain.dimensionSize))

        for (i <- 0 until LabelDomain.size)
          for (j <- 0 until FeatureDomain.dimensionSize)
            weights.value(i, j) = random.nextDouble - 0.5

        def unroll1(l: Label) = Factor(l, l.features)

        def unroll2(f: Features) = Factor(f.label, f)

        override def toString = "obs"
      }
    }

  @Test
  def testPseudolikelihood() {
    val data = createData(10)
    val model = createModel()

    val plExamples = data.map(d => new PseudolikelihoodExample(Seq(d), model))
    val plgrad = new LocalWeightsMapAccumulator(model.parameters.blankDenseMap)
    val plvalue = new LocalDoubleAccumulator(0.0)

    val llExamples = data.map(d => new LikelihoodExample(Seq(d), model, InferByBPTree))
    val llgrad = new LocalWeightsMapAccumulator(model.parameters.blankDenseMap)
    val llvalue = new LocalDoubleAccumulator(0.0)

    for ((ple, lle) <- plExamples.zip(llExamples)) {
      val localPLgrad = new LocalWeightsMapAccumulator(model.parameters.blankDenseMap)
      val localPLvalue = new LocalDoubleAccumulator(0.0)
      ple.accumulateValueAndGradient(localPLvalue, localPLgrad)
      ple.accumulateValueAndGradient(plvalue, plgrad)

      val localLLgrad = new LocalWeightsMapAccumulator(model.parameters.blankDenseMap)
      val localLLvalue = new LocalDoubleAccumulator(0.0)
      lle.accumulateValueAndGradient(localLLvalue, localLLgrad)
      lle.accumulateValueAndGradient(llvalue, llgrad)

      // check local
      assertEquals("local value does not match", localPLvalue.value, localLLvalue.value, 1.0e-7)
      assertEquals("local tensors size does not match", localPLgrad.tensorSet.toSeq.size, localLLgrad.tensorSet.toSeq.size)
      for ((a, llt) <- localLLgrad.tensorSet.toSeq) {
        val plt = localPLgrad.tensorSet(a)
        assertEquals("local tensor size for " + a + " does not match", plt.size, llt.size)
        for (i <- 0 until llt.size) {
          assertEquals(plt(i), llt(i), 1.0e-7)
        }
      }
    }
    // check global
    assertEquals("global value does not match", plvalue.value, llvalue.value, 1.0e-7)
    assertEquals("global tensors size does not match", plgrad.tensorSet.toSeq.size, llgrad.tensorSet.toSeq.size)
    for ((a, llt) <- llgrad.tensorSet.toSeq) {
      val plt = plgrad.tensorSet(a)
      assertEquals("global tensor size for " + a + " does not match", plt.size, llt.size)
      for (i <- 0 until llt.size) {
        assertEquals("global tensor value for " + a + "(" + i + ") does not match", plt(i), llt(i), 1.0e-7)
      }
    }

  }

}
