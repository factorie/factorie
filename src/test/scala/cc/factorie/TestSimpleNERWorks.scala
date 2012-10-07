package cc.factorie

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 9/18/12
 * Time: 9:29 AM
 * To change this template use File | Settings | File Templates.
 *
 *
 * This test asserts nothing, its purpose is to ensure that simple NER-like code keeps working.
 */



import cc.factorie.app.nlp._
import cc.factorie.app.chain._
import cc.factorie._
import app.chain.Lexicon
import app.nlp.ner.ChainNerLabel
import app.nlp.Token
import collection.mutable.ArrayBuffer
import org.junit.Test

/**
 * Created by IntelliJ IDEA.
 * User: apassos
 * Date: 9/18/12
 * Time: 9:23 AM
 * To change this template use File | Settings | File Templates.
 */

class TestSimpleNERWorks {
  object ChainNerFeaturesDomain extends CategoricalTensorDomain[String]
  val lexicons = new scala.collection.mutable.ArrayBuffer[Lexicon]


  class ChainNerFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = ChainNerFeaturesDomain
    override def skipNonCategories = true
  }

   object LabelDomain extends CategoricalDomain[String]
   class Label(t: Token, s: String) extends ChainNerLabel(t,s) {
     def domain = LabelDomain

     type ContainedVariableType = this.type
   }

  def someVariables() : Seq[Token] = {
    val d = new Document("foo")
    val t0 = new Token(d, "bar")
    t0.attr += new Label(t0, "A")
    t0.attr += new ChainNerFeatures(t0)
    val t1 = new Token(d, "baz")
    t1.attr += new Label(t1, "B")
    t1.attr += new ChainNerFeatures(t1)
    Seq(t0, t1)
  }

  @Test def testNotBroken() {
    val trainTokens = someVariables()
    val trainLabels = trainTokens.map(_.attr[Label])

    val model = new CombinedModel[Variable](
      new DotTemplateWithStatistics2[Label, Label]() {
        lazy val weights = new la.DenseTensor2(LabelDomain.size, LabelDomain.size)
        factorName = "labelLabel"
        def unroll1(v: Label) = if (v.token.hasNext) Factor(v, v.token.next.attr[Label]) else Nil
        def unroll2(v: Label) = if (v.token.hasPrev) Factor(v.token.prev.attr[Label], v) else Nil

      },
      new DotTemplateWithStatistics2[Label, ChainNerFeatures]() {
        lazy val weights = new la.DenseTensor2(LabelDomain.size, ChainNerFeaturesDomain.dimensionSize)
        factorName = "labelToken"
        def unroll1(v: Label) = Factor(v, v.token.attr[ChainNerFeatures])
        def unroll2(v: ChainNerFeatures) = Nil
      })

    val trainer = new DotMaximumLikelihood(model)
    trainer.processAllBP(Seq(trainLabels), InferByBPChainSum)

    BP.inferChainMax(trainLabels, model)
  }
}
