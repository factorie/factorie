package cc.factorie

import org.junit.Test

class TestSingleSingleSparse {

  object LabelDomain extends CategoricalDomain[Boolean](List(true, false))
  class Label(target: Boolean, val categoryFeature: CategoryFeature, val feature: Feature) extends LabelVariable[Boolean](target) {
    def domain = LabelDomain 
  }

  object FeatureDomain extends CategoricalTensorDomain[String]
  class Feature(feats: Seq[String]) extends BinaryFeatureVectorVariable[String](feats) {
    def domain = FeatureDomain
  }

  object CategoryFeatureDomain extends CategoricalDomain[Int](List(1,2,3,4,5))
  class CategoryFeature(v: Int) extends CategoricalVariable[Int](v) {
    def domain = CategoryFeatureDomain 
  }

  @Test def test {
    
    throw new Error("This code is significantly slower than expected.")

    val t =
      new TemplateWithDotStatistics3[Label, CategoryFeature, Feature] {
        //override def statisticsDomains = ((LabelDomain, CategoryFeatureDomain, FeatureDomain))
        lazy val weights = new la.DenseTensor3(LabelDomain.size, CategoryFeatureDomain.size, FeatureDomain.dimensionSize)
        def unroll1(label: Label) = Factor(label, label.categoryFeature, label.feature)
        def unroll2(categoryFeature: CategoryFeature) = throw new Error("CategoryFeature shouldn't change")
        def unroll3(feature: Feature) = throw new Error("Feature shouldn't change")
      }

    val model = new TemplateModel(t)

    val feature = new Feature(Seq("feature1", "feat2", "feat3"))
    val categoryFeature = new CategoryFeature(2)
    val label = new Label(true, categoryFeature, feature)

    label := LabelDomain.value(false)

    val objective = new TemplateModel(new HammingLossTemplate[Label])
    val learner = new SampleRank(new GibbsSampler(model, objective), new cc.factorie.optimize.AROW(model))
    val predictor = new VariableSettingsSampler[Label](model, null) { temperature = 0.01 }

    val featureSpace=5000

    val train = for (i <- 1 to 44000) yield {
      val feature = new Feature((1 to 20).map(_ => random.nextInt(featureSpace).toString))
      val categoryFeature = new CategoryFeature(random.nextInt(3)+1)
      new Label(true, categoryFeature, feature)
    }

    train.foreach(_.setRandomly())
    println("Dataset Created.")

    learner.processAll(train)
    println(t.weights)

    predictor.process(label)

  }

}
