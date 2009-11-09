package cc.factorie

import scala.util.Random

object Global {
  var randomSeed = 0
	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  
	val defaultModel = new Model
	val defaultObjective = new Model(new TrueLabelTemplate[Label[AnyRef]]())

  import cc.factorie.util.Implicits._
  lazy val defaultSamplerSuite = new SamplerSuite
	defaultSamplerSuite += new GenerativeVariableSampler
	defaultSamplerSuite += new GibbsSampler(defaultModel)

}
