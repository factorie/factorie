package cc.factorie

import scala.util.Random

object Global {
  var randomSeed = 0
	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  // TODO Consider renaming this "defaultRandom", 
  // anticipating the time when all these definitions make go in "package object factorie"?
  
	val defaultModel = new Model
	val defaultObjective = new Model(new TrueLabelTemplate[CoordinatedLabel[AnyRef]]())

  import cc.factorie.util.Implicits._
  val defaultSampler = new SamplerSuite
	defaultSampler += new GenerativeVariableSampler
	defaultSampler += new GibbsSampler(defaultModel)

}
