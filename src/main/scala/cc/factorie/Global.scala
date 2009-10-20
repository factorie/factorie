package cc.factorie

import scala.util.Random

object Global {
  
	var randomSeed = 0
	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  
	val defaultModel = new Model
	val defaultSamplers = new Samplers
}
