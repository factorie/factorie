/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

import scala.util.Random

object Global {
  var randomSeed = 0
  implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
  // TODO Consider renaming this "defaultRandom", 
  // anticipating the time when all these definitions make go in "package object factorie"?
  
  val defaultModel = new Model
  val defaultObjective = new Model(new TrueLabelTemplate[CoordinatedLabelVariable[AnyRef]]())

  import cc.factorie.util.Implicits._
  val defaultSampler = new SamplerSuite
  defaultSampler += new GeneratedVariableSampler
  defaultSampler += new GibbsSampler(defaultModel)

}
