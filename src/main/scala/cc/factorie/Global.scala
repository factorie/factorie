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
  val defaultGenerativeModel = new Model(new GeneratedValueTemplate, new MixtureChoiceVariableTemplate)
  val defaultObjective = new Model(new TrueLabelTemplate[CoordinatedLabelVariable[AnyRef]]())

  // TODO Consider removing this now that we have separate, more specific samplers.
  // TODO Consider also removing SamplerSuite?
  val defaultSampler = new SamplerSuite
  //defaultSampler += new GenericSampler(new GeneratedVariableSampler)
  //defaultSampler += new GenericSampler(new GibbsSampler[Variable with IterableSettings](defaultModel))

  /*val idVariableMap = new VariableMap {
    override def apply[V<:Variable](in:V): V = in
    def get(key: Variable): Option[B] = Some(key)
    def iterator: Iterator[(Variable, Variable)] = throw new Error
    def +[B1 >: Variable](kv: (Variable, B1)): IdVariableMap = throw new Error
    def -(key: A): IdVariableMap = throw new Error
  }*/

}
