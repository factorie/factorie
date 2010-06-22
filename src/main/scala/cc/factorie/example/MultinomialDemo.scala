/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.example
import cc.factorie._
import cc.factorie.DenseProportionsMaximumLikelihoodEstimator._

object MultinomialDemo {

  def main(args:Array[String]) : Unit = {
    //val r = new scala.util.Random
    val numSides = 6
    class Roll(die:Proportions, value:Int = die.sampleInt) extends Discrete(die, value)
    Domain[Roll].size = () => numSides // TODO Make this unnecessary

    val die = new DenseProportions(List(.1, .2, .3, .2, .2))
    println("True distribution "+die)
    val rolls = for (i <- 1 to 1000) yield new Roll(die, 0)
    rolls.foreach(_.sample(null))
    die.estimate()
    println("Est  distribution "+die)

    val r = new scala.util.Random
    val die2 = new GrowableDenseCountsProportions
    val rolls2 = for (i <- 1 to 1000) yield new Roll(die2, r.nextInt(6))
    die2.estimate()
    println("Die2 "+die2)
  }

}
