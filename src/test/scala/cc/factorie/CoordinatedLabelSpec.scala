/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie

/*
import org.specs.runner.{JUnit, JUnit4}
import org.specs.Specification

// @author Sebastian Riedel
class CoordinatedLabelTest extends JUnit4(CoordinatedLabelSpec)
object CoordinatedLabelSpec extends Specification with JUnit {

  "A CoordinatedLabel" should {
    "change its value when the set method is called" in {
      implicit val diffList = new DiffList
      val label = new CoordinatedLabelVariable("ORG") {}
      label.set("MISC")
      label.value must_== "MISC"
    }

    "create a diff object that undoes a set action when the set method is called" in {
      implicit val diffList = new DiffList
      val label = new CoordinatedLabelVariable("ORG") {}
      label.set("MISC")
      diffList(0).undo
      label.value must_== "ORG"
    }

  }

}
*/
