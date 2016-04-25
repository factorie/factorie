/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.app.bib.hcoref

import cc.factorie.util.namejuggler._

/**
 * @author John Sullivan
 */
object Canopies {

  def fromString(rawNameString:String):String = StringUtils.toOptionNonempty(rawNameString.trim.replace(", , ",", ")) match {
    case Some(nonEmptyNameString) =>
      try{
        val personName = PersonNameWithDerivations(nonEmptyNameString).inferFully
        val res = OptionUtils.merge[NonemptyString](personName.firstInitial, personName.longestSurName, {(initial, surname) => NonemptyString(initial.s.substring(0, 1).toLowerCase + surname.s)}).map(_.s).getOrElse("")
        res
      } catch {
        case e:Exception =>
          println("Failed on %s" format rawNameString)
          //e.printStackTrace()
          ""
      }
    case None => ""
  }

  def streamCanopies(ncs:Iterator[AuthorNodeCubbie]):Iterator[(String, String)] = ncs.map { nc =>
    fromString(nc.fullName.value) -> nc.id.toString
  }

}

