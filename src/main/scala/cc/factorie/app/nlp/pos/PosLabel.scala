/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.nlp.pos
import cc.factorie._
import cc.factorie.app.nlp._

// TODO can we not make the default implementation of NLP stuff use global variables, this is bug city -luke
@deprecated("Use PTBPosDomain instead.")
object PosDomain extends CategoricalDomain[String]
@deprecated("Use PTBPosLabel instead.")
class PosLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = PosDomain } 

// TODO Rename to PPosDomain, just because it is shorter and easier to pronounce
// TODO Consider renaming classes POS1... to Pos1

/** Penn Treebank part-of-speech tag domain. */
object PTBPosDomain extends CategoricalDomain[String] {
  this ++= Vector(
      "#",
      "$",
      "''",
      ",",
      "-LRB-",
      "-RRB-",
      ".",
      ":",
      "CC",
      "CD",
      "DT",
      "EX",
      "FW",
      "IN",
      "JJ",
      "JJR",
      "JJS",
      "LS",
      "MD",
      "NN",
      "NNP",
      "NNPS",
      "NNS",
      "PDT",
      "POS",
      "PRP",
      "PRP$",
      "RB",
      "RBR",
      "RBS",
      "RP",
      "SYM",
      "TO",
      "UH",
      "VB",
      "VBD",
      "VBG",
      "VBN",
      "VBP",
      "VBZ",
      "WDT",
      "WP",
      "WP$",
      "WRB",
      "``")
  freeze()
}
class PTBPosLabel(val token:Token, targetValue:String) extends LabeledCategoricalVariable(targetValue) { def domain = PTBPosDomain }
