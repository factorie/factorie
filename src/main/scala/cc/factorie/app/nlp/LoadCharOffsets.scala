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

package cc.factorie.app.nlp
import cc.factorie._
import cc.factorie.app.nlp.ner._
import java.io.File

// TODO Finish something like this.

//object LoadCharOffsets {
//  def fromTextAndOffsets(textFile:File, annFile:File, labelDomain:CategoricalDomain[String]): Document = {
//    val doc = LoadPlainText.fromFile(textFile, segmentSentences=false)
//    val annSource = scala.io.Source.fromFile(annFile)
//    val lineRe = "^(\\w+)\\s+(\\d+)\\s+(\\d+)".r
//    for (line <- annSource.getLines) {
//      val fields = line.split(' ')
//      val label = 
//    }
//    doc
//  }
//
//}
