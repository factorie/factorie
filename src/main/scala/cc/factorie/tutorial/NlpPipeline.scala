/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
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
package cc.factorie.tutorial

object NlpPipeline extends App {
  import cc.factorie.app.nlp._
  var doc = new Document("Education is the most powerful weapon which you can use to change the world.")
  val annotator = DocumentAnnotatorPipeline(pos.OntonotesForwardPosTagger, parse.WSJTransitionBasedParser)
  annotator.process(doc)
  for (token <- doc.tokens)
    println("%-10s %-5s %-4d %-7s".format(token.string, token.posTag.categoryValue, token.parseParentIndex, token.parseLabel.categoryValue))
    
    
  for (line <- scala.io.Source.stdin.getLines()) {
    doc = new Document(line)
    for (token <- doc.tokens)
      println("%-10s %-5s %-7s".format(token.string, token.posTag.categoryValue, token.nerTag.categoryValue))
  }
}
