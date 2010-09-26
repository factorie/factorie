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

package cc.factorie.app.classify.document
import cc.factorie._
import cc.factorie.er._
import cc.factorie.app.classify.Label
import cc.factorie.app.strings._
import scala.util.matching.Regex
import scala.io.Source
import java.io.File

abstract class Document[L<:Label[This,L],This<:Document[L,This]](override val name:String) extends cc.factorie.app.classify.Instance[L,This](name) {
  this: This =>
  //type GetterType <: DocumentGetter[L,This];
  //class GetterClass extends DocumentGetter[L,This]
  /** Populate the document from the words in the file. */
  def this(file:File, segmenter:StringSegmenter = alphaSegmenter) = {
    this(file.toString)
    segmenter(file).foreach(m => this += m.toString)
  }
  /* By default take the directory name to be the label string. */
  //def this(file:File) = this(file, file.getParentFile.getName)
  //def size = 3 // TODO implement this
}
 
class DocumentGetter[L<:Label[ThisDocument,L],ThisDocument<:Document[L,ThisDocument]] extends cc.factorie.app.classify.InstanceGetter[L,ThisDocument] {
  //override def newLabelGetter = new LabelGetter[ThisDocument,L]
  //def label = initOneToOne[L](newLabelGetter, instance => instance.label, label => label.instance)
  def size = getOneWay(document => new IntegerObservation(document.vector.activeDomainSize))
}
