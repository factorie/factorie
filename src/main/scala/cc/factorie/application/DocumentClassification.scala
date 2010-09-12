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



package cc.factorie.application
import cc.factorie._
import scala.util.matching.Regex
import java.io.File
import scala.io.Source
import scala.reflect.Manifest

/** Variables and factors for independent classification of documents represented as bag-of-words feature vectors.
 
    @author Andrew McCallum
    @since 0.8
 */
object DocumentClassification {
 import cc.factorie.application.FeatureVectorClassification._

 val defaultLexer = "\\w+".r
 
 abstract class Document[L<:Label[This,L],This<:Document[L,This]](override val name:String) extends FeatureVectorClassification.Instance[L,This](name) {
   this: This =>
   //type GetterType <: DocumentGetter[L,This];
   //class GetterClass extends DocumentGetter[L,This]
   /** Populate the document from the words in the file. */
   def this(file:File, lexer:Regex) = {
     this(file.toString)
     val source = Source.fromFile(file)
     lexer.findAllIn(source.mkString).foreach(m => this += m.toString)
   }
   /** By default use the defaultLexer */
   def this(file:File) = this(file, defaultLexer)
   /* By default take the directory name to be the label string. */
   //def this(file:File) = this(file, file.getParentFile.getName)
   def size = 3 // TODO implement this
 }
 
 abstract class Label[D<:Document[This,D],This<:Label[D,This]](labelString:String, override val instance:D) extends FeatureVectorClassification.Label[D,This](labelString, instance) {
   this: This =>
   //type GetterType <: LabelGetter[D,This];
   //class GetterClass extends LabelGetter[D,This]
   def document = instance
 }

 class DocumentGetter[L<:Label[ThisDocument,L],ThisDocument<:Document[L,ThisDocument]] extends InstanceGetter[L,ThisDocument] {
   override def newLabelGetter = new LabelGetter[ThisDocument,L]
   //def label = initOneToOne[L](newLabelGetter, instance => instance.label, label => label.instance)
   def size = getOneWay(document => new IntegerObservation(document.size))
 }
 
 class LabelGetter[D<:Document[ThisLabel,D],ThisLabel<:Label[D,ThisLabel]] extends FeatureVectorClassification.LabelGetter[D,ThisLabel] {
   override def newInstanceGetter = new DocumentGetter[ThisLabel,D]
   //override def instance = initOneToOne[D](newInstanceGetter, label => label.instance, instance => instance.label)
   def document = instance
 }

 def newModel[L<:Label[D,L],D<:Document[L,D]](implicit lm:Manifest[L],dm:Manifest[D]): Model = {
   if (lm.erasure == classOf[Nothing] || dm.erasure == classOf[Nothing]) 
     throw new Error("You must specify type arguments to cc.factorie.application.DocumentClassification.newModel")
   new Model(
     new LabelTemplate[L],
     new LabelInstanceTemplate[L,D]
   )
 }

 def newObjective[L<:Label[D,L],D<:Document[L,D]](implicit lm:Manifest[L]) = new LabelTemplate[L]()(lm)
 //def newObjective[L<:Label[_,L]](implicit lm:Manifest[L]) = new TrueLabelTemplate[L]()(lm)

}
