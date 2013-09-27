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

package cc.factorie.app.nlp.load
import java.io.File
import scala.xml._
import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.DocumentAnnotatorPipeline

object LoadNYTimesXML {
  def fromFile(file:File): Seq[Document] = {
    val article = XML.loadFile(file)
    //println(article \\ "head" \\ "title" text)
    //println(article \ "head" \ "title" text)
    //println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
    val content = article \ "head" \ "docdata" \ "identified-content"
    //print("Reading ***"+(article\"head"\"title").text+"***")
    // This does not include the headline, perhaps it should -akm
    new LoadPlainText(documentName = file.getCanonicalPath)(DocumentAnnotatorPipeline.defaultDocumentAnnotationMap).fromString((article \ "body" \ "body.content").text)
  }
}