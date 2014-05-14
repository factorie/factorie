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

package cc.factorie.app.nlp.load
import java.io.File
import scala.xml._
import cc.factorie.app.nlp.Document

/** Load a Document from a single NYTimes article in the XML format released by NYTimes and described in
    Evan Sandhaus (2008), "The New York Times Annotated Corpus," Linguistic Data Consortium, Philadelphia. */
object LoadNYTimesXML {
  def fromFile(file:File): Seq[Document] = {
    val article = XML.loadFile(file)
    //println(article \\ "head" \\ "title" text)
    //println(article \ "head" \ "title" text)
    //println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
    val content = article \ "head" \ "docdata" \ "identified-content"
    //print("Reading ***"+(article\"head"\"title").text+"***")
    // TODO This does not include the headline, perhaps it should -akm
    LoadPlainText.fromString((article \ "body" \ "body.content").text).map(_.setName(file.getCanonicalPath))
  }
}