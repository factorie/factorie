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
package cc.factorie.app.bib.parser

import collection.immutable.TreeMap
import annotation.tailrec

object Dom {

  final case class Document(
    comments: List[String],
    preambles: List[String],
    entries: Map[String, Entry])

  final case class Entry(
    ty: String,
    citationKey: String,
    crossReference: Option[Entry], // todo: maybe remove this field - I haven't seen a "crossref" yet
    authors: Option[List[Name]],
    editors: Option[List[Name]],
    otherFields: Map[String, String])

  final case class Name(
    first: String,
    von: String,
    last: String,
    jr: String)

  def stringToDom(str: String, expandAbbreviations: Boolean = true): Either[String, Document] =
    try {
      DocumentParser.parseString(str).right.map(astToDom(_, expandAbbreviations))
    } catch {
      case e: Exception => Left(e.toString)
    }

  private[parser] def astToDom(astDoc: AST.Document, expandAbbreviations: Boolean = true): Document = {

    // NOTE: some fields (acknowledgement, etc) don't quote their string inputs so we just pass through
    // symbols that we can't find unchanged. month abbreviations also are not really useful to expand
    val standardEnvironment = Map.empty[String, String].withDefault(identity)

    val emptyDocument = Document(Nil, Nil, Map.empty)

    def evalValue(value: AST.Value, env: Map[String, String]): String = value match {
      case AST.Literal(str) => str
      case AST.Abbrev(id) => if (expandAbbreviations) env(id) else id
      case AST.Concat(l, r) => evalValue(l, env) + evalValue(r, env)
    }

    @tailrec def loop(
      currentDoc: Document = emptyDocument,
      astEntries: List[AST.Entry] = astDoc.entries,
      env: Map[String, String] = standardEnvironment
      ): Document = astEntries match {
      case Nil => currentDoc
      case entry :: rest => entry match {

        case AST.StringEntry(name, value) =>
          loop(currentDoc, rest, env + (name -> evalValue(value, env)))

        case AST.CommentEntry(comment) =>
          val newComments =
            if (comment.trim.isEmpty) currentDoc.comments
            else currentDoc.comments :+ comment
          loop(currentDoc.copy(comments = newComments), rest, env)

        case AST.PreambleEntry(pre) =>
          loop(currentDoc.copy(preambles = currentDoc.preambles :+ evalValue(pre, env)), rest, env)

        case AST.RegularEntry(ty, citationKey, tags) =>
          val insensitiveMap = new TreeMap[String, String]()(CaseInsensitiveCompare)
          val evaldTags = tags.foldLeft(insensitiveMap)((acc, el) => acc + (el._1 -> evalValue(el._2, env)))
          val crossRefEntry = for {
            referenceName <- evaldTags.get("crossref")
            referenceEntry <- currentDoc.entries.get(referenceName)
          } yield referenceEntry
          def namesForField(fieldName: String) =
            evaldTags.get(fieldName).map(NameParser.stringToNames(_)).toList.flatten
          val remainingTags = evaldTags - "crossref" - "author" - "editor"
          val authorNames = Some(namesForField("author"))
          val editorNames = Some(namesForField("editor"))
          val entry = Entry(ty, citationKey, crossRefEntry, authorNames, editorNames, remainingTags)
          loop(currentDoc.copy(entries = currentDoc.entries + (entry.citationKey -> entry)), rest, env)
      }
    }
    loop()
  }

  object CaseInsensitiveCompare extends Ordering[String] {
    def compare(x: String, y: String) = x.compareToIgnoreCase(y)
  }
}
