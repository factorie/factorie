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
import cc.factorie.app.nlp._
import java.io.File
import scala.util.matching.Regex
import scala.io.Source
import org.apache.commons.compress.compressors.CompressorStreamFactory
import java.io._
import info.bliki.wiki.filter.PlainTextConverter
import info.bliki.wiki.model.WikiModel
import scala.xml.{XML,Text,Elem}

/** Create Documents from plain text files.
    By default create one Document per file.
    To create multiple Documents from one file, set documentSeparator regex.  
    If the regex specifies a group (via parenthesis) then the Document's name will be set to the match of the contents of this first group. */
class LoadWikipediaPlainText {
  /** This assumes that the file has format of enwiki-latest-pages-articles.xml.bz2. */
  def fromCompressedFilename(filename:String, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    require(filename.startsWith("enwiki") && filename.endsWith(".xml.bz2"))
    val input = Source.fromInputStream(new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(filename)))
    fromSource(input, maxArticleCount)
  }
  /** This assumes that the file has format of enwiki-latest-pages-articles.xml.bz2. */
  def fromCompressedFile(file:File, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    val input = Source.fromInputStream(new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(file)))
    fromSource(input, maxArticleCount)
  }
  def fromSource(input:io.Source, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    new Iterator[Document] {
      //private val wikiModel = new WikiModel("http://www.mywiki.com/wiki/${image}", "http://www.mywiki.com/wiki/${title}")
      private val wikiModel = new WikiModel("${image}", "${title}")
      private val plainConverter = new PlainTextConverter()
      private val lines = input.getLines()
      private var articleCount = 0
      private var done = false
      private var insidePage = false
      private var insideText = false
      private var nextDocument = getNextDocument
      val cleaningRegex = List(
          "\\{\\{[^\\}]*\\}\\}", // Remove everything {{inside}}
          "\\{\\|[^\\}]*\\|\\}", // Remove everything {|inside|}
          "(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
          "\\[\\[(?:File|Image):[^\\]]+\\|", // Remove [[File:WilliamGodwin.jpg|left|thumb|[
          "wikt:|nbsp;|ndash;|br/",
          "Category:"
          ).mkString("|").r
      assert(cleaningRegex ne null)

      // Keep getting next document until we get a non-null one
      private def getNextDocument: Document = {
        var result = getNextDocument1
        while (lines.hasNext && (result eq null))
          result = getNextDocument
        result
      }
      // Try to fetch one document, but if there is no text in this article, return null
      private def getNextDocument1: Document = {
        val nonBracket = "(?:(?:.(?!\\[\\[|\\]\\]))*)"
        //val pairedBracket = s"\\[\\[$nonBracket(?:\\[\\[$nonBracket\\]\\]$nonBracket)*\\]\\]"
        val cleaningRegex = ("(?s)" + (List( // Make "." match also match newline
          "&lt;!--(?:.(?!--&gt;))+.--&gt;", // Remove comments
          "\\{\\|(?:.(?!\n\\|\\}))+.\n\\|\\}", // Remove everything {|inside|}
          "&lt;math&gt;(?:.(?!&lt;/math&gt;))+.&lt;/math&gt;", // Remove everything inside <math> and </math>
          "&lt;code&gt;(?:.(?!&lt;/code&gt;))+.&lt;/code&gt;", // Remove everything inside <code> and </code>
          "&lt;gallery(?:.(?!&lt;/gallery&gt;))+.&lt;/gallery&gt;", // Remove everything inside <gallery blah> and </gallery>
          ////"\\{\\{(?:.(?!\\{\\{|\\}\\}))+.\\{\\{(?:.(?!\\}\\}|\\{\\{))+.\\}\\}(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          //"\\{\\{(?:(?:.(?!\\{\\{|\\}\\}))+.?\\{\\{(?:.(?!\\}\\}|\\{\\{))*.\\}\\})+(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          //"\\{\\{(?:(?:.(?!\\{\\{|\\}\\}))+.?\\{\\{(?:.(?!\\}\\}|\\{\\{))*.\\}\\})+(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          ////"\\{\\{(?:[^\\{]*\\{\\{[^\\}]*\\}\\}[^\\}]*)+\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}
          //"\\{\\{[^\\}]*\\}\\}", // Remove everything {{inside}}, but this messes up multiple nestings of {{}}
          //s"\\[\\[(?:wikisource:)?(?:File|Image|Media):${nonBracket}.?(?:\\[\\[${nonBracket}.?\\]\\]${nonBracket}.?)*.?\\]\\]",
          s"\\[\\[[A-Za-z:]+:${nonBracket}.?(?:\\[\\[${nonBracket}.?\\]\\]${nonBracket}.?)*.?\\]\\]",
          "(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
          //"\\[\\[(?:File|Image):[^\\]]+\\]\\]", // Remove [[File:WilliamGodwin.jpg|left|thumb]]
          //"\\[\\[(?:File|Image):(?(?:.(?!\\[\\[|\\]\\])+(?:\\[\\[(?:.(?!\\[\\[|\\]\\](?:.(?!\\[\\[|\\]\\])+)*\\]\\])))\\]\\]", // Remove [[File:WilliamGodwin.jpg|left|thumb]]
          "&lt;(?:.(?!&gt;))*.&gt;", //Remove everything between &lt; and &gt;
          "&(?:[a-z]{2,6};)+", // Remove &quot; and solo &gt; (meaning > symbol) and &amp;nbsp; and all other similar patterns 
          //"wikt:|nbsp;|ndash;|br/",
          "Category:",
          "#REDIRECT",
          "^\\s+", // Remove leading whitespace
          "\\s+$" // Remove trailing whitespace
          ).mkString("|"))).r

        var sb = new StringBuffer(2048)
        var docDone = false
        var title: String = null
        while (lines.hasNext && !docDone) {
          val line = lines.next
          //println(articleCount.toString+" Line>>> "+line)
          val titleIndex = line.indexOf("<title>")
          if (titleIndex >= 0) {
            val titleEndIndex = line.indexOf("</title>")
            title = line.substring(titleIndex+7, titleEndIndex)
          } else if (line.contains("<text") && !line.contains("</text>")) { insideText = true; sb append line.substring(line.lastIndexOf('>')+1) }
          else if (line.contains("</text>")) { insideText = false; docDone = true; sb append line.substring(0, line.indexOf('<')) }
          else if (insideText) { sb append line; sb append '\n' }
        }
        if (!lines.hasNext) input.close()
        sb = removeNestedBrackets(sb)
        val text = cleaningRegex.replaceAllIn(sb, " ").trim
        if (text.length == 0) return null
        articleCount += 1
        new Document(text).setName(title)
      }
      def hasNext: Boolean = articleCount < maxArticleCount && (nextDocument ne null)
      def next: Document = {
        val result = nextDocument
        nextDocument = getNextDocument
        result
      }
      private def removeNestedBrackets(s:StringBuffer): StringBuffer = {
        val sb = new StringBuffer
        var openCount = 0
        var i = 0; val len = s.length
        while (i < len) {
          val c = s.codePointAt(i).toChar
          if (c == '{') openCount += 1
          else if (c == '}' && openCount > 0) openCount -= 1
          else if (openCount == 0) sb append c
          i += 1
        }
        sb
      }
      // No longer used...
      private def xmlToPlainText(xmlString:String): String = {
        //val xml = new scala.xml.pull.XMLEventReader(Source.fromString(xmlString))
        val xml = scala.xml.XML.loadString(xmlString).iterator
        var done = false
        var insideText = false
        var insideRef = false
        var insideComment = false
        val sb = new StringBuffer
        while (xml.hasNext && !done) {
          xml.next() match {
            case e => println(e)
            //case EvElemStart(_, "text", _, _) => { insideText = true }
            //case EvElemEnd(_, "text") => { insideText = false; done = true }
            case Text(t) if insideText => {
              if (t.startsWith("!--") && !t.endsWith("--")) insideComment = true
              else if (t.endsWith("--")) insideComment = false
              else if (t.startsWith("ref") && !t.endsWith("/")) insideRef = true
              else if (t == "/ref") insideRef = false
              else if (!insideRef && !insideComment && !t.startsWith("ref ") && !t.startsWith("#REDIRECT")) { sb append t; sb append ' ' }
            }
            case _ => // ignore all other tags
          }
        }
        //cleaningRegex.replaceAllIn(sb.toString, " ")
        sb.toString
      }
    }
  }
}

object LoadWikipediaPlainText extends LoadWikipediaPlainText {
  def main(args:Array[String]): Unit = {
    val docs = fromCompressedFilename(args(1), args(0).toInt)
    for (doc <- docs) {
      println(doc.string)
      println("\n+++++++++++++++++++++++++++++++++++++++++\n\n")
    }
  }
}
