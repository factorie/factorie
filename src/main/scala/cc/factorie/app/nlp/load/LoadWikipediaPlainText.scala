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
    //require(filename.startsWith("enwiki") && filename.endsWith(".xml.bz2"))
    val inputStream = new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(filename))
    return fromInputStream(inputStream)

    val input = Source.fromInputStream(inputStream)
    fromSource(input, maxArticleCount)
  }
  /** This assumes that the file has format of enwiki-latest-pages-articles.xml.bz2. */
  def fromCompressedFile(file:File, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    val inputStream = new CompressorStreamFactory().createCompressorInputStream(CompressorStreamFactory.BZIP2, new FileInputStream(file))
    return fromInputStream(inputStream)
    
    val br = new BufferedReader(new InputStreamReader(inputStream))
    var line: String = null
    var articleCount = 0
    while ({ line = br.readLine(); line ne null}) {
      if (line.contains("<text")) {
        articleCount += 1
        if (articleCount % 100 == 0) print("\rBufferedReader "+articleCount)
      }
    }
    return null
    
    val input = Source.fromInputStream(inputStream)
    fromSource(input, maxArticleCount)
  }
  
  // An io.Source version of this just keeps growing until it runs out of memory, as if io.Source were keeping the entire contents in memory.
  // So we make an InputStream version of this method.
  
  def fromInputStream(input:InputStream, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    new Iterator[Document] {
      val bufferedReader = new BufferedReader(new InputStreamReader(input))
      var bufferedReaderDone = false
      private var articleCount = 0
      private var nextDocument = getNextDocument

      // Keep getting next document until we get a non-null one or the bufferedReader is done
      private def getNextDocument: Document = {
        var result = getNextDocument1
        while ((result eq null) && !bufferedReaderDone)
          result = getNextDocument
        result
      }
      // Try to fetch one document, but if there is no text in this article, return null
      private def getNextDocument1: Document = {
        val nonBracket = "[^\\[\\]]*"
        val cleaningRegex = ("(?s)" + (List( // Make "." match also match newline
          "&lt;!--(?:.(?!--&gt;))+.--&gt;", // Remove comments
          //"\\{\\|(?:.(?!\n\\|\\}))+.\n\\|\\}", // Remove everything {|inside|}
          "&lt;ref&gt;(?:.(?!&lt;/ref&gt;))*.&lt;/ref&gt;", // Remove everything inside <ref> and </ref>
          "&lt;math&gt;(?:.(?!&lt;/math&gt;))*.&lt;/math&gt;", // Remove everything inside <math> and </math>
          "&lt;code&gt;(?:.(?!&lt;/code&gt;))*.&lt;/code&gt;", // Remove everything inside <code> and </code>
          "&lt;gallery(?:.(?!&lt;/gallery&gt;))*.&lt;/gallery&gt;", // Remove everything inside <gallery blah> and </gallery>

          // Temporarily comment out next line:
          //s"\\[\\[[A-Za-z:]+:${nonBracket}(?:\\[\\[${nonBracket}\\]\\]${nonBracket})*\\]\\]", // Remove [[:wikisource:File:WilliamGodwin.jpg|left|thumb]]
          
          // Temporarily comment out next line:
          //"(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
          
          
          "&lt;(?:.(?!&gt;))*.&gt;", //Remove everything between &lt; and &gt;
          "&(?:[a-z]{2,6};)+", // Remove &quot; and solo &gt; (meaning > symbol) and &amp;nbsp; and all other similar patterns 
          "Category:",
          "#REDIRECT",
          "^\\s+", // Remove leading whitespace
          "\\s+$" // Remove trailing whitespace
          ).mkString("|"))).r

        var sb = new StringBuffer(2048)
        var docDone = false
        var title: String = null
        var insideText = false
        var line: String = null
        
        
        
        
        
        while ({ line = bufferedReader.readLine(); (line ne null) && !docDone }) {
          //println(articleCount.toString+" Line>>> "+line)
          if (!insideText) {
            val titleIndex = line.indexOf("<title>")
            if (titleIndex >= 0) {
              val titleEndIndex = line.indexOf("</title>")
              title = line.substring(titleIndex+7, titleEndIndex)
              //println(title)
            } else if (line.contains("<text") && !line.contains("</text>")) {
              insideText = true; sb append line.substring(line.lastIndexOf('>')+1)
            }
          } else {
            if (line.contains("</text>")) { insideText = false; docDone = true; sb append line.substring(0, line.indexOf('<')) }
            else { sb append line; sb append '\n' }
          }
        }
        if (line eq null) { input.close(); bufferedReaderDone = true }
        sb = removeNestedBrackets2(sb)
        val text = cleaningRegex.replaceAllIn(sb, " ")
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
      private def removeNestedBrackets2(s:StringBuffer): StringBuffer = {
        val sb = new StringBuffer
        var sb2 = new StringBuffer
        var curlyOpenCount = 0
        var squareOpenCount = 0
        var i = 0; val len = s.length
        while (i < len) {
          val c = s.codePointAt(i).toChar
          if (c == '{') { curlyOpenCount += 1 /*; sb.append("{"+curlyOpenCount)*/ }
          else if (c == '[')  { squareOpenCount += 1 /*; sb.append("["+squareOpenCount)*/ }
          else if (c == '}' && curlyOpenCount > 0) { curlyOpenCount -= 1 ; sb.append(' ') /* Why it this "append" necessary (otherwise large chunks of text missing before "In Aristotle's terminology..."); sb.append("}"+curlyOpenCount)*/ }   // include (openCount > 0) because sometimes a }} will appear inside a comment.
          else if (c == ']' && squareOpenCount > 0) {
            squareOpenCount -= 1
            //sb.append("]"+squareOpenCount)
            if (squareOpenCount == 0) {
              // Handling [[wikt:anarchism|anarchism]] and [[Taoism|Taoist]] and [[File:WilliamGodwin.jpg|left|thumb|[[William Godwin]] and [[wiktionary:anthropology|anthropology]] and [[w:Charles Lyell|Charles Lyell's]]
              val s2 = sb2.toString
              val colonIndex = s2.indexOf(':')
              val barIndex = s2.indexOf('|')
              if (colonIndex >= 1) {
                //val prefix = s2.substring(0, 3); if (prefix == "wik") { if (barIndex > 0) sb.append(s2.substring(colonIndex+1, barIndex)) else sb.append(s2.substring(colonIndex+1)) }
                if (s2(0) == 'w') { if (barIndex > 0) sb.append(s2.substring(barIndex+1)) else sb.append(s2.substring(colonIndex+1)) }
              } else if (colonIndex == -1) {
                if (barIndex > 0) sb.append(s2.substring(barIndex+1))
                //if (barIndex > 0) sb.append(s2.substring(0, barIndex)) // Note: this appends the Wikipedia title, whereas the Wikipedia page text would show the part after the '|', not before. -akm 
                else sb.append(s2)
              }
              //if (!s2.contains(':')) sb append s2
              sb2 = new StringBuffer
            }
          } else if (curlyOpenCount == 0) {
            if (squareOpenCount == 0) sb append c
            else if (squareOpenCount == 2) sb2 append c
          }
          i += 1
        }
        sb
      }
      private def removeNestedBrackets(s:StringBuffer): StringBuffer = {
        val sb = new StringBuffer
        var openCount = 0
        var i = 0; val len = s.length
        while (i < len) {
          val c = s.codePointAt(i).toChar
          if (c == '{') openCount += 1
          else if (c == '}' && openCount > 0) openCount -= 1 // include (openCount > 0) because sometimes a }} will appear inside a comment.
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
        var line: String = null
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

  
  def fromSource(input:io.Source, maxArticleCount:Int = Int.MaxValue): Iterator[Document] = {
    new Iterator[Document] {
      //private val wikiModel = new WikiModel("http://www.mywiki.com/wiki/${image}", "http://www.mywiki.com/wiki/${title}")
      //private val wikiModel = new WikiModel("${image}", "${title}")
      //private val plainConverter = new PlainTextConverter()
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
        //val nonBracket = "(?:(?:.(?!\\[\\[|\\]\\]))*)"
        val nonBracket = "[^\\[\\]]*"
        //val pairedBracket = s"\\[\\[$nonBracket(?:\\[\\[$nonBracket\\]\\]$nonBracket)*\\]\\]"
        val cleaningRegex = ("(?s)" + (List( // Make "." match also match newline
          "&lt;!--(?:.(?!--&gt;))+.--&gt;", // Remove comments
          "\\{\\|(?:.(?!\n\\|\\}))+.\n\\|\\}", // Remove everything {|inside|}
          "&lt;math&gt;(?:.(?!&lt;/math&gt;))*.&lt;/math&gt;", // Remove everything inside <math> and </math>
          "&lt;code&gt;(?:.(?!&lt;/code&gt;))*.&lt;/code&gt;", // Remove everything inside <code> and </code>
          "&lt;gallery(?:.(?!&lt;/gallery&gt;))*.&lt;/gallery&gt;", // Remove everything inside <gallery blah> and </gallery>
          ////"\\{\\{(?:.(?!\\{\\{|\\}\\}))+.\\{\\{(?:.(?!\\}\\}|\\{\\{))+.\\}\\}(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          //"\\{\\{(?:(?:.(?!\\{\\{|\\}\\}))+.?\\{\\{(?:.(?!\\}\\}|\\{\\{))*.\\}\\})+(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          //"\\{\\{(?:(?:.(?!\\{\\{|\\}\\}))+.?\\{\\{(?:.(?!\\}\\}|\\{\\{))*.\\}\\})+(?:.(?!\\}\\}|\\{\\{))*.?\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}.  The use of .? also handles closing with }}}}
          ////"\\{\\{(?:[^\\{]*\\{\\{[^\\}]*\\}\\}[^\\}]*)+\\}\\}", // Remove everything {{inside}}, and handle a single nesting of {{ {{}} {{}} }}
          //"\\{\\{[^\\}]*\\}\\}", // Remove everything {{inside}}, but this messes up multiple nestings of {{}}
          //s"\\[\\[(?:wikisource:)?(?:File|Image|Media):${nonBracket}.?(?:\\[\\[${nonBracket}.?\\]\\]${nonBracket}.?)*.?\\]\\]",
          
          s"\\[\\[[A-Za-z:]+:${nonBracket}(?:\\[\\[${nonBracket}\\]\\]${nonBracket})*\\]\\]", // Remove [[:wikisource:File:WilliamGodwin.jpg|left|thumb]]
          // Temporarily comment out next line:
          //s"\\[\\[[A-Za-z:]+:${nonBracket}.?(?:\\[\\[${nonBracket}.?\\]\\]${nonBracket}.?)*.?\\]\\]", // Remove [[:wikisource:File:WilliamGodwin.jpg|left|thumb]]
          "(?<!\\[)\\[(?!\\[)[^\\]]*\\]", // Remove everything [inside] but not [[inside]]
          ////"\\[\\[(?:File|Image):[^\\]]+\\]\\]", // Remove [[File:WilliamGodwin.jpg|left|thumb]]
          ////"\\[\\[(?:File|Image):(?(?:.(?!\\[\\[|\\]\\])+(?:\\[\\[(?:.(?!\\[\\[|\\]\\](?:.(?!\\[\\[|\\]\\])+)*\\]\\])))\\]\\]", // Remove [[File:WilliamGodwin.jpg|left|thumb]]
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
        var inText = false
        while (lines.hasNext && !docDone) {
          val line = lines.next
          //println(articleCount.toString+" Line>>> "+line)
          if (!insideText) {
            val titleIndex = line.indexOf("<title>")
            if (titleIndex >= 0) {
              val titleEndIndex = line.indexOf("</title>")
              title = line.substring(titleIndex+7, titleEndIndex)
            } else if (line.contains("<text") && !line.contains("</text>")) {
              insideText = true; sb append line.substring(line.lastIndexOf('>')+1)
            }
          } else {
            if (line.contains("</text>")) { insideText = false; docDone = true; sb append line.substring(0, line.indexOf('<')) }
            else { sb append line; sb append '\n' }
          }
        }
        if (!lines.hasNext) input.close()
        sb = removeNestedBrackets(sb)
        val text = cleaningRegex.replaceAllIn(sb, " ")
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
          else if (c == '}' && openCount > 0) openCount -= 1 // include (openCount > 0) because sometimes a }} will appear inside a comment.
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
