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

package cc.factorie.app.chain
import collection.mutable
import io.{BufferedSource, Source}
import cc.factorie.app.nlp.{TokenSpan, Sentence, Document, Token}
import collection.mutable.ArrayBuffer

/** Methods of retrieving the lexicons that a token in a document (using the window around the token) or a span matches into
  * returns the lexicons names, and the location the token matches into the lexicon (like B-label, I-label, U-label, or L-label)
    @author anzaroot */
class Lexicons( val sources : List[(String,BufferedSource)]) {
  val lexiconMap = mutable.HashMap[String, List[String]]()
  val lexiconNames = ArrayBuffer[String]()

  for(s <- sources) {
    val filename = s._1
    lexiconNames += filename
    println("Reading lexicon "+filename)
    val source = s._2
    source.getLines().foreach{ l =>
      if(removeTrail(l).length > 2)
        setOrUpdate(removeTrail(l),filename)
    }
    source.close()
  }

  def removeTrail(s : String) : String = s.replaceAll("\\.|,|\\(|\\)","").replaceAll("  +"," ").trim.toLowerCase

  def setOrUpdate(s : String, filename : String) = if(lexiconMap.contains(s.toLowerCase)) { lexiconMap(s) = lexiconMap(s) ++ List(filename); } else { lexiconMap(s) = List(filename); }

  def apply(token : Token) : List[String] = {
    val phrase = getPhrase(token)
    val keys = subsect(phrase, token, 7).filter( k => removeTrail(k.head.string) != "" && removeTrail(k.last.string) != ""  )

    var lexes = List[String]()
    for(keyPre <- keys) {
      val key = removeTrail(keyPre.map(_.string).mkString(" "))
      if(lexiconMap.contains(key) && (removeTrail(token.string) != "" || (keyPre.head.position < token.position && keyPre.last.position > token.position ))) {
        lexes = lexiconMap(key).map(locate(token, keyPre) + _) ::: lexes
        //println("Found for token: " + token.string + " with key: " + keyPre + " the lexicons: " + lexiconMap(key).mkString(" , "))
        //println("And phrase: " + phrase.map( _.string ).mkString(" "))
      }
    }
    lexes
  }

  def apply(span : TokenSpan) : List[String] = {
    if(lexiconMap.contains(removeTrail(span.phrase))) lexiconMap(removeTrail(span.phrase)) else List[String]()
  }


  def subsect(phrase :Seq[Token], token : Token, maxOutLength : Int) : List[List[Token]] = {
    val middle = phrase.zipWithIndex.filter( _._1 == token).head._2
    var keys = List[List[Token]]()
    for(i <- 0 to maxOutLength) {
      var start = middle
      for(j <- 0 to i) {
        start = middle-j
        var key : List[Token]= List[Token]()
        if(start > -1 && (start+i) < phrase.size) {
          for(k <- 0 to i) {
            key =  key ++ List(phrase(start+k))
          }
          keys = key :: keys
        }
      }
    }
    keys
  }

  def locate(token : Token, key : List[Token]) : String = {
    if (key.length == 1) return "U-"
    if(token.position == key.head.position) "B-"
    else if (token.position == key.last.position) "L-"
    else "I-"
  }

  def getPhrase(token : Token) : Seq[Token] = {
    val fullPhrase = new ArrayBuffer[Token]()
    val start = if(token.position-7 >= 0) token.position-7 else 0
    val end = if (token.position+7 < token.section.length) token.position+7 else token.section.length-1
    for (i <- start to end) fullPhrase += token.section.tokens(i)
    fullPhrase.toSeq
  }

}

object Lexicons {
  def apply(dir : String, lexicons : List[String]) : Lexicons = {
       new Lexicons(lexicons.map(x => (x,scala.io.Source.fromFile(dir + "/" + x))))
  }
}