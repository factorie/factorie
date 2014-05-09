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
package cc.factorie.app.nlp.segment

import java.io.File
import scala.collection.mutable.ArrayBuffer
import cc.factorie._
import cc.factorie.app.nlp._
import cc.factorie.variable._

abstract class SegmentationLabelDomain
  extends CategoricalDomain[String]
  with SegmentedCorpusLabeling

object BIOSegmentationDomain extends SegmentationLabelDomain {

  this ++= Vector(
    "RR",
    "LR",
    "LL",
    "MM"
  )
  
  freeze

  def indicatesSegmentStart(label: String): Boolean = {
    val segmentStarts = List( "LL", "LR" )

    segmentStarts.exists( segStart => segStart equals label )
  }

  def isSolitary(label: String): Boolean = label equals "LR"

  def getLabeledCharacter(i: Int, line: String): (String, String) = {

    val label =
      if(isFirst(i, line) && isLast(i, line)) "LR"
      else if(isFirst(i, line)) "LL"
      else if(isLast(i, line)) "RR"
      else "MM"

    (line.slice(i, i+1), label)
  }
}

trait SegmentedCorpusLabeling {

  def indicatesSegmentStart(label: String): Boolean

  def isSolitary(label: String): Boolean

  def getLabeledCharacters(corpus: File): IndexedSeq[IndexedSeq[(String, String)]] = {

    val fileLines = scala.io.Source.fromFile(corpus, "utf-8").getLines.toList
    val labeledCorpus =
      fileLines.map( 
        line => (0 until line.size).filter( 
          i => !isWhiteSpace(line(i)) 
        ).map( 
          i => getLabeledCharacter(i, line) 
        ).toIndexedSeq
      ).toIndexedSeq

    labeledCorpus
  }

  def getLabeledCharacters(document: Document): IndexedSeq[IndexedSeq[(String, String)]] = {

    val docString = document.string
    val labeledCorpus = (0 until docString.size).filter( 
      i => !isWhiteSpace(docString(i)) 
    ).map( 
      i => getLabeledCharacter(i, docString) 
    ).toIndexedSeq

    IndexedSeq(labeledCorpus)
  }

  def getLabeledCharacter(i: Int, line: String): (String, String)

  def getWhiteSpaceOffsets(content: String): IndexedSeq[Int] = {

    val offsets = new ArrayBuffer[Int]

    offsets += 0

    var count = 0

    ( 0 until content.size ).foreach{ i =>
      if(isWhiteSpace(content(i))) count += 1
      else offsets += count
    }

    offsets
  }

  //Checks if a character in a training set is first in a word
  def isFirst(i: Int, line: String): Boolean = 
    (i == 0 || isWhiteSpace(line(i-1)) && !isWhiteSpace(line(i)))

  //Checks if a character in a training set is last in a word
  def isLast(i: Int, line: String): Boolean = 
    (i == (line.size - 1) || isWhiteSpace(line(i+1)) && !isWhiteSpace(line(i)))

  def isEndOfSentence(character: Char): Boolean = {

    List( 0x002C,
          0x3002,
          0xFE50,
          0xFE52,
          0xFE54,
          0xFE56,
          0xFE57,
          0xFF01,
          0xFF0C,
          0xFF1B,
          0xFF1F,
          0xFF61
    ).exists(
      punct => character == punct
    )
  }

  def isWhiteSpace(character: Char): Boolean = {

    List( (0x0000, 0x0020), 
          (0x0085, 0x0085), 
          (0x2000, 0x200F),
          (0x2028, 0x202F),
          (0x205F, 0x206F),
          (0x3000, 0x3000)
    ).exists( 
      range => character >= range._1 && character <= range._2
    )
  }

}
