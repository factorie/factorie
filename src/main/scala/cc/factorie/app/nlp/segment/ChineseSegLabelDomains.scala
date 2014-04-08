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

<<<<<<< HEAD
  def isPunctTag(label: String): Boolean
  
  def getLabeledCharacters(corpus: File): IndexedSeq[(String, String)] = {
=======
  def getLabeledCharacters(corpus: File): IndexedSeq[IndexedSeq[(String, String)]] = {
>>>>>>> punctmod

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

<<<<<<< HEAD
  def isFirst(i: Int, line: String): Boolean = (i == 0 || isWhiteSpace(line(i-1)) && !isWhiteSpace(line(i)))

  def isLast(i: Int, line: String): Boolean = (i == (line.size - 1) || isWhiteSpace(line(i+1)) && !isWhiteSpace(line(i)))

  def isPunctuation(character: Char): Boolean = {

    List( (0x0021, 0x002D), 
          (0x002F, 0x002F),
          (0x003A, 0x0040), 
          (0x005B, 0x0060), 
          (0x007B, 0x007E),
          (0x2010, 0x2010),
          (0x2015, 0x2027),
          (0x2030, 0x205E),
          (0x2400, 0x243F), 
          (0x3001, 0x303F), 
          (0xFE50, 0xFE57),
          (0xFE59, 0xFE61),
          (0xFE68, 0xFE68),
          (0xFF00, 0xFF04), 
          (0xFF06, 0xFF0D), 
          (0xFF0F, 0xFF0F),
          (0xFF1A, 0xFFEF) 
    ).exists( 
      range => character >= range._1 && character <= range._2 
    )
  }

  def isEndOfSentence(character: Char): Boolean = {
    
    List( 0x002C, 
=======
  //Checks if a character in a training set is first in a word
  def isFirst(i: Int, line: String): Boolean = 
    (i == 0 || isWhiteSpace(line(i-1)) && !isWhiteSpace(line(i)))

  //Checks if a character in a training set is last in a word
  def isLast(i: Int, line: String): Boolean = 
    (i == (line.size - 1) || isWhiteSpace(line(i+1)) && !isWhiteSpace(line(i)))

  def isEndOfSentence(character: Char): Boolean = {

    List( 0x002C,
>>>>>>> punctmod
          0x3002,
          0xFE50,
          0xFE52,
          0xFE54,
          0xFE56,
          0xFE57,
<<<<<<< HEAD
          0xFF01, 
          0xFF0C, 
          0xFF1B, 
          0xFF1F, 
          0xFF61
    ).exists( 
      punct => character == punct 
    )
  }

  def isNumerical(character: Char): Boolean = {

    List( (0x0030, 0x0039),
          (0x4E00, 0x4E00),
          (0x4E03, 0x4E03),
          (0x4E07, 0x4E07),
          (0x4E09, 0x4E09),
          (0x4E5D, 0x4E5D),
          (0x4E8C, 0x4E8C),
          (0x4E94, 0x4E94),
          (0x4EBF, 0x4EBF),
          (0x5104, 0x5104),
          (0x5146, 0x5146),
          (0x516B, 0x516B),
          (0x516D, 0x516D),
          (0x5341, 0x5341),
          (0x5343, 0x5343),
          (0x56DB, 0x56DB),
          (0x767E, 0x767E),
          (0x842C, 0x842C),
          (0x96F6, 0x96F6),
          (0xFF10, 0xFF19)
=======
          0xFF01,
          0xFF0C,
          0xFF1B,
          0xFF1F,
          0xFF61
>>>>>>> punctmod
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
