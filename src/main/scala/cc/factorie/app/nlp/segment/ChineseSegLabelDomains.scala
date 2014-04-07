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
    "B",
    "I",
    "O",
    "P"
  )
  
  freeze

  def indicatesSegmentStart(label: String): Boolean = {
    val segmentStarts = List( "P", "O", "B" )

    segmentStarts.exists( segStart => segStart equals label )
  }

  def getLabeledCharacter(i: Int, line: String): (String, String) = {

    val label =
      if(isPunctuation(line(i))) "P"
      else if(isFirst(i, line) && isLast(i, line)) "O"
      else if(isFirst(i, line)) "B"
      else "I"

    (line.slice(i, i+1), label)
  }
}

trait SegmentedCorpusLabeling {

  def indicatesSegmentStart(label: String): Boolean
  
  //Labels a pre-segmented training set based on this tag set: 
  //B (beginning) I (inner) O (solitary) P (punctuation)
  def getLabeledCharacters(corpus: File): IndexedSeq[(String, String)] = {

    val labeledCorpus =
      (for{
         line <- scala.io.Source.fromFile(corpus, "utf-8").getLines
         i <- 0 until line.size
         if !isWhiteSpace(line(i))
       } yield getLabeledCharacter(i, line)
      ).toList.foldRight(IndexedSeq[(String, String)]())(_+:_)

    labeledCorpus }

  def getLabeledCharacters(document: Document): IndexedSeq[(String, String)] = {

    val docString = document.string
    val labeledCorpus =
      (for{
         i <- 0 until docString.size
         if !isWhiteSpace(docString(i))
       } yield getLabeledCharacter(i, docString)
      ).toList.foldRight(IndexedSeq[(String, String)]())(_+:_)

    labeledCorpus
  }

  //Returns a 2-tuple of an instance of character from a training set mapped to its tag
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
  def isFirst(i: Int, line: String): Boolean = (i == 0 || isWhiteSpace(line(i-1)))

  //Checks if a character in a training set is last in a word
  def isLast(i: Int, line: String): Boolean = (i == (line.size - 1) || isWhiteSpace(line(i+1)))

  def isPunctuation(character: Char): Boolean = {

    val punctuationChars = 
      List( (0x3000, 0x303F), (0x2400, 0x243F), (0xFF00, 0xFF04), 
            (0xFF06, 0xFF0D), (0xFF1A, 0xFFEF), (0x2000, 0x206F), 
            (0x0021, 0x002C), (0x002E, 0x002F), (0x003A, 0x0040), 
            (0x005B, 0x0060), (0x007B, 0x007E) )
    
    punctuationChars.exists( range => character >= range._1 && character <= range._2 )
  }

  def isEndOfSentence(character: String): Boolean = {
    
    val EOSChars = List( 0x3002, 0xFF0C, 0x002C, 0x002E ) 

    EOSChars.exists( punct => character equals punct.toString )
  }

  def isWhiteSpace(character: Char): Boolean = 
    List( (0x0000, 0x0020), (0x0085, 0x0085), (0x2028, 0x2029) ).exists( range => character >= range._1 && character <= range._2)
}
