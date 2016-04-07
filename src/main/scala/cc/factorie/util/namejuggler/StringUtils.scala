/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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
package cc.factorie.util.namejuggler

import java.util.regex.Pattern

/**
 * Shamelessly yoinked from edu.umass.cs.iesl.scalacommons
 */
object StringUtils {
  implicit def toOptionNonempty(s: String): Option[NonemptyString] = if (s.trim.isEmpty) None else Some(new NonemptyString(s.trim))

  // don't make this implicit; that would mask implicit conversions in Predef, providing String.size, String.nonEmpty, etc.
  //def toSingletonSetNonempty(s: String): Set[NonemptyString] = toOptionNonempty(s).toSet

  implicit def toSetNonempty[T <: Set[String]](ss: T): Set[NonemptyString] = ss.flatMap(toOptionNonempty)

  implicit def toSeqNonempty[T <: Seq[String]](ss: T): Seq[NonemptyString] = ss.flatMap(toOptionNonempty)


  //** need to understand CanBuildFrom etc. to make this work right
  //implicit def toTraversableNonempty[T <: Traversable[String]](ss: T): T[NonemptyString] = ss.flatMap(toOptionNonempty)

  //def toTraversableNonempty2[B, That, Repr](ss: B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {}
  // def flatMap[B, That](f: A => TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {}

  // this interferes with toOptionNonempty
  //implicit def stringToOptionInt(s: String): Option[Int] = if (s.trim.isEmpty) None else Some(s.toInt)

  implicit def enrichString(s: String): RichString = new RichString(s)

  //** just use NonemptyString.unapply
  implicit def unwrapNonemptyString(n: NonemptyString): String = n.s

  // this is bad because they confound map, size, etc. operations from String and Option
  //implicit def unwrapNonemptyString(n: Option[NonemptyString]): String = n.map(unwrapNonemptyString).getOrElse("")

  // but this should be OK, because it requires an explicit "unwrap" or whatever
  implicit def enrichNonemptyString(n: Option[NonemptyString]): OptionNonemptyString = new OptionNonemptyString(n)

  //implicit def wrapNonemptyString(s: String) = NonemptyString(s)
}

class OptionNonemptyString(val o: Option[NonemptyString]) {
  def unwrap: String = o.map(_.s).getOrElse("")
}

object RichString {

  final private val deAccentPattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+")
  final private val trimPunctuationRE = "^\\p{Punct}*(.*?)\\p{Punct}*$".r
}

class RichString(val s: String) {

  import java.text.Normalizer

  import RichString._

  def maskNewlines: String = s.replaceAll("[\\n\\r]+", " ")

  def maskNewlinesAndTabs: String = s.replaceAll("[\\n\\r\\t]+", " ")

  def stripWhitespace: String = s.replaceAll("\\s", "")

  def maskPunctuation: String = s.replaceAll("\\p{Punct}+", " ")

  def stripPunctuation: String = s.replaceAll("\\p{Punct}+", "")

  def trimPunctuation: String = {
    val trimPunctuationRE(result) = s
    result
  }

  def maskAllButWord: String = s.replaceAll("[^\\w\\s]+", " ")

  def stripVowels: String = s.replaceAll("[AEIOUaeiou]", "")

  def collapseWhitespace: String = s.replaceAll("\\s+", " ")

  def opt: Option[NonemptyString] = StringUtils.toOptionNonempty(s)

  def n: NonemptyString = new NonemptyString(s.trim)

  def just: Set[NonemptyString] = opt.toSet

  def limit(len: Int): String = s.substring(0, math.min(s.length, len))

  def firstLine = {
    // surely there is a more idiomatic solution?
    val i: Int = s.indexOf("\n")
    i match {
      case -1 => s
      case _ => limit(i)
    }
  }

  def limitAtWhitespace(len: Int, suffixIfLimited: String) = {
    val l = limit(len + 1) // allow for a space after the last retained word
    if (l.length < s.length) {
      val i = l.lastIndexOf(" ")
      if (i >= 0) {
        val r = l.substring(0, i)
        (r + suffixIfLimited)
      }
      else l + suffixIfLimited
    } else s

  }

  //http://stackoverflow.com/questions/1008802/converting-symbols-accent-letters-to-english-alphabet
  // see also icu4j Transliterator-- better, but a 7 MB jar, yikes.
  // Note this does not catch all interesting Unicode characters, e.g. Norwegian O-slash.  http://stackoverflow.com/questions/8043935/normalizing-unaccenting-text-in-java
  lazy val deAccent: String = {
    val nfdNormalizedString = Normalizer.normalize(s, Normalizer.Form.NFD)
    val result = deAccentPattern.matcher(nfdNormalizedString).replaceAll("")
    result
  }


  def containsLowerCase: Boolean = deAccent.find(_.isLower).isDefined

  /*{
    val lc = """[a-z]""".r
    val r = lc.findFirstIn(deAccent)
    r.nonEmpty
  }*/

  def containsUpperCase: Boolean = deAccent.find(_.isUpper).isDefined

  /*{
    val lc = """[A-Z]""".r
    val r = lc.findFirstIn(deAccent)
    r.nonEmpty
  }*/

  def isAllUpperCase: Boolean = containsUpperCase && !containsLowerCase

  def isAllLowerCase: Boolean = containsLowerCase && !containsUpperCase

  def isMixedCase: Boolean = containsLowerCase && containsUpperCase
}

case class NonemptyString(s: String) extends Ordered[NonemptyString] {
  require(s.nonEmpty, "Expected non-empty String")

  override def toString = s

  override def equals(other: Any): Boolean = other match {
    case that: NonemptyString => this.s == that.s
    case _ => false
  }

  override def hashCode: Int = s.hashCode


  //def +(that:NonemptyString) = new NonemptyString(s + that.s)
  def compare(that: NonemptyString) = s.compare(that.s)
}
