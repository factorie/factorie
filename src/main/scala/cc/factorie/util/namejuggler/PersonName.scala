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

import cc.factorie.util.namejuggler.StringUtils._

object PersonName {
  /**
   * Whenever one field is empty and the other is full, use the full one.  If both are full but don't match, emit a warning.
   * @param primary
   * @param secondary
   * @return
   */
  def merge(primary: PersonName, secondary: PersonName): CanonicalPersonName = {
    new CanonicalPersonName {
      override val preferredFullName = OptionUtils.mergeWarn(primary.preferredFullName, secondary.preferredFullName)
      override val prefixes = primary.prefixes ++ secondary.prefixes
      // ** careful: what if one variant has just the first name, and the other also has middle names?  Or, middle name vs. middle initial, etc.
      override val givenNames = combineGivenNames(Set(primary.givenNames, secondary.givenNames))
      //SeqUtils.mergeWarn(primary.givenNames, secondary.givenNames)
      override val nickNames = primary.nickNames ++ secondary.nickNames
      //OptionUtils.mergeWarn(primary.nickName, secondary.nickName)
      override val surNames = primary.surNames ++ secondary.surNames
      //SetUtils.mergeWarn(primary.surNames,
      // secondary.surNames)
      override val hereditySuffix = OptionUtils.mergeWarn(primary.hereditySuffix, secondary.hereditySuffix)
      override val degrees = primary.degrees ++ secondary.degrees //SeqUtils.mergeWarn(primary.degrees, secondary.degrees)
    }
  }

  def merge(s: Seq[PersonName]): PersonName = s.reduceLeft[PersonName](merge)

  private def bestIfCompatibleMaybeInitial(x: NonemptyString, y: NonemptyString): Option[NonemptyString] = {
    val xx = x.toLowerCase.stripPunctuation
    val yy = y.toLowerCase.stripPunctuation

    if (xx.isEmpty && yy.isEmpty) None
    else if (xx.isEmpty) Some(y)
    else if (yy.isEmpty) Some(x)
    else if (xx == yy) {
      if (x.length > y.length) Some(x) else Some(y)
    }
    else {
      val xI = xx.head
      val yI = yy.head

      if ((xx.length == 1) && xI == yI) Some(y)
      else if ((yy.length == 1) && xI == yI) Some(x)
      else None
    }
  }

  // see also noMismatchingNameOrInitial
  def combineGivenNames(a: Seq[NonemptyString], b: Seq[NonemptyString]): Seq[NonemptyString] = {

    val longer = a //.map(_.toLowerCase.stripPunctuation.n)
    val shorter = b //.map(_.toLowerCase.stripPunctuation.n)

    // should do a real alignment here
    // instead, for now, require that every element of the shorter sequence match something in the longer sequence, in order

    //val (s, l) = if (bb.length >= aa.length) (aa, bb) else (bb, aa)

    def mergeGivenNamesRecursion(longer: List[NonemptyString], shorter: List[NonemptyString]): List[NonemptyString] = {


      if (shorter.isEmpty) longer
      else
      if (longer.isEmpty) shorter
      else {
        bestIfCompatibleMaybeInitial(longer.head, shorter.head).map(
          x => x :: mergeGivenNamesRecursion(longer.tail, shorter.tail) // consume head of each sequence
        ).getOrElse(
          mergeGivenNamesRecursion(longer.tail, shorter) // drop incompatible entry from longer sequence
        )

      }
    }

    mergeGivenNamesRecursion(longer.toList, shorter.toList)


  }

  def combineGivenNames(set: Set[Seq[NonemptyString]]): Seq[NonemptyString] = {
    // ** align all the sequences, allowing initials to align with full names, and output the consensus sequence

    // note we've already decided that these are compatible (one matching 

    val longestFirst = set.toList.sortBy((x: Seq[NonemptyString]) => {
      -x.length
    })

    val result = longestFirst.reduceLeft((a, b) => combineGivenNames(a, b))

    result

    // throw new NotImplementedException
    // ** just pick the longest for now


    //val longest = SeqUtils.argMax(set, (s: Seq[NonemptyString]) => s.map(_.length).sum)

    // if there is a tie, pick the one with more elements
    //SeqUtils.argMax(longest, (s: Seq[NonemptyString]) => s.size).head
  }
}

/**
 * An attempt to represent names as a set of canonical atomic fields.  Being really comprehensive and accurate about this is not possible due to too many
 * cultural variations and ambiguities.  Still this should cover most of the cases we care about re authorship of journal articles.
 *
 * A name is not a fixed thing; it is a probabilistic cloud of strings, all denoting the same person.  Here we don't cover the case that a person changes
 * names completely; in that case there are two disjoint clouds of strings, so that should be modeled by allowing a Person to have multiple PersonNames.
 *
 * Here we try to model different representations of "the same name".  Variations may include: omitting some components; using initials for some components;
 * reordering; etc.  The most "different" case to model is that of married names vs. maiden names.  Since one or both of these may appear,
 * but the other name components are not affected, we consider this a case of multiple surnames within one name.
 *
 * Subclasses propagate name fragments around the various representations, in an attempt to provide some reasonable value for each field.
 *
 * Here we want to take multiple name variants as input and coordinate them into a single record.  For instance,
 * if we assert that Amanda Jones and A. Jones-Archer are the same person, then we should later recognize Amanda Archer as a valid variant.
 */
trait PersonName {
  /** Out of the cloud of possible name representations, the person probably prefers one variant.  This is how we know to use the first-initial form,
    * or a married name, etc.
    * @return
    */
  def preferredFullName: Option[NonemptyString] = None

  def prefixes: Set[NonemptyString] = Set.empty

  // typically first name + middle initial, etc.
  def givenNames: Seq[NonemptyString] = Nil

  def nickNames: Set[NonemptyString] = Set.empty

  final def nickNamesInQuotes: Option[NonemptyString] = nickNames.map(s => NonemptyString("'" + s.toString + "'")).mkString(" ")

  final def allGivenAndNick: Seq[NonemptyString] = (givenNames ++ nickNamesInQuotes).toSeq

  /**
   * Each element of this list should be a complete and valid surname (i.e., using only one should produce a valid full name)
   *
   * Includes single surnames, multiple sequential surnames (in joined form), hyphenated names (in joined form), maiden names, and married names.
   *
   * The point is to facilitate recognizing both "Jorge Martinez" and "Ivan Renteria" as variants of "Jorge Ivan Renteria Martinez".
   *
   * Also: Camille Rosenthal-Sabroux Lamsade might have surnames "Rosenthal-Sabroux Lamsade", "Rosenthal-Sabroux", "Rosenthal", "Sabroux",
   * and "Lamsade".  Here we should record only those variants that are actually observed, but these may later be tokenized for matching purposes.
   *
   * Names with particles should simply include the particle, e.g. "de la Mouliere".  The later tokenization for matching should produce at least
   * "Mouliere" and "la Mouliere", since it's not predictable which particles are "dropping" and which are not.
   *
   * Variants that can be inferred at matching time (e.g., with particles removed, or accented characters reduced to roman) need not be stored explicitly.
   *
   * Surnames may also include known alternate spellings, e.g. Jouline/Zhulin
   */
  def surNames: Set[NonemptyString] = Set.empty

  // e.g., Jr. or III
  def hereditySuffix: Option[NonemptyString] = None

  // ** Careful: don't duplicate degrees when merging, but also don't assume they're unique (Kermit the Frog, Ph.D., Ph.D.)
  // ** no problem: assume unique for now, and assume order doesn't matter
  def degrees: Set[NonemptyString] = Set.empty

  def fieldsInCanonicalOrder: Seq[Iterable[NonemptyString]] = Seq(prefixes, givenNames, nickNamesInQuotes, surNames, hereditySuffix, degrees, preferredFullName)

}

/**
 * allow declaring a record canonical, to ensure that there are no explicit derivations
 */
trait CanonicalPersonName extends PersonName {

  lazy val withDerivations = new CanonicalPersonNameWithDerivations(this)

  override def toString = withDerivations.toString

  //def inferFully: PersonNameWithDerivations = withDerivations.inferFully


  /**
   * Very liberal definition: just look for incontrovertible conflicts
   * @param other
   * @return
   */
  def compatibleWith(other: CanonicalPersonName): Boolean = {


    def noMismatchingNameOrInitial(a: Seq[NonemptyString], b: Seq[NonemptyString]): Boolean = {
      lazy val aa = a.map(_.toLowerCase.stripPunctuation)
      lazy val bb = b.map(_.toLowerCase.stripPunctuation)

      // should do a real alignment here
      // instead, for now, require that every element of the shorter sequence match something in the longer sequence, in order

      val (s, l) = if (bb.length >= aa.length) (aa, bb) else (bb, aa)

      def isCompatible(s: Seq[NonemptyString], l: Seq[NonemptyString]): Boolean = {

        def compatibleMaybeInitial(x: NonemptyString, y: NonemptyString): Boolean = {
          val xI = x.s.head
          val yI = y.s.head
          x == y || ((x.length == 1 || y.length == 1) && xI == yI)
        }

        if (s.isEmpty) true
        else
        if (l.isEmpty) false
        else {
          if (compatibleMaybeInitial(s.head, l.head))
            isCompatible(s.tail, l.tail) // consume head of each sequence
          else
            isCompatible(s, l.tail) // drop incompatible entry from longer sequence
        }
      }
      isCompatible(s, l)

    }

    def atLeastOneMatchingNameOrInitial(a: Set[NonemptyString], b: Set[NonemptyString]): Boolean = {
      lazy val aa = a.map(_.toLowerCase)
      lazy val bb = b.map(_.toLowerCase)

      // At least one given name or nickname matches fully
      lazy val matching = aa.intersect(bb).nonEmpty

      // if a given name is provided only as an initial, see if a full given name or initial from the other name matches
      lazy val aInitialsOnly: Set[String] = aa.filter(_.s.stripPunctuation.length == 1).map(x => x.s.head.toString)
      lazy val bInitialsOnly: Set[String] = bb.filter(_.s.stripPunctuation.length == 1).map(x => x.s.head.toString)
      lazy val aAsInitials: Set[String] = aa.map(x => x.s.head.toString)
      lazy val bAsInitials: Set[String] = bb.map(x => x.s.head.toString)

      lazy val matchingInitial = aInitialsOnly.intersect(bAsInitials).nonEmpty || bInitialsOnly.intersect(aAsInitials).nonEmpty

      a.isEmpty || b.isEmpty || matching || matchingInitial
    }



    // ** these are redundant?  Well, maybe not, due to withDerivations...
    val matchingFirst = noMismatchingNameOrInitial(allGivenAndNick, other.allGivenAndNick)

    val matchingMiddle = noMismatchingNameOrInitial(withDerivations.middleNames, other.withDerivations.middleNames)

    val sA = surNames.flatMap(expandSurname)
    val sB = other.surNames.flatMap(expandSurname)

    val matchingLast = atLeastOneMatchingNameOrInitial(sA, sB)
    // val sAB = sA.intersect(sB)
    // val matchingLast = sAB.nonEmpty

    val result = matchingLast && matchingFirst && matchingMiddle
    result
  }

  private def expandSurname(s: NonemptyString): Set[NonemptyString] = {
    import StringUtils._
    val deAccented = NonemptyString(s.s.deAccent)
    val base: Set[String] = if (deAccented == s) {
      Set(s)
    } else {
      Set(s, deAccented)
    }
    base.map(_.toLowerCase).flatMap(removeParticles).flatMap(splitOnWhitespace).flatMap(splitOnHyphens)
  }

  private def removeParticles(s: String): Set[String] = {
    val result = s.s.split(" ").scanRight(List[String]())((element: String, prev: List[String]) => (element :: prev))
    result.map(_.mkString(" ")).toSet
  }

  private def splitOnHyphens(s: String): Set[String] = {
    s.s.split("-").toSet
  }

  private def splitOnWhitespace(s: String): Set[String] = {
    s.s.split("\\s").toSet
  }

  private def splitOnSpacesAndHyphens(s: String): Set[String] = {
    s.s.split("[ -]").toSet
  }

}

object InferredCanonicalPersonName {
  implicit def inferCanonical(n: PersonNameWithDerivations) = new InferredCanonicalPersonName(n)
}

/**
 * Infer any empty canonical fields, if possible, from provided derived fields.
 *
 * The approach is to generate full names from the derived fields, and then parse those full names back to canonical fields.  On the one hand,
 * that risks losing information.  On the other hand, this is generally used upstream of a merge where the "correct" derived fields will take priority anyway.
 * Also, this may help clean up mistagged data.
 *
 * In cases of single-value fields, nonempty explicit data overrides implicit data resulting from full-name parsing.  Thus, e.g.,
 * an explicit Mr. overrides an implicit Dr.  Should there be precedence rules?
 *
 * Set-valued fields are just merged.
 *
 * Note relationship with PersonName.merge().  Really we want to a) derive canonical fields only from derived fields; b) merge those with existing canonical
 * fields.
 *
 *
 * @param n
 */
class InferredCanonicalPersonName(n: PersonNameWithDerivations) extends CanonicalPersonName {
  private val nParsedFullNames = n.originalFullNames.map(n => PersonNameParser.parseFullName(n))

  // ** if the prefix is populated in more than one input, pick a random one.  Better: emit warning, choose best (?)
  override val prefixes = nParsedFullNames.flatMap(_.prefixes)
  override val givenNames = PersonName.combineGivenNames(nParsedFullNames.map(_.givenNames))
  override val nickNames = nParsedFullNames.flatMap(_.nickNames)
  override val surNames = nParsedFullNames.flatMap(_.surNames).toSet
  // e.g., Jr. or III
  // ** if the hereditySuffix is populated in more than one input, pick a random one.  Better: emit warning, choose best (?)
  override val hereditySuffix = nParsedFullNames.map(_.hereditySuffix).flatten.headOption
  override val degrees = nParsedFullNames.toSeq.map(_.degrees).flatten.toSet
}
