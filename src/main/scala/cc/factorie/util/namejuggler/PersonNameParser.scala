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

import scala.annotation.tailrec


/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
/**
 * This could be a crf...
 * @return
 */
object PersonNameParser {

  import cc.factorie.util.namejuggler.PersonNameFormat._

  private val splitFirst = """^(.*?)[ ]+(.*)$""".r

  def stripPrefixes(s: String): (Set[NonemptyString], String) = {
    try {
      val splitFirst(firstToken, remainder) = s.trim
      if (isPrefix(firstToken)) {
        val (p, r) = stripPrefixes(remainder)
        val f: Option[NonemptyString] = firstToken
        f.map(ne => (p + ne, r)).getOrElse((p, r))
      }
      else (Set.empty, s)
    }
    catch {
      case e: MatchError => (Set.empty, s)
    }
  }

  private val splitLast = """^(.*[^, ])([, ]+)(.*)$""".r

  def findHasFirstName(s:String) : Boolean = {
    val sa = s.split(" ")
    sa.size > 1 && !isSurnameParticle(sa.head)
  }
  def findHasFirstName(sa:Seq[String]) : Boolean = {
    sa.size > 1 && !isSurnameParticle(sa.head)
  }
  
  /**
   * @param s
   * @param containsLowerCase
   * @return triple of (heredity suffixes, degree suffixes, core name string)
   */
  def stripSuffixes(s: String, containsLowerCase: Boolean): (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = {
    // the last token may be separated by a space or a comma, and it's often not meaningful either way.
    // e.g. Brock, Stuart, Ph.D. == Brock, Stuart PHD
    // but Smith, John, MD PHD  is helpful
    // what about Smith, John, Jr. (never happens?)

    // anything after a second comma is a degree or a heredity suffix, whether separated by comma or space.
    // Brock, Stuart, Ph.D., III, M.D.   we even accept this!?

    val tokensByCommas = s.split(",").map(_.trim)
    
    // edge case where s == ",", returns None as the name string
    if (tokensByCommas.length == 0)
      (None, Set.empty, None)
    else if (tokensByCommas.length == 1) {
      stripSuffixesNoCommas(tokensByCommas(0), containsLowerCase, false, findHasFirstName(tokensByCommas(0)))
    }
    else if (tokensByCommas.length == 2) {
      val preComma = tokensByCommas(0)
      val postComma = tokensByCommas(1) // we know this will resolve
      stripSuffixesOneComma(preComma, postComma, containsLowerCase)
    }
    else {
      //if(tokensByCommas.length > 2) {

      // look for Smith, John, MD PHD

      val degreesOrHereditySuffixes = tokensByCommas.drop(2).flatMap(_.split(" ")).flatMap(_.opt)
      val (hereditySuffixes, degrees) = degreesOrHereditySuffixes.partition(x => isHereditySuffix(x.s))
      val preComma = tokensByCommas(0)
      val postComma = tokensByCommas(1) // we know this will resolve
      val (h, d, r) = stripSuffixesOneComma(preComma, postComma, containsLowerCase)
      (h.map(x => x +: hereditySuffixes).getOrElse(hereditySuffixes).mkString(" "), d ++ (degrees.map(x=>fixDegree(x).n)), r)
    }
  }


  def stripSuffixesOneComma(preComma: String, postComma: String, containsLowerCase: Boolean): (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = {

    // Garcia Molina, Hector Alvarez
    // Garcia Molina, Hector Alvarez Ph.D.  ** don't accept; degree requires comma if name is inverted.
    // Garcia Molina, Hector Alvarez III  ** don't accept; heredity requires comma if name is inverted.

    // Garcia Molina, Bargle Greeber   ** not detected as given names

    // Alvarez de Garcia Molina, Hector III  ** don't accept; heredity requires comma if name is inverted.

    if (isGivenNames(postComma)) {
      val s = preComma + ", " + postComma // don't uninvert here, because that loses the given vs. last distinction useful later.
      (None, Set.empty, s)
    }
    else {
      val hasFirstName = findHasFirstName( preComma)
      val (preh, pred, prer) = stripSuffixesNoCommas(preComma, containsLowerCase, false, hasFirstName)
      val (posth, postd, postr) = stripSuffixesNoCommas(postComma, containsLowerCase, true, hasFirstName)
      (Seq(preh, posth).flatten.mkString(" "), pred ++ postd, Seq(prer, postr).flatten.mkString(", "))
      /*
      // lop off one space token and try again

      val postCommaSpaceTokens: Array[String] = postComma.split(" ")
      val lastSpaceToken = postCommaSpaceTokens.last
      
      val degreesOrHereditySuffixes = postComma
      
      
      
      val lastNameOnlyBeforeComma = {
        val preCommaTokens: Array[String] = preComma.split(" ")
        val result = isSurnameParticleNoCase(preCommaTokens.head) || preCommaTokens.size == 1
        result
      }
      if(lastNameOnlyBeforeComma) {
        
      }*/
    }
  }

  def stripSuffixesNoCommas(s: String, containsLowerCase: Boolean, isPostComma: Boolean, hasFirstName: Boolean): (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = {
    val spaceTokens = s.split(" ")
    stripSuffixesNoCommas(spaceTokens, containsLowerCase, isPostComma, hasFirstName)
  }

  def stripSuffixesNoCommas(spaceTokens: Seq[String], containsLowerCase: Boolean, isPostComma: Boolean, hasFirstName: Boolean): (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = {

    if (spaceTokens.size == 0 || (spaceTokens.size == 1 && spaceTokens(0).trim.isEmpty)) {
      (None, Set.empty, "")
    } else if (spaceTokens.size == 1) {
      if(!spaceTokens(0).trim.isEmpty) {
        val t = spaceTokens(0).n
        if (isPostComma && likelyDegree(t, containsLowerCase, hasFirstName)) (None, Set(fixDegree(t.s).n), "")
        else (None, Set.empty, Some(t))
      } else {
        (None, Set.empty, "")
      }
    }
    else {
      //} if (spaceTokens.size > 1) {


      val lastToken = spaceTokens.last
      val preLastTokens = spaceTokens.dropRight(1)

      // now that commas are dealt with separately, this just grabs the last space token
      //val splitLast(preLastToken, separator, lastToken) = s.trim

      if (lastToken.isEmpty) {
        // anomalous name ending in comma; just drop it
        stripSuffixesNoCommas(preLastTokens, containsLowerCase, isPostComma, hasFirstName)
      }

      else if (isHereditySuffix(lastToken)) {
        // logger.debug("Found heredity suffix in '" + s + "': '" + lastToken + "'")
        val (h, d, r) = stripSuffixesNoCommas(preLastTokens, containsLowerCase, isPostComma, hasFirstName)
        val f: Option[NonemptyString] = lastToken
        if (h.nonEmpty) {
          //throw new PersonNameParsingException("More than one heredity suffix: " + s)
        }

        // combine heredity suffixes into a single string
        (Seq(h, f).flatten.mkString(" "), d, r)
      }


      // we don't immediately run isDegree here, because it can be hard to distinguish degrees from middle initials.
      // hence the name "likelyDegree" for that function.  Any anyway we need to use context clues.
      // Is Smith, John RN == John R. N. Smith, or John Smith, RN?
      // we interpret that case as middle initials, but Smith, John, RN as a degree.
      // what about Smith, JR ?

      else {
        def acceptDegree: (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = {
          //logger.debug("Found degree in '" + s + "': '" + lastToken + "'")
          val (h, d, r) = stripSuffixesNoCommas(preLastTokens, containsLowerCase, isPostComma, hasFirstName)
          val f: Option[NonemptyString] = fixDegree(lastToken)
          f.map(ne => (h, d + ne, r)).getOrElse((h, d, r))
        }
        def rejectDegree: (Option[NonemptyString], Set[NonemptyString], Option[NonemptyString]) = (None, Set.empty, spaceTokens.mkString(" "))

        // Garcia Molina, Beeblebrox   (first name not detected by isGivenName)
        val lastTokenCouldPossiblyBeADegree = lastToken.length < 6

        if (!lastTokenCouldPossiblyBeADegree) rejectDegree // Smith, MD is M. D. Smith.   de Broeck, Ph.D.  never happens.
        else {
          //val spaceTokens: Array[String] = preLastToken.split(" ")
          val hasFirstNameLocal = findHasFirstName(preLastTokens) // preLastTokens.size > 0 && !isSurnameParticle(preLastTokens.head)

          // if there is a first name and no comma, then the degree might be detected based on capitalization
          if ((isPostComma || hasFirstNameLocal) && likelyDegree(lastToken, containsLowerCase, hasFirstName || hasFirstNameLocal)) {
            acceptDegree // John Smith MD 
          } else {
            // if there is no first name and no comma, then any lastToken could be an inverted given name or initials,
            // e.g. Smith JA, so the only way to detect a degree is by lexicon or maybe a pronouncability measure
            rejectDegree // Smith JA
          }

        }
      }


    }
  }

  def fixCaps(s: String): String = if (s.isMixedCase) s else s.toLowerCase.capitalize

  def fixCapsExceptParticles(ss: String): String = ss.split(" ").map(s => {
    if (isSurnameParticle(s)) s.toLowerCase else if (s.isMixedCase) s else s.toLowerCase.capitalize
  }).mkString(" ")

  // Kermit Kalman the Frog, Ph.D., F.R.S.
  // the Frog, Kermit Kalman, Ph.D., F.R.S.
  // the Frog, Ph.D., F.R.S., Kermit Kalman  // ** this never happens?
  // the Frog, Kermit Kalman
  // we are not parsing lists here, but list context might be informative if multiple names have a consistent format:
  // the Frog KK, Grouch O, and Bird B.
  // KK the Frog, O Grouch, and B Bird.
  // is Jones, M.D. => Michael Douglas Jones or Dr. Jeremiah Jones, MD?  Probably the former. But MD Jones, MD is Dr. Michael Douglas Jones, M.D.
  def parseFullName(s: String): PersonName = {
    val (parsedPrefixes: Set[NonemptyString], noPrefixes: String) = stripPrefixes(s)
    val containsLowerCase = s.containsLowerCase
    val (parsedHereditySuffix: Option[NonemptyString], parsedDegrees: Set[NonemptyString], coreNameString: Option[NonemptyString]) = stripSuffixes(noPrefixes, containsLowerCase)
    require(coreNameString.isDefined)
    val coreToks: Array[Array[String]] = coreNameString.get.split(",").map(_.replace(".", ". ")).map(_.split(" ").map(_.trim).filter(!_.isEmpty))

    val coreName: PersonName = {
      if (coreToks.size == 0) {
        // no data
        new PersonName() {}
      }
      else if (coreToks.size == 1) {
        // no commas: Kermit Kalman the Frog

        parseUninvertedCore(coreToks.head)
      }
      else if (coreToks.size == 2) {
        // exactly one comma:
        // the Frog, Kermit Kalman
        new PersonNameWithDerivations {
          override val givenNames: Seq[NonemptyString] = massageGivenNames(coreToks(1).toSeq)

          // declare a single complete surname for now.  If there are several names, they should get expanded later.
          override val surNames: Set[NonemptyString] = fixCapsExceptParticles(coreToks(0).mkString(" ")).opt.toSet
        }
      }
      else {
        throw new PersonNameParsingException("Multiple commas even after removing all degrees")
      }
    }

    val extraName = new PersonName() {
      override val prefixes: Set[NonemptyString] = parsedPrefixes.map(_.toLowerCase.capitalize)
      override val hereditySuffix = parsedHereditySuffix
      override val degrees = parsedDegrees
    }

    PersonName.merge(Seq(extraName, coreName))
    /*

     // ** return the person name format actually found.  Maybe recurse to steady state?
     def parseFullName(s: String, expectedFormat: PersonNameFormat): (PersonName, PersonNameFormat) = {
       throw new NotImplementedException("Fancy name processing temporarily disabled")
     }

     */
  }

  def parseFullNameSafe(s: String): Option[PersonName] = {
    try {
      Some(parseFullName(s))
    } catch {
      case e: IllegalArgumentException =>
        None
    }
  }
  
  class PersonNameParsingException(s: String) extends Exception(s)


  private def massageGivenNames(maybePunct: Seq[String]): Seq[String] = {
    // TODO what about hyphenated names?

    // we mask only periods, not _.maskPunctuation, because of hyphenated names

    val rawGivenNames = maybePunct.map(_.s.replaceAll("\\.+", " ")).flatMap(_.split(" ")).filterNot(_.isEmpty)
    val result = if (rawGivenNames.size == 1 && rawGivenNames(0).isAllUpperCase && rawGivenNames(0).length < 4) {
      // interpret solid caps as initials
      rawGivenNames(0).stripPunctuation.split("").filter(!_.isEmpty).toSeq.map(_ + ".")
    }
    else {
      rawGivenNames.map({
        case i if (i.length == 1) => i + "."
        case i => i
      })
    }
    result.map(fixCaps(_))
  }

  private def parseUninvertedCore(nameTokens: Array[String]): PersonName = {
    // OK this is the hard part.  No commas or other structure, so we have to figure out which part is which.
    // ** assume case sensitive for now to help identify the "prelast" particle
    //nameTokens.filter(_.is)
    // detect nicknames in quotes, etc.
    // ** completely simplistic for now


    // ** detect initials

    // if there is one token, expand it
    // if there are two tokens, expand the first
    // if there are three tokens, expand the first two (probably one or the other but not both...)
    // if there are more than three, expand none.

    val nameTokensInitialsFixed: Traversable[String] = nameTokens.size match {
      case 1 => nameTokens flatMap expandInitials
      case 2 => expandInitials(nameTokens.head) ++ nameTokens.tail
      case 3 => expandInitials(nameTokens.head) ++ expandInitials(nameTokens.tail.head) ++ nameTokens.tail.tail
      case _ => nameTokens
    }
    // if a particle appears capitalized but not in the first position, lowercase it.
    // so "la Silva" will be interpereted as a surname only; Charlie La Silva turns into Charlie La Silva, but
    // "Van Morrison" is left alone

    val nameTokensParticlesFixed: List[String] = List(nameTokensInitialsFixed.head) ++ (nameTokensInitialsFixed.tail map fixParticle)

    val (givenR, sur) = splitOnCondition((s: String) => (s.isAllLowerCase))(Nil, nameTokensParticlesFixed)

    new PersonName {
      override val surNames: Set[NonemptyString] = sur.map(fixCapsExceptParticles(_)).mkString(" ").opt.toSet
      //emptyStringToNone(nameTokens.last).toSet
      override val givenNames: Seq[NonemptyString] = {
        val maybePunct = givenR.reverse
        massageGivenNames(maybePunct)
      }
    }
  }

  private def expandInitials(s: String): Traversable[String] = {
    if (s.s.isAllUpperCase && s.length <= 4) s.split("").toSeq.filterNot(_ == ".").map(_ + ".") else Some(s)
  }

  @tailrec
  private def splitOnCondition[T](condition: (T => Boolean))(a: List[T], b: List[T]): (List[T], List[T]) = {
    b match {
      case Nil => (a, b)
      case h :: l if (l.isEmpty || condition(h)) => (a, b)
      case h :: l => splitOnCondition[T](condition)(h :: a, l)
    }
  }

  /*
       /*	def parseFullName(s: String): PersonName =
            {
            throw new NotImplementedException("Fancy name processing temporarily disabled")
            }
        */
     // Kermit Kalman the Frog, Ph.D., F.R.S.
     // the Frog, Kermit Kalman, Ph.D., F.R.S.
     // the Frog, Ph.D., F.R.S., Kermit Kalman  // ** this never happens?
     // the Frog, Kermit Kalman


     // we are not parsing lists here, but list context might be informative if multiple names have a consistent format:
     // the Frog KK, Grouch O, and Bird B.
     // KK the Frog, O Grouch, and B Bird.
     def parseFullName(s: String): PersonName =
       {
       //throw new NotImplementedException("Fancy name processing temporarily disabled")
       // split first on commas, then on spaces
       val toks: Array[Array[String]] = s.split(",").map(_.split(" ").map(_.trim).filter(_.nonEmpty))

       // remove any valid prefixes from the front
       val (prefixName: PersonName, toks2: Array[Array[String]]) = stripPrefixes(toks)

       // remove any valid degrees and hereditySuffixes from the end
       val (degreesName: PersonName, coreToks: Array[Array[String]]) = stripSuffixes(toks)

       val coreName: PersonName =
         {
         if (coreToks.size == 0)
           {
           // no data
           new PersonName()
             {}
           }
         else if (coreToks.size == 1)
           {
           // no commas: Kermit Kalman the Frog Ph.D.

           parseUninvertedFullNoPrefix(coreToks.head)
           }
         else if (coreToks.size == 2)
           {
           // exactly one comma:
           // the Frog, Kermit Kalman
           new PersonNameWithDerivations
             {
             override val givenNames = coreToks(1).mkString(" ")
             override val surNames   = Set(coreToks(0).mkString(" "))
             }
           }
         else
           {
           throw new PersonNameParsingException("Multiple commas even after removing all degrees")
           }
         }

       // merge the prefix, degrees, and core data
       PersonName.merge(Seq(prefixName, degreesName, coreName))
       }

     private def parsePrefix(s: String): Option[String] =
       {
       // BAD
       // ** note we are case-sensitive here, but we ignore periods.  Solid caps will fail, eg. MR. KERMIT FROG.
       // the reason is that MR FRITZ could be Mary Roselda Fritz, bur MR. FRITZ could not.
       val q = s.replace(".", "")
       if (validPrefixes.contains(q)) Some(q) else None
       }

     /**
      * Remove any valid prefixes from the front and collect them into a PersonName.
      */
     private def stripPrefixes(ss: Array[Array[String]]): (PersonName, Array[Array[String]]) =
       {
       val p = ss.head.head
       val pp = parsePrefix(p)
       pp match
       {
         case None => (new PersonName()
           {}, ss)
         case Some(s: String) => (new PersonName()
           {prefix = s}, ss.head.tail +: ss.tail)
       }
       }

     private def parseFirstMiddle(ss: Array[String]): PersonName = new PersonNameWithDerivations
       {givenNames = ss.mkString(" ")}



     */
  /** ****************************************/
  /** **** CRUFT BELOW ***********************/
  /*
       val invertedNamePattern = """^([^,]*),([^,]*)$""".r

       /**
      * if there is exactly one comma in the name, reverse the order, e.g. "lastname, firstname" -> "firstname lastname".  In any other case just
      * return the
      * string as is.  Careful: the prefix, suffix, and degree may be confounded with the inversion, e.g. Dr. Soergel, David, Ph.D.
      *
      * @param s
      */
       def uninvertName(s: String): String =
         {
         assert(!s.isEmpty)
         if (s.count(_ == ',') > 1)
           {
           throw new PersonNameParsingException("Too many commas: " + s)
           }
         val q = try
         {
         val invertedNamePattern(lastname, firstname) = s
         if (lastname != null && firstname != null)
           {
           firstname.trim + " " + lastname.trim
           }
         else s
         }
         catch
         {
         case e: MatchError => s
         }
         val r = q.replace("  ", " ").trim
         assert(!r.isEmpty)
         r
         }*/
}

/*case class Person(firstNameInitial: Option[Char] = None, // used only for J. Harrison Ford
				  firstName: Option[String] = None, // preferred name goes here too, e.g. Harrison
				  middleName: Option[String] = None, // often just middle initial // combine as "givenNames"?
				  givenInitials: Option[Seq[Char]] = None, //
				  lastName: Option[String] = None, //
				  pedigree: Option[String] = None, //
				  degree: Option[String] = None, //
				  // droppingParticle: Option[String] = None, //
				  nonDroppingParticle: Option[String] = None, //
				  address: Option[Address] = None, //
				  email: Option[String] = None, //
				  phone: Option[String] = None, //
				  affiliations: Seq[Institution] = Nil, //
				  homepages: Seq[URL] = Nil) //
  {
  // val droppingParticles = Map(("de la" ->("de", "la")))  // ignore for now
  val particles = List("st", "de", "la", "de la", "du", "des", "del", "di", "van", "van den", "von", "zu", "der", "ter")  // "ben"?  "Van't"
  val authorSplit = "(\\S+ )?(.*? )?((" + particles.mkString("|") + ") )?(\\S+)".r
  def Person(x: String)
	{
	val authorSplit(f: String, m: String, p: String, l: String) = x
	val solidCaps = (x == x.toUpperCase)

	f.length match
	{
	  case 0 => // Prufrock
		{
		assert(m.isEmpty)
		}
	  case 1 if m.length > 1 => // J. Alfred X. Prufrock
		{
		firstName = Some(f); // we have only the initial, but call it the "name"
		middleName = Some(m);
		givenInitials = Some(f.head :: m.split(" .").map(_.head).toList)
		}
	  case 2 =>
		{
		if (solidCaps)
		  {
		  if (m.isEmpty) // JA PRUFROCK.  ED GREEN is interpreted as E.D. Green, not Ed Green.
			{
			givenInitials = Some(f.toCharArray.toSeq)
			}
		  else // AL J PRUFROCK interpreted as Al J. Prufrock
			{
			firstName = Some(f);
			middleName = Some(m);
			givenInitials = Some(f.head :: m.split(" .").map(_.head).toList)
			}
		  }
		}
	  case _ => firstName = Some(f);
	}
  }
*/
/*
object PersonNameUtils
	{

	/**
	 * Replace periods with spaces.
	 * if there is exactly one comma in the name, reverse the order, e.g. "lastname, firstname" -> "firstname lastname".  In any other case just return the
	 * string as is.
	 */
	/*	def cleanupNameNoPeriods(s: String): String =
	   {
	   assert(!s.isEmpty)
	   val r = cleanupName(s).replace(".", " ").replace("  ", " ").trim
	   assert(!r.isEmpty)
	   r
	   }

   /**
	* could two names conceivably refer to the same person?  Largely for use within a single WOS record, not for coref
	*/
   def compatibleName(oa: Option[PersonName], ob: Option[PersonName]): Boolean =
	   {
	   // the usual case is that the last names match, but the first name may be an initial
	   // but there may be additional stuff with prefixes, suffixes, middle names, etc. etc.
	   // or just initials
	   // we don't want to do coref here!  Just look for contradictions
	   // two names are compatible iff
	   // a) any string in one name longer than 3 chars is matched in the other string either exactly or by first initial
	   // b) any s

	   (oa, ob) match
	   {
		   case (Some(a), Some(b)) =>
			   {
			   compatibleName(a, b)
			   }
		   case default => false
	   }
	   }

   def compatibleName(a: PersonName, b: PersonName): Boolean =
	   {
	   val aToks = cleanupNameNoPeriods(a).toLowerCase.split(" ").reverse.map(_.trim).filterNot(_.isEmpty)
	   val bToks = cleanupNameNoPeriods(b).toLowerCase.split(" ").reverse.map(_.trim).filterNot(_.isEmpty)
	   if (aToks.isEmpty || bToks.isEmpty)
		   {
		   false
		   }
	   else
		   {
		   val try1 = compatibleTokens(aToks, bToks)

		   val try2 = if (try1) true
		   else
			   {
			   // special case: see if separating a two- or three-character name into initials helps
			   val aToks2: Array[String] = aToks
										   .flatMap(tok => (if (tok.length == 2 || tok.length == 3) tok.toCharArray.reverse.map(_.toString) else Some(tok)))
			   val bToks2: Array[String] = bToks.flatMap(tok => if (tok.length == 2 || tok.length == 3) tok.toCharArray.reverse.map(_.toString)
			   else Some(tok))

			   compatibleTokens(aToks2, bToks2)
			   }

		   try2
		   }
	   }

   private def compatibleTokens(aToks: Array[String], bToks: Array[String]): Boolean =
	   {
	   // basically a simple alignment.  don't bother with DP, just a couple heuristics for the common cases
	   // don't support suffixes; the last names must match, (allowing initials)
	   // suffixes after a comma may be OK due to uninvert
	   val headA = aToks.head
	   val headB = bToks.head
	   if (!((headA equals headB) || (headA equals headB(0).toString) || (headB equals headA(0).toString)))
		   {
		   false
		   }
	   else
		   {
		   // choose the minimum set of first & middle names & initials
		   if (aToks.length <= bToks.length)
			   {
			   compatibleFirstMiddle(aToks.tail, bToks.tail)
			   }
		   else
			   {
			   compatibleFirstMiddle(bToks.tail, aToks.tail)
			   }
		   }
	   }

   // remember tokens are reversed
   private def compatibleFirstMiddle(fewerToks: Array[String], moreToks: Array[String]): Boolean =
	   {

	   if (fewerToks.isEmpty || moreToks.isEmpty) true
	   else
		   {
		   val headX = fewerToks.head
		   val headY = moreToks.head
		   if (!((headX equals headY) || (headX equals headY(0).toString) || (headY equals headX(0).toString)))
			   {
			   // mismatch in first token; try to drop middle name/initial
			   // note this means that "A J Smith" and "J Smith" are compatible; oh well
			   if (fewerToks.length < moreToks.length)
				   compatibleFirstMiddle(fewerToks, moreToks.tail)
			   else false
			   }
		   else
			   {
			   //first initial equal; proceed
			   compatibleFirstMiddle(fewerToks.tail, moreToks.tail)
			   }
		   }
	   }
	   */
	}
*/
