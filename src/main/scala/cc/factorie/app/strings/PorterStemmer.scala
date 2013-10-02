package cc.factorie.app.strings

/**
 * Rewritten from http://tartarus.org/martin/PorterStemmer/scala.txt
 * for thread-safety and style (but definitely not too pretty yet).
 * @author Brian Martin
 */
object PorterStemmer {
  val vowels = "aeiou"
  val step1aVals = List(("sses", "ss"), ("ies","i"), ("ss","ss"), ("s", ""))
  val step1bVals = List(("at", "ate"), ("bl","ble"), ("iz","ize"))
  val step2Vals = List(("ational", "ate"),("tional","tion"),("enci","ence"),("anci","ance"),("izer","ize"),("bli","ble"),("alli", "al"), ("entli","ent"),("eli","e"),("ousli","ous"),("ization","ize"),("ation","ate"),("ator","ate"),("alism","al"), ("iveness","ive"),("fulness","ful"),("ousness", "ous"),("aliti", "al"),("iviti","ive"),("biliti", "ble"),("logi", "log"))
  val step3Vals = List(("icate", "ic"),("ative",""),("alize","al"),("iciti","ic"),("ical","ic"),("ful",""),("ness",""))
  val step4aVals = List(("al",""),("ance",""),("ence",""),("er",""),("ic",""),("able",""),("ible",""),("ant",""),("ement",""), ("ment",""),("ent",""))
  val step4bVals = List(("ou",""),("ism",""),("ate",""),("iti",""),("ous",""),("ive",""),("ize",""))

  def applySteps(_b: String): String = {
    if (_b.size <= 2) return _b

    var b = _b

    def isConsonant(i: Int): Boolean = {
      if (b(i) == 'y') {
        if (i == 0) true
        else !isConsonant(i-1)
      }
      else !vowels.contains(b(i))
    }

    /* m() measures the number of consonant sequences between 0 and j. if c is
        a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
        presence,

           <c><v>       gives 0
           <c>vc<v>     gives 1
           <c>vcvc<v>   gives 2
           <c>vcvcvc<v> gives 3
           ....
     */
    def calcM(s:String): Int = {
      if (b.length == 0) return 0
      var count = 0
      (1 until s.length).foldLeft(isConsonant(0)) {
        case (lastIsC, c) =>
          val isC = isConsonant(c)
          if (isC && !lastIsC) count += 1
          isC
      }
      count
    }

    /* removing the suffix string, s, does a vowel exist?' */
    def vowelInStem(s: String): Boolean = (0 until b.length - s.length).exists(!isConsonant(_))

    /* doublec(j) is true <=> j,(j-1) contain a double consonant. */
    def doublec(): Boolean = {
      var l = b.length - 1
      l >= 1 && b(l) == b(l-1) && isConsonant(l)
    }

    /* cvc(i) is true <=> i-2,i-1,i has the form consonant - vowel - consonant
       and also if the second c is not w,x or y. this is used when trying to
       restore an e at the end of a short word. e.g.
          cav(e), lov(e), hop(e), crim(e), but
          snow, box, tray.
    */
    def cvc(s:String): Boolean = {
      val i = b.length - 1 - s.length
      if (i < 2 || !isConsonant(i) || isConsonant(i-1) || !isConsonant(i-2)) false
      else if ("wxy".contains(b(i))) false
      else true
    }

    def replacer(orig: String, replace:String, checker: Int => Boolean): Boolean = {
      if (b.endsWith(orig)) {
        var n = b.dropRight(orig.length)
        if (checker(calcM(n)))
          b = n + replace
        true
      }
      else false
    }

    def processSubList( l:List[(String, String)], checker: Int=>Boolean ): Boolean =
      l.exists(v => replacer(v._1, v._2, checker))

    // step 1a
    processSubList(step1aVals, _ >= 0)

    // step 1b
    if (!replacer("eed", "ee", _ > 0) &&
        ((vowelInStem("ed") && replacer("ed", "", _>=0) ) || ( vowelInStem("ing") && replacer( "ing", "", _>=0)  ) ) &&
        (! processSubList(step1bVals, _>=0))) {
      if ( doublec() && !"lsz".contains( b.last ) )
        b = b.substring( 0, b.length - 1 )
      else if (calcM(b) == 1 && cvc(""))
        b = b + "e"
    }

    // step 1c
    vowelInStem("y") && replacer("y", "i", _ >= 0)

    // step 2
    processSubList(step2Vals, _>0 )

    // step 3
    processSubList(step3Vals, _>0)

    // step 4
    var res = processSubList(step4aVals, _>1)
    if (!res && b.length > 4 && (b(b.length-4) == 's' || b(b.length-4) == 't'))
      res = replacer("ion", "", _>1)
    if (!res) processSubList(step4bVals, _>1)

    // step 5a
    replacer("e", "", _>1)
    if ( !cvc("e") )
      replacer("e", "", _==1)

    // step 5b
    if (calcM(b) > 1 && doublec() && b.last == 'l') b = b.dropRight(1)

    b
  }

  def apply(s:String): String = applySteps(s)

  def main(args: Array[String]): Unit = {
    def getOWPL(f: String) = io.Source.fromFile(f).getLines().toSeq.map(_.trim)

    if (args.length != 2)
      println("Expected arguments are a OWPL file of unstemmed and a OWPL file of properly stemmed words to check against.\n" +
              "These are available at http://tartarus.org/martin/PorterStemmer/")

    val unstemmed = getOWPL(args(0))
    val trueStemmed = getOWPL(args(1))

    println("unstemmed.size: " + unstemmed.size)
    println("trueStemmed.size: " + trueStemmed.size)

    val stemmed = unstemmed.map(apply)

    println("stemmed.size: " + stemmed.size)

    stemmed.zip(trueStemmed).filter(s => s._1 != s._2).foreach(println(_))
    stemmed.zip(trueStemmed).foreach(s => assert(s._1 == s._2, s._1 + " " + s._2))
    stemmed.zip(trueStemmed).take(20).foreach(s => println("sample: " + s._1 + " " + s._2))
  }
}
