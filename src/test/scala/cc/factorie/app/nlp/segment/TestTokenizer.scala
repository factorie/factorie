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

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie.app.nlp.{DocumentAnnotatorPipeline, DocumentAnnotator, Sentence, Document, Token}
import cc.factorie.util.FastLogging

class TestTokenizer extends JUnitSuite with FastLogging {
  
  def assertEquals(a: Any, b: Any): Unit = assert(a == b, "\"%s\" did not equal \"%s\"" format (a, b))

  @Test def testSentenceSegmenter(): Unit = {
    val text =
      """
      I got bored of the iPhone. This has been a conundrum for for the past few months. I wanted a new phone, but didn't know how to transcend away from the iOS environment. I woke up one morning, and said, "whatever, I don't care anymore," and walked into BestBuy(save the hate, I had a $25 coupon and $35 gift card) and bought the Note 2. I have been wanting this phone since I first hear about it.

      Why I made this decision. I love tech stuff, and the iphone, to me, was getting boring. My early upgrade just came through, so I could test a new phone, and still be within the launch period of the next gen iPhone. Having gone from the iPhone 3G, to Iphone4, to the 4S, you would think I would be drawn to the iPhone5 right? No. It did nothing for me. I don't even know how to explain it.

      These are some things that worried me about switching. I have a Mac, and though I use Windows7 on BC, I still want my phone to sync natively to my Mac. The worry that syncing wouldn't be as smooth as the iPhone had me. I don't think there's another phone in existence that syncs info as well as the iPhone on an Mac. I had gotten used to easily syncing EVERYTHING in one go with Itunes and iPhoto, but I decided to just go with it and use it as an experience.

      Now that I actually own the Note 2, and more specifically an Android phone, I actually have a better understanding of the OS quality provided by Apple.
      However, with that said, the Android 4.1 is awesome. Better than anything to come before it from Android(obviously, right?). This phone is an absolute MONSTER!

      I now use my iphone as an alarm clock and is the bluetooth source to play music in my car.
      """.stripMargin
    val d = new Document((1 to 2).map(_ => text).mkString("\n"))
    DocumentAnnotatorPipeline(DeterministicTokenizer, DeterministicSentenceSegmenter).process(d)
    assert(d.sentences.size > 0)
    d.sentences.map(_.string).foreach(s => logger.debug(s.toString))
  }

  @Test def testSentenceSegmenterWithOneSentence() {
    val text = "The quick brown fox jumps over the lazy dog."
    val d = new Document(text)
    DocumentAnnotatorPipeline(DeterministicTokenizer, DeterministicSentenceSegmenter).process(d)
    assert(d.sentences.size == 1)
    assert(d.tokens.size == 10)
  }

  private def runDeterministicTokenizer(src: String): Iterable[Token] = {
    val d = new Document(src)
    DocumentAnnotatorPipeline(DeterministicTokenizer).process(d).tokens
  }

  @Test def testDeterministicTokenizer(): Unit = {
    def check(src: String, trg: String): Unit = {
      val tokens = runDeterministicTokenizer(src)
      for (t <- tokens) {
        assertEquals(t.string, src.substring(t.stringStart, t.stringEnd))
      }
      assertEquals("[" + tokens.map(_.string).mkString(", ") + "]", trg)
    }

    // spaces
    check(
      src = "a b  c\n d \t\n\r\fe",
      trg = "[a, b, c, d, e]")

    // emoticons
    check(
      src = ":-)))) :----) :( :-) :--)",
      trg = "[:-)))), :----), :(, :-), :--)]")
    
    // URLs
    check(
      src = "|http://www.google.com|www.google.com|mailto:somebody@google.com|some-body@google+.com|",
      trg = "[|, http://www.google.com, |, www.google.com, |, mailto:somebody@google.com, |, some-body@google+.com, |]")
    
    check(
      src = "google.com index.html a.b.htm ab-cd.shtml",
      trg = "[google.com, index.html, a.b.htm, ab-cd.shtml]")

    // abbreviations
    check(
      src = "prof. ph.d. a. a.b. a.b a.b.c. men.cd ab.cd",
      trg = "[prof., ph.d., a., a.b., a.b, a.b.c., men, ., cd, ab, ., cd]")

    // consecutive punctuation
    check(
      src = "A..B!!C??D.!?E.!?.!?F..!!??",
      trg = "[A, .., B, !!, C, ??, D, .!?, E, .!?.!?, F, ..!!??]")

    check(
      src = ",,A---C*D**E~~~~F==",
      trg = "[,,, A, ---, C, *, D, **, E, ~~~~, F, ==]")
      // was: trg = "[,,, A, ---, C*D, **, E, ~~~~, F, ==]")

    // dots in numbers
    // Really?  Do we want this? -akm
//    check(
//      src = ".1 a.1 2.3 4,5 6:7 8-9 0/1 '2 3's 3'4 5'b a'6 a'b",
//      trg = "[.1, a.1, 2.3, 4,5, 6:7, 8-9, 0/1, '2, 3's, 3'4, 5'b, a'6, a'b]")

//    check(
//      src = ".a a.3 4,a a:a a8-9 0/1a",
//      trg = "[., a, a.3, 4, ,, a, a, :, a, a8-9, 0/1a]")
    
    // hyphens
    check(
      src = "dis-able cross-validation o-kay art-o-torium s-e-e art-work",
      trg = "[dis-able, cross-validation, o-kay, art-o-torium, s-e-e, art, -, work]")
    
    // apostrophies
    check(
      src = "he's we'd I'm you'll they're I've didn't did'nt",
      trg = "[he, 's, we, 'd, I, 'm, you, 'll, they, 're, I, 've, did, n't, did, 'nt]")
    
    check(
      src = "he'S DON'T gue'ss",
      trg = "[he, 'S, DO, N'T, gue, ', ss]")
      // Was: trg = "[he, 'S, DO, N'T, gue'ss]")
    
      // Really?  Do we want this? -akm
//    check(
//      src = "aint cannot don'cha d'ye i'mma dunno",
//      trg = "[ai, nt, can, not, do, n', cha, d', ye, i, 'm, ma, du, n, no]")

    check(
      src = "$1 E2 L3 USD1 2KPW ||$1 USD1..",
      trg = "[$, 1, E2, L3, USD, 1, 2, KPW, |, |, $, 1, USD, 1, ..]")
    
    check(
      src = "1m 2mm 3kg 4oz",
      trg = "[1, m, 2, mm, 3, kg, 4, oz]")
    
    check(
      src = "1D 2nM 3CM 4LB",
      trg = "[1, D, 2, nM, 3, CM, 4, LB]")
    
    check(
      src = "(1){2}[3]<4>",
      trg = "[(, 1, ), {, 2, }, [, 3, ], <, 4, >]")
    
    // Really?  Do we want this? -akm
//    check(
//      src = "`a'b,c:d;e-f/g\"h'",
//      trg = "[`, a'b, ,, c, :, d, ;, e, -, f, /, g, \", h, ']")
//    check(
//      src = "`a'b,c:d;e-f/g\"h'",
//      trg = "[`, a'b, ,, c, :, d, ;, e, -, f, /, g, \", h, ']")
    
    check(
      src = "a@b #c$d%e&f|g",
      trg = "[a@b, #, c$, d, %, e, &, f, |, g]")
      // Was: trg = "[a@b, #, c, $, d, %, e, &, f, |, g]")
    
    check(
      src = "e.g., i.e, (e.g.,",
      trg = "[e.g., ,, i.e, ,, (, e.g., ,]")
    
    check(
      src = " \n \t",
      trg = "[]")

    check(    
      src = "\"John & Mary's dog,\" Jane thought (to herself).\n" + "\"What a #$%!\n" + "a- ``I like AT&T''.\"",
      trg = "[\", John, &, Mary, 's, dog, ,, \", Jane, thought, (, to, herself, ), ., \", What, a, #, $, %, !, a, -, ``, I, like, AT&T, '', ., \"]")
    
    check(
      src = "I said at 4:45pm.  Never 2am.",
      trg = "[I, said, at, 4:45, pm, ., Never, 2, am, .]")
    
    check(
      src = "I can't believe they wanna keep 40% of that.\"``Whatcha think?''\"I don't --- think so...,\"",
      trg = "[I, ca, n't, believe, they, wan, na, keep, 40, %, of, that, ., \", ``, What, cha, think, ?, '', \", I, do, n't, ---, think, so, ..., ,, \"]")
    
    check(
      src = "You `paid' US$170,000?!\nYou should've paid only$16.75.",
      trg = "[You, `, paid, ', US$, 170,000, ?!, You, should, 've, paid, only, $, 16.75, .]")
    
    check(
      src = " 1. Buy a new Chevrolet (37%-owned in the U.S..) . 15%",
      trg = "[1, ., Buy, a, new, Chevrolet, (, 37, %, -, owned, in, the, U.S., ., ), ., 15, %]")
  }

  val testText2 =
    """
      The problem with MacroTypeTag is that it can be used outside macros.

      A fact about FullTypeTag that I don't like is that it implies that
      it's somehow more full-fledged than TypeTag.

      What about ArbTypeTag (from arbitrary)? I agree the name is cryptic,
      but at least it's not misleading and it doesn't imply that this type
      tag carries more information that a vanilla TypeTag.
    """.stripMargin

  @Test def testRegexes(): Unit = {

    val reg = new PunktSentenceSegmenter.Punkt.PunktLanguageVars()
    val allMatches = reg.wordTokenizerRegex.findAllIn(testText2).toSeq
    allMatches.foreach(s => logger.debug(s.toString))

    assert(allMatches == Seq("The", "problem", "with", "MacroTypeTag", "is", "that", "it", "can", "be", "used",
       "outside", "macros.", "A", "fact", "about", "FullTypeTag", "that", "I", "don", "'t", "like", "is", "that", "it", "implies",
       "that", "it", "'s", "somehow", "more", "full-fledged", "than", "TypeTag.", "What", "about", "ArbTypeTag", "(", "from",
       "arbitrary", ")", "?", "I", "agree", "the", "name", "is", "cryptic", ",", "but", "at", "least", "it", "'s", "not",
       "misleading", "and", "it", "doesn", "'t", "imply", "that", "this", "type", "tag", "carries", "more", "information",
       "that", "a", "vanilla", "TypeTag."))

  }

  @Test def allTests(): Unit = {

    val testText1 =
      """
        Something *less* subtle differentiates them. "f" is a method in *both*
        cases. In the first case, it's a method with one parameter list of
        arity 1 taking an Int, and returning an Int. I'll try to use the word U.S.A. In the second case, f is
        a nullary method returning an Int => Int.

        Now, Int => Int, to be clear about it, is the same thing as Function1[Int, Int].

        Methods are not values, functions are.
      """.stripMargin

//    val tokenized1 = getTokenizedSentences(testText1)
//    printTokenizedSentences(tokenized1)

//    val tokenized2 = getTokenizedSentences(testText2)
//    printTokenizedSentences(tokenized2)

    val jointTokenized = getTokenizedSentences(Seq(testText1, testText2))

    jointTokenized.foreach(printTokenizedSentences(_))

    assert(jointTokenized(0).length == 7, jointTokenized(0).length)
    assert(jointTokenized(1).length == 4, jointTokenized(1).length)

//    val noInference = getTokenizedSentences(Seq(testText1, testText2), Non)
//    noInference.foreach(printTokenizedSentences(_))

    val text = """
    Punkt knows that the periods in Mr. Smith and Johann S. Bach
    do not mark sentence boundaries.  And sometimes sentences
    can start with non-capitalized words.  i is a good variable
    name.
               """

    val sampleTokenized = getTokenizedSentences(Seq(text))
    assert(sampleTokenized(0).length == 3, sampleTokenized(0).length)

    val moreText =
      """
        President F.W. de Klerk released the ANC men -- along with one of the founding members of the Pan Africanist Congress, a rival liberation group --
        as part of his efforts to create a climate of trust and peace in which his government can begin negotiations with black leaders over a new constitution
        aimed at giving blacks a voice in national government.  But Pretoria may instead be creating a climate for more turmoil and uncertainty in this
        racially divided country.  As other repressive governments, particularly Poland and the Soviet Union, have recently discovered, initial steps to open
        up society can create a momentum for radical change that becomes difficult, if not impossible, to control.  As the days go by, the South African
        government will be ever more hard pressed to justify the continued imprisonment of Mr. Mandela as well as the continued banning of the ANC and
        enforcement of the state of emergency.  If it does n't yield on these matters, and eventually begin talking directly to the ANC, the expectations
        and promise raised by yesterday 's releases will turn to disillusionment and unrest.  If it does, the large number of right-wing whites, who
        oppose any concessions to the black majority, will step up their agitation and threats to take matters into their own hands.  The newly released ANC
        leaders also will be under enormous pressure.  The government is watching closely to see if their presence in the townships leads to increased anti-government
        protests and violence; if it does, Pretoria will use this as a reason to keep Mr. Mandela behind bars.  Pretoria has n't forgotten why they were all
        sentenced to life imprisonment in the first place: for sabotage and conspiracy to overthrow the government.  In addition, the government is figuring
        that the releases could create a split between the internal and external wings of the ANC and between the newly freed leaders and those activists
        who have emerged as leaders inside the country during their imprisonment.  In order to head off any divisions, Mr. Mandela, in a meeting with
        his colleagues before they were released, instructed them to report to the ANC headquarters in Lusaka as soon as possible.  The men also will be faced
        with bridging the generation gap between themselves and the country 's many militant black youths, the so-called young lions who are anxious to see
        the old lions in action.  Says Peter Mokaba, president of the South African Youth Congress: `` `` We will be expecting them to act like leaders
        of the ANC. ''  They never considered themselves to be anything else.  At last night 's rally, they called on their followers to be firm,
        yet disciplined, in their opposition to apartheid.  `` `` We emphasize discipline because we know that the government is very, very sensitive, '' said
        Andrew Mlangeni, another early Umkhonto leader who is now 63.  `` `` We want to see Nelson Mandela and all our comrades out of prison, and if we are n't
        disciplined we may not see them here with us.
      """

    val moreTokenized = getTokenizedSentences(Seq(moreText))
    printTokenizedSentences(moreTokenized(0))
    assert(moreTokenized(0).length == 17, moreTokenized(0).length)
  }

  def printTokenizedSentences(sentences: Seq[Sentence]): Unit = sentences.foreach(sen => logger.debug(sen.tokens.map(t =>   t.string)))

  def getTokenizedSentences(text: Seq[String], inference: SentenceBoundaryInference = JointlyAcrossDocuments): Seq[Seq[Sentence]] = {
    val docs = text.map(t => new Document(t))
    new PunktTokenizer { override def sentenceBoundaryInference = inference }.process(docs)
    docs.map(_.sentences.toSeq)
  }

  @Test def testDeterministicTokenizerUnclosedLine(): Unit = {
    val unclosedLine = "<" + ("x" * 1679)
    val tokens = runDeterministicTokenizer(unclosedLine)
  }

  @Test def testDeterministicTokenizerLongLine(): Unit = {
    val hugeLine = """<meta content="Read Featured Article “YesAsia 2011 Bestsellers - Movies” by YumCha! Editorial Team mentioning Donnie Yen,Shawn Yue,Jung Woo Sung,Michelle Yeoh,Jiang Wen,Andy Lau,Kaneshiro Takeshi,Yamazaki Takashi,Benny Chan,Won Bin,Kim Jee Woon,Kimura Takuya,Tsui Hark,Su Chao Pin,Barbie Hsu,Wang Xue Qi,John Woo,Peter Chan, &quot;Villain (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Let The Bullets Fly (2010) (Blu-ray + DVD) (Hong Kong Version)&quot;,&quot;Villain (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Borrower Arrietty (DVD) (English Subtitled) (2-Disc Edition) (Hong Kong Version)&quot;,&quot;Shaolin (2011) (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;A Better Tomorrow (2010) (DVD) (First Press Limited Edition) (Korea Version)&quot;,&quot;Outrage (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Housemaid (2010) (DVD) (Single Disc) (Korea Version)&quot;,&quot;Bruce Lee My Brother (Blu-ray) (Hong Kong Version)&quot;,&quot;Space Battleship Yamato (DVD) (Collector&#039;s Edition) (First Press Limited Edition) (Japan Version)&quot;,&quot;I Saw the Devil (Blu-ray) (2-Disc) (Normal Edition) (Korea Version)&quot;,&quot;Natalie (DVD) (Hong Kong Version)&quot;,&quot;The Lady Shogun And Her Men (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Wu Xia (2011) (DVD) (Hong Kong Version)&quot;,&quot;The Borrower Arrietty (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;Whisper Of The Heart (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Housemaid (2010) (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Laputa: Castle in The Sky (Blu-ray) (Hong Kong Version)&quot;,&quot;A Better Tomorrow (2010) (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Borrower Arrietty (Blu-ray) (Multi Audio &amp; Subtitled) (Region Free) (Japan Version)&quot;,&quot;Natalie (Blu-ray) (2D+3D Version) (Hong Kong Version)&quot;,&quot;The Lost Bladesman (DVD) (Hong Kong Version)&quot;,&quot;Haunters (Blu-ray + OST) (First Press Limited Edition) (Korea Version)&quot;,&quot;Cyrano Agency (DVD) (Single Disc) (Korea Version) &quot;,&quot;The Man From Nowhere (Blu-ray) (Normal Edition) (Korea Version)&quot;,&quot;Outrage (Blu-ray) (English Subtitled) (Japan Version)&quot;,&quot;Natalie 3D (DVD) (2-Disc) (First Press Limited Edition) (Korea Version)&quot;,&quot;Laputa: Castle in the Sky (Blu-ray) (Multi Audio &amp; Subtitled) (Region Free) (Japan Version)&quot;,&quot;Sex &amp; Zen: Extreme Ecstasy (Blu-ray) (2D + 3D Director&#039;s Cut) (Hong Kong Version)&quot;,&quot;A Moment to Remember (Blu-ray) (Director&#039;s Cut) (First Press Limited Edition) (Korea Version)&quot;,&quot;The Man From Nowhere (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;A Bittersweet Life (Blu-ray) (Director&#039;s Cut) (Normal Edition) (First Press Limited Edition) (Korea Version)&quot;,&quot;The Lost Bladesman (Blu-ray) (Hong Kong Version)&quot;,&quot;Whisper Of The Heart (Blu-ray) (Multi Audio &amp; Subtitled) (Region Free) (Japan Version)&quot;,&quot;Wu Xia (2011) (Blu-ray) (Hong Kong Version)&quot;,&quot;Outrage (2-DVD Special Edition) (English Subtitled) (Hong Kong Version)&quot;,&quot;Reign Of Assassins (Blu-ray) (Hong Kong Version)&quot;,&quot;The Sorcerer And The White Snake (2011) (Blu-ray) (Hong Kong Version)&quot;,&quot;Space Battleship Yamato (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Man From Nowhere (DVD) (2-Disc) (Normal Edition) (Korea Version)&quot;,&quot;Norwegian Wood (2010) (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;What Women Want (2011) (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;A Better Tomorrow (2010) (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;Secret Reunion (DVD) (Single Disc) (Korea Version)&quot;,&quot;Outrage (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Sex &amp; Zen: Extreme Ecstasy (DVD) (2D+3D Special Version) (Hong Kong Version)&quot;,&quot;The Borrower Arrietty (DVD) (English Subtitled) (Japan Version)&quot;,&quot;Detective Dee And The Mystery Of The Phantom Flame (Blu-ray) (Hong Kong Version)&quot;,&quot;Reign Of Assassins (DVD) (Hong Kong Version)&quot;,&quot;Detective Dee And The Mystery Of The Phantom Flame (DVD) (Hong Kong Version)&quot;,&quot;Secret Reunion (Blu-ray) (Limited Edition) (Korea Version)&quot;,&quot;Confessions (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;Confessions (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Tomorrow&#039;s Joe (Blu-ray) (Premium Edition) (Japan Version)&quot;,&quot;Tomorrow&#039;s Joe (DVD) (English Subtitled) (Taiwan Version)&quot;,&quot;Bruce Lee My Brother (DVD) (Hong Kong Version)&quot;,&quot;Haunters (DVD) (Single Disc) (Korea Version)&quot;,&quot;Shaolin (2011) (DVD) (2-Disc Edition) (Hong Kong Version)&quot;,&quot;The Lady Shogun and Her Men (DVD) (Deluxe Edition) (First Press Limited Edition) (Japan Version)&quot;,&quot;The Housemaid (2010) (Blu-ray) (First Press Limited Edition) (Korea Version)&quot;,&quot;Space Battleship Yamato (DVD) (2-Disc Edition) (English Subtitled) (Hong Kong Version)&quot;,&quot;Sex &amp; Zen: Extreme Ecstasy (DVD) (2D Director&#039;s Cut) (Hong Kong Version)&quot;,&quot;Norwegian Wood (2010) (DVD) (English Subtitled) (Hong Kong Version)&quot;,&quot;Tomorrow&#039;s Joe (DVD) (Standard Edition) (Japan Version)&quot;,&quot;The Lady Shogun And Her Men (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;What Women Want (2011) (Blu-ray) (English Subtitled) (Hong Kong Version)&quot;,&quot;The Sorcerer And The White Snake (2011) (DVD) (Hong Kong Version)&quot; and more customer and professional reviews, editors’ picks, and the latest in Asian Entertainment at YumCha! from YesAsia.com. - North America Site" name="description"/>^M"""
    val tokens = runDeterministicTokenizer(hugeLine)
  }
  
  @Test def testRegexTokenizer(): Unit = {
    assert(DeterministicTokenizer("Washington D.C.").toSeq == Seq("Washington", "D.C."))
    assert(DeterministicTokenizer("Acme Inc.").toSeq == Seq("Acme", "Inc."))
    assert(DeterministicTokenizer("Oct. 24").toSeq == Seq("Oct.", "24"))
    assert(DeterministicTokenizer("Mr. Smith.").toSeq == Seq("Mr.", "Smith", "."))
    //println(RegexTokenizer("MR. SMITH.").mkString(" "))
    //assert(RegexTokenizer("MR. SMITH.").toSeq == Seq("MR.", "SMITH", ".")) // TODO It would be nice if this worked.
    //assert(RegexTokenizer("mr. smith.").toSeq != Seq("mr.", "smith", ".")) // TODO Should this work? -akm
  }
  
}
