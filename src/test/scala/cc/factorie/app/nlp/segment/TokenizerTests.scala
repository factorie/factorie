package cc.factorie.app.nlp.segment

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import cc.factorie.app.nlp.{Sentence, Document}

class TokenizerTests extends JUnitSuite {

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
    allMatches.foreach(println(_))

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

  def printTokenizedSentences(sentences: Seq[Sentence]): Unit = sentences.foreach(sen => println(sen.tokens.map(t =>   t.string)))

  def getTokenizedSentences(text: Seq[String], inference: SentenceBoundaryInference = JointlyAcrossDocuments): Seq[Seq[Sentence]] = {
    val docs = text.map(t => new Document("", t))
    new PunktTokenizer { override def sentenceBoundaryInference = inference }.process(docs)
    docs.map(_.sentences)
  }
}
