// package cc.factorie.app.nlp.event

// import org.scalatest.{FlatSpec, Matchers}
// import cc.factorie.app.nlp.{RequiresTrainedModel, Section, Document}
// import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicNormalizingTokenizer}
// import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
// import cc.factorie.app.nlp.ner._

// /**
//  * @author John Sullivan
//  */
// class TestPatternBasedEventFinder extends FlatSpec with Matchers {
//   def f = new {
//     //val text = "Barack Obama met Bill Clinton at the White House. He was extradited for treason for his Terrorist Fist Bump and workplace bullying."
//     val text = "John was sentenced and tried in Cuba. Barack Obama was elected."
//     val nerTags = "U-PERSON O O O O O U-GPE O B-PERSON L-PERSON O O O".split(" ")

//     val doc = new Document(text)

//   }

//   "PatternBasedEventFinder" should "generate candidates with gold NER" in {
//     val doc = f.doc
//     DeterministicNormalizingTokenizer.process(doc)
//     DeterministicSentenceSegmenter.process(doc)
//     val ner = new NerAnnotator[BBNEventNerSpan, BilouBBNEventNerTag] {
//       def prereqAttrs = Seq.empty[Class[_]]

//       def newSpan(sec: Section, start: Int, length: Int, category: String) = new BBNEventNerSpan(sec, start, length, category)

//       def newBuffer = new BBNEventNerSpanBuffer

//       def annotateTokens(document: Document) = {
//         document.tokens.zip(f.nerTags) foreach { case(token, tagValue) =>
//           token.attr += new BilouBBNEventNerTag(token, tagValue)
//         }
//         document
//       }
//     }
//     ner.process(doc)
//     BBNEventPatternBasedEventFinder.process(doc)
//     val pats = doc.attr[MatchedEventPatterns]
//     val recoveredRoles = pats.map(pat => pat.span.string -> pat.pattern.eventRole.event).toSet
//     val expectedRoles =
//       Set(("John", "Justice.Sentence"),
//           ("Cuba", "Justice.Trial-Hearing"),
//           ("Barack Obama", "Personnel.Elect"))
//     assert(pats != null && pats.nonEmpty)

//     assert(expectedRoles.forall(recoveredRoles.contains))
//   }

//   it should "generate candidates with non-gold NER" taggedAs RequiresTrainedModel in {
//     val doc = f.doc
//     EventNLPComponents.process(doc)

//     val pats = doc.attr[MatchedEventPatterns]
//     val recoveredRoles = pats.map(pat => pat.span.string -> pat.pattern.eventRole.event).toSet
//     val expectedRoles =
//       Set(("John", "Justice.Sentence"),
//         ("Cuba", "Justice.Trial-Hearing"),
//         ("Barack Obama", "Personnel.Elect"))
//     assert(pats != null && pats.nonEmpty)

//     assert(expectedRoles.forall(recoveredRoles.contains))
//   }

//   /*
//   "EventNLPComponents" should "recall patterns" in {
//     EventNLPComponents.process(f.doc)
//     val res = f.doc.attr[MatchedEventPatterns]
//     assert(res != null && res.nonEmpty)
//   }
//   */
// }
