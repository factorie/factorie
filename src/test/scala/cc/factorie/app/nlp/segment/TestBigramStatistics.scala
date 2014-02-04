package cc.factorie.app.nlp.segment
import org.junit.Test

/**
 * User: apassos
 * Date: 8/19/13
 * Time: 2:24 PM
 */
class TestBigramStatistics {
  @Test def testBigramStatistics() {
    val gpl = new cc.factorie.app.nlp.Document(cc.factorie.tutorial.WordSegmenter.data.mkString("\n"))
    DeterministicTokenizer.process(gpl)
    val bg = new BigramStatistics
    bg.process(gpl)
    val phrases = bg.getLikelyPhrases(5, 40)
    assert(phrases.exists(p => p(0) == "free" && p.length > 1 && p(1) == "software"))
    assert(!phrases.exists(p => p(0) == "you" && p.length > 1 && p(1) == "may"))
  }
}
