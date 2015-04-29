package cc.factorie.app.nlp.ner

import org.junit.{ Assert, Before, Test }
import Assert.assertEquals
import cc.factorie.app.nlp.Section
import cc.factorie.app.nlp.Document

class TestNerTag {

    @Test
    def testOntonotesNerSpanBufferFlatMap() = {
        val document: Document = new Document("test text").setName("test name")
        val section: Section = document.sections.head

        val ons1: OntonotesNerSpan = new OntonotesNerSpan(section, 0, 1, "ORG") //the values don't really matter
        val ons2: OntonotesNerSpan = new OntonotesNerSpan(section, 1, 2, "PERSON")
        val ons3: OntonotesNerSpan = new OntonotesNerSpan(section, 2, 3, "ORG")
        val it: Iterable[OntonotesNerSpan] = Array(ons1, ons2, ons3)
//      var onsb: OntonotesNerSpanBuffer = new OntonotesNerSpanBuffer(it)
        var onsb: OntonotesNerSpanBuffer = new OntonotesNerSpanBuffer ++= it
        assertEquals(onsb.size, 3)
    }

}