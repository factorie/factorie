package cc.factorie.util

import scala.xml.factory.XMLLoader
import scala.xml.Elem
import javax.xml.parsers.{SAXParserFactory, SAXParser}

/**
 * Convenience class for loading in xml docs that specify
 * a dtd that is unavailable.
 *
 * @author John Sullivan
 */
object NonValidatingXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = SAXParserFactory.newInstance()
    f.setValidating(false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}
