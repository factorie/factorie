package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.strings.RegexSegmenter

/**
 * Author: martin
 * Date: 11/23/11
 */

// TODO: this still needs testing on text other than CoNLL, contractions, and probably simplification --brian
// Aims to adhere to CoNLL 2003 tokenization rules.
class Tokenizer extends RegexSegmenter(Seq(
  "'[tT]|'[lL]+|'[sS]|'[mM]|'re|'RE|'ve|'VE",
  "[A-Z]\\.O'[A-Za-z]+", // T.O'Gorman
  "[ONd]'[A-Za-z]+", // O'Brien, N'Djamena, d'affaires
  "[A-Za-z'\\-]+'[oa]", // al-Gama'a, Gama'a, Songo'o
  "[A-Za-z]\\.[a-z]+\\.+", // A.de
  "[0-9\\.]+-[0-9]+-[0-9]+-[0-9]+", // 7-0-29-0
  "[0-9]+[/\\-\\.\\:][0-9]+[/\\-\\.\\:][0-9\\-]+", // TODO: add more general date/time regex
  "[a-zA-Z0-9]+\\-[a-zA-Z0-9]+\\-[a-zA-Z0-9]+", // Chester-le-Street
  "[a-zA-Z0-9]+\\-[a-zA-Z0-9]+", // all-rounder
  "[0-9][0-9][sthnrd]+",
  "[a-zA-Z\\.0-9]+@[a-zA-Z0-9\\.]+\\.[a-z]+", // email
  "[A-Z]+\\$", // C$ and A$ show up in CoNLL 2003
  "[A-Z][a-z]+\\.", // Mr. Mrs. Calif.
  "\\.\"",
  "[0-9\\.,\\=\\-\\+/]+", // 1., +1, 1/2, =1
  "\\.+", // ...
  "[A-Z]+\\$", // C$ and A$ show up in CoNLL 2003
  """[`'",-:;$?&@]+""", // + is for em dash
  "\\(|\\)",
  "\\w+").mkString("|").r) {
  
  def process(documents: Seq[Document]): Unit = documents.map(d => process(d))
  def process(document: Document): Unit = {
    println(document.string)
    val tokenIterator = this.apply(document.string)
    while (tokenIterator.hasNext) {
      tokenIterator.next()
      new Token(document, tokenIterator.start, tokenIterator.end - tokenIterator.start)
    }
  }
}

object Tokenizer extends Tokenizer {
  def main(args: Array[String]): Unit = {
    val string = io.Source.fromFile(args(0)).getLines().mkString("\n")
    val doc = new Document("name", strValue = string)
    Tokenizer.process(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}