package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{Token, Document}
import cc.factorie.app.strings.RegexSegmenter

/**
 * Author: martin
 * Date: 11/23/11
 */

// TODO: this still needs testing on more text, contractions, and probably simplification --brian
// Aims to adhere to CoNLL 2003 tokenization rules.
class Tokenizer extends RegexSegmenter(Seq(
  "'[tT]|'[lL]+|'[sS]|'[mM]|'re|'RE|'ve|'VE", // this isn't working as expected
  "[\\p{L}]\\.[\\p{L}\\.]*", // A.de, A.A.A.I, etc.
  "[0-9]{1,2}[sthnrd]+[\\-\\p{L}]+", // the second part is for 20th-century
  "[0-9]{4}", // match years before other numbers
  "[0-9\\-.\\:/,\\+\\=]+[0-9\\-:/,\\+\\=]", // is there a better way to say, "doesn't end in '.'"?
  "[\\p{L}\\p{Nd}.]+@[\\p{L}\\{Nd}\\.]+\\.[a-z]{2,4}", // email
  "[A-Z][a-z]{0,4}\\.", // Mr. Mrs. Calif. but not Institute.
  "[.?!][\\p{Pf}\\p{Pe}]?", // ending/final punctuation
  "[\\p{Pf}\\p{Pe}]?[.?!]", // ending/final punctuation followed by [.?!]
  "[`'\"]+", // mid-sentence quotes
  """[,-:;$?&@\(\)]+""", // other symbols
  "[\\w\\-0-9']+"  // any combo of word-chars, numbers, and hyphens
  ).mkString("|").r) {
  
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
    val doc = new Document("", strValue = string)
    Tokenizer.process(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}