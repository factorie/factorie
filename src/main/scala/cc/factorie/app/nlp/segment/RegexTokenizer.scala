package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp.{Implicits, DocumentAnnotator, Token, Document}
import cc.factorie.app.strings.RegexSegmenter

// TODO: this still needs testing on more text, contractions, and probably simplification --brian

// TODO: Gather abbreviations in separate Collection[String], and remove "may\\." from the collection. 

// TODO: Rename to RegexTokenizer

/** Split a String into Tokens.  Aims to adhere to CoNLL 2003 tokenization rules.
    Punctuation that ends a sentence should be placed alone in its own Token, hence this segmentation implicitly defines sentence segmentation also.
    @author martin 
    */
class RegexTokenizer extends RegexSegmenter(RegexTokenizerHelper.tokenRegex) with DocumentAnnotator {

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.string + "\t"

  def process1(document: Document): Document = {
    for (section <- document.sections) {
      val tokenIterator = RegexTokenizer.this.apply(section.string)
      while (tokenIterator.hasNext) {
        tokenIterator.next()
        val t = new Token(section, section.stringStart + tokenIterator.start, section.stringStart + tokenIterator.end)
//        // No, don't do this.  Look for \n\n in Document.String in Sentence segmenter
//        if (false && document.stringLength > t.stringEnd +2 && document.string(t.stringEnd) == '\n' && document.string(t.stringEnd+1) == '\n') {
//          // Two newlines in a row, without ending punctuation; add an extra "." to help Sentence segmentation 
//          val t2 = new Token(section, t.stringEnd, t.stringEnd)
//          t2.attr += new cc.factorie.app.nlp.TokenString(t2, ".")
//        }
      }
    }
    document
  }
  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs: Iterable[Class[_]] = List(classOf[Token])
}

object RegexTokenizer extends RegexTokenizer {
  def main(args: Array[String]): Unit = {
    val string = io.Source.fromFile(args(0)).mkString
    val doc = new Document(string)
    RegexTokenizer.process1(doc)
    println(doc.tokens.map(_.string).mkString("\n"))
  }
}
  
object RegexTokenizerHelper { 
  
  
  val month = """Jan
Feb
Mar
Apr
Jun
Jul
Aug
Sep
Sept
Oct
Nov
Dec"""
    
  val state = """Ala
Alab
Ariz
Ark
Calif
Colo
Conn
Del
Fla
Ill
Ind
Kans
Kan
Ken
Kent
Mass
Mich
Minn
Miss
Mont
Nebr
Neb
Nev
Dak
Okla
Oreg
Tenn
Tex
Virg
Wash
Wis
Wyo"""
    
  val honorific = """
Capt
Col
Com
Comdr
Cpl
Cpt
Dr
Hon
Gen
Gov
Lt
Mr
Mrs
Ms
Pfc
Pvt
Rev
Sen
Sgt"""
    
  val place = """Ave
St
Str
Ln"""
  val org = """Inc
Corp
Ltd
Sci
Comm
Inst
Lib
Mus
Ser
Alt
Comput"""
    
  val abbrev = """etc
vol
rev
dea
est
gal"""
    
    
  
  
  val contractions = "'[tT]|'[lL]+|'[sS]|'[mM]|'re|'RE|'ve|'VE" // this isn't working as expected
  val abbrevs = Seq(month, state, honorific, place, org, abbrev).flatMap(_.split("\n").map(_.trim).filter(_.length > 0).map(_ + "\\.")).mkString("|")
  //println("RegexTokenizerHelper "+abbrevs)
  val initials = "[\\p{L}]\\.[\\p{L}\\.]*" // A.de, A.A.A.I, etc.
  val ordinals = "[0-9]{1,2}[sthnrd]+[\\-\\p{L}]+" // the second part is for 20th-century
  val numerics = "[0-9\\-.\\:/,\\+\\=%]+[0-9\\-:/,\\+\\=%]" // is there a better way to say, "doesn't end in '.'"?
  val email = "[\\p{L}\\p{Nd}.]+@[\\p{L}\\{Nd}\\.]+\\.[a-z]{2,4}" // email
  val briefAbbrevs = "[A-Z][a-z]?\\." // A. Mr. but not Mrs. Calif. or Institute.
  val finalPunc = "[.?!][\\p{Pf}\\p{Pe}]?" // ending/final punctuation
  val finalPunc2 = "[\\p{Pf}\\p{Pe}]?[.?!]" // ending/final punctuation followed by [.?!]
  val quotes = "[`'\"“”’]+" // mid-sentence quotes
  val symbols = """[,-:;$?&@\(\)]+""" // other symbols
  val words = "[\\w\\-0-9]+(?='([tT]|[lL]+|[sS]|[mM]|re|RE|ve|VE))" // any combo of word-chars, numbers, and hyphens
  val briefDashedWords = "[\\p{L}0-9]+(-[\\p{L}0-9\\.]+)*" // words with sequences of single dashes in them
  val dashedWords = "[\\w0-9]+(-[\\w0-9]+)*" // words with sequences of single dashes in them
  val combo = "[\\w0-9']+" // any combo of word-chars, numbers, and hyphens
    
  val tokenRegexString = Seq(contractions, abbrevs, initials, ordinals, numerics, email, briefAbbrevs, finalPunc, finalPunc2, quotes, symbols, words, briefDashedWords, dashedWords, combo).mkString("|")
  val tokenRegex = ("(?i)"+tokenRegexString).r // The (?i) makes it case insensitive
  //println("RegexTokenizerHelper "+tokenRegex)

}
