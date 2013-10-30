package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer


/**
 * concatenates words split by hyphens in the original text based on user-provided dictionary
 * or other words in the same document. It works on the output of the tokenizer.
 * Caution: It modifies the output of the tokenizer by removing some tokens so run this before any other downstream tasks.
 * @param tokenizer tokenizer to use to tokenize the doc. Default is DeterministicTokenizer
 * @param dictionary dictionary to lookup to check for merge eligibility
 * @param useTokens if true, other tokens in document are used to check for merge eligibility
 * @author harshal
 */
class DehyphenatingTokenizer[T <: DocumentAnnotator](tokenizer: T = new DeterministicTokenizer(), dictionary: Set[String] = Set.empty[String], useTokens: Boolean) extends DocumentAnnotator {

  def tokenize(document: Document) = tokenizer.process(document)

  def process(document: Document) = {
    val tokenizedDoc = tokenize(document)

    lazy val dictionaryFromDocWords = buildDictionaryFromDocWords(tokenizedDoc.tokens)

    def eligibleForMerge(first: String, last:String) = dictionary((first+last).toLowerCase) || (useTokens && dictionaryFromDocWords((first+last).toLowerCase))

    var _skipCounter = 0

    for(section <- tokenizedDoc.sections){
      if(section.tokens.size>2){ //if the section has less than 3 tokens, nothing to do
      var lastWindow: IndexedSeq[Token] = null
        for(tokens <- section.tokens.sliding(3).toList){
          lastWindow = tokens
          if(_skipCounter==0 && tokens(1).string=="-" && tokens(2).hasFollowingWhitespace
            && eligibleForMerge(tokens(0).string, tokens(2).string)) {
            val first = tokens.head
            val last  = tokens.last
            //create a new token and set it's string offset to the first to the last token
            val t = new Token(first.stringStart, last.stringEnd)
            //add a TokenString attr to output the concatenated string
            t.attr += new TokenString(first, first.string+last.string)
            section.remove(first.positionInSection)
            section.insert(first.positionInSection, t)
            //next two windows must be skipped
            _skipCounter = 2
          }
          else{
            //removes the next two tokens after a merge
            if(_skipCounter != 0) {
              section.remove(tokens(0).positionInSection)
              _skipCounter-=1
            }
          }
        }
        //if the last window was merged then
        if(_skipCounter>0){
          section.remove(lastWindow(1).positionInSection)
          section.remove(lastWindow(2).positionInSection)
        }
      }
    }
    tokenizedDoc
  }

  def buildDictionaryFromDocWords(tokens: Iterable[Token]) = tokens.filterNot(_.isPunctuation).map(_.string).toSet

  def prereqAttrs: Iterable[Class[_]] = Nil
  def postAttrs:   Iterable[Class[_]] = List(classOf[Token])

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString
}