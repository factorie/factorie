package cc.factorie.app.nlp.segment

import cc.factorie.app.nlp._
import scala.collection.mutable.ArrayBuffer


/**
 * concatenates words split by hyphens in the original text based on user-provided dictionary
 * or other words in the same document. It works on the output of the tokenizer.
 * Caution: It modifies the output of the tokenizer by removing some tokens so run this before any other downstream tasks.
 * @param dictionary dictionary to lookup to check for merge eligibility
 * @param useTokens if true, other tokens in document are used to check for merge eligibility
 * @author harshal
 */
class HyphenConcatenator(dictionary: Set[String] = Set.empty[String], useTokens: Boolean) extends DocumentAnnotator {

  def process(document: Document) = {

    lazy val dictionaryFromDocWords = buildDictionaryFromDocWords(document.tokens)

    def eligibleForMerge(first: String, last:String) = dictionary((first+last).toLowerCase) || (useTokens && dictionaryFromDocWords((first+last).toLowerCase))

    var wasLastOperationMerge = false

    for(section <- document.sections){
      if(section.tokens.size<3){ //if the section has less than 3 tokens, nothing to do
      val buffer = new ArrayBuffer[Token]()
        val slidingIterator = section.tokens.sliding(3)
        while(slidingIterator.hasNext){
          slidingIterator.next() match {
            case tokens if tokens.size == 3 && tokens(1).string=="-" &&
              tokens(2).hasFollowingWhitespace && eligibleForMerge(tokens(0).string, tokens(2).string) =>
              val first = tokens.head
              val last  = tokens.last
              //create a new token and set it's string offset to the first to the last token
              val t = new Token(first.stringStart, last.stringEnd)
              //add a TokenString attr to output the concatenated string
              t.attr += new TokenString(t, first.string+last.string)
              buffer += t
              slidingIterator.drop(2)
              wasLastOperationMerge = true
            case tokens =>
              buffer += tokens.head
              wasLastOperationMerge = false
          }
        }
        //if last 3 tokens did not qualify for a merge, the last 2 tokens were skipped because of the sliding window of 3
        //add them now
        if(!wasLastOperationMerge) buffer ++= section.tokens.takeRight(2)
        while(section.tokens.size>0){
          section.remove(0)
        }
        section ++= buffer
      }
    }
    document
  }

  def buildDictionaryFromDocWords(tokens: Iterable[Token]) = tokens.filterNot(_.isPunctuation).map(_.string).toSet

  // NOTE: this method may mutate and return the same document that was passed in
  def prereqAttrs = List(classOf[Token])

  def postAttrs = List(classOf[Token])

  /** How the annotation of this DocumentAnnotator should be printed in one-word-per-line (OWPL) format.
      If there is no per-token annotation, return null.  Used in Document.owplString. */
  def tokenAnnotationString(token: Token) = token.stringStart.toString+'\t'+token.stringEnd.toString
}