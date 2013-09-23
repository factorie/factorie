/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.nlp
import cc.factorie._
import cc.factorie.util.Attr
import scala.collection.mutable.ArrayBuffer

/** A part of a Document, delineated by character offsets into the Document's string,
    and which can hold a sequence of Tokens, a sequence of Sentences and a sequence
    of other arbitrary TokenSpans.
    
    By defining Section in terms of character offsets instead of by Token positions
    we gain the ability to (a) split the Document into Sections before tokenization, 
    (b) run different tokenizers in different sections, (c) even have overlapping 
    Sections with alternative tokenization and annotation for the same text.
    
    The canonical sequence of Sections in a Document is available as Document.sections,
    but a Document may have multiple overlapping Sections (for example to store alternative tokenizations
    or wholly distinct sets of annotations in other "non-canonical" Sections, which may be stored
    by some customized scheme in the Document attributes, Document.attr.
    
    In addition to their canonical sequence of Sections, all Documents also have a Section that
    encompasses the entire Document (even if the Document grows in length).  This is accessed
    via Document.asSection.  This is the sole member of the default Document.sections, but 
    be cautious about always using Document.asSection to get the Documents Tokens, sentences and
    their annotations, because some other processing may reset the canonical sequence of Sections
    to some other collection.
    
    If you want to tokenize first, and then split a Document into Sections, you can tokenize
    into Document.asSection, and then create new canonical Section at your desired boundaries,
    and then re-tokenize each Section.  (In the future we may provide a way to avoid the 
    computation of re-tokenizing.)  
    
    @author Andrew McCallum */
trait Section extends ChainWithSpansVar[Section,TokenSpan,Token] with DocumentSubstring with Attr {
  /** The sub-string of the Document string encompased by this Section.
      Note that the returned string will not include any Token.string substitutions 
      (e.g. WSJ normalization of quotation styles or de-hyphenation, typically implemented using TokenString) 
      from the Document's original raw string  */
  def string: String = document.string.substring(stringStart, stringEnd)
  
  /** The sequence of Tokens inside this Section.  Just a clearly-named alias for Chain.links. */
  def tokens: IndexedSeq[Token] = links
  
  // Managing Sentences
  private var _sentences = new ArrayBuffer[Sentence]
  /** The sequence of Sentences in this Section.  Sentences can be added by Section.+=(Sentence),
      but only added in order, and never removed. */
  def sentences: Seq[Sentence] = _sentences
  /** Does this Section have a non-zero number of Sentences?
      Note that a Section can have Tokens but no Sentences. */
  def hasSentences: Boolean = _sentences.length > 0

  // Managing Spans, keeping Sentence-spans separate from all other TokenSpans
  /** Add a new TokenSpan to this Section.  Since a Sentence is a TokenSpan, this is also used to add new Sentences. */
  override def +=(s:TokenSpan): Unit = s match {
    case s:Sentence => {
      s._chain = this // not already done in += be cause += is not on ChainWithSpans
      s._indexInSection = _sentences.length
      if (_sentences.length == 0 || _sentences.last.end <= s.start) _sentences += s
      else throw new Error("Sentences must be added in order and not overlap.")
    }
    case s:TokenSpan => super.+=(s)
  }
  /** Remove a TokenSpan from this Section.  Note that Sentences cannot be removed. */
  override def -=(s:TokenSpan): Unit = s match {
    case s:Sentence => throw new Error("Once added Sentences cannot be removed from a Section.") // _sentences -= s
    case s:TokenSpan => super.-=(s)
  }
}

/** A simple concrete implementation of Section. */
class BasicSection(val document:Document, val stringStart:Int, val stringEnd:Int) extends Section

