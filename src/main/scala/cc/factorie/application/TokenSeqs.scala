/* Copyright (C) 2008-2010 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.application
import scala.reflect.Manifest
import cc.factorie._
import cc.factorie.er._
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import scala.util.Sorting

// application.LabeledTokenSeqs should be changed to re-use functionality here 

/** Predefined variables and factor templates for applying FACTORIE to sequences of Tokens.
    The Token remembers its String 'word', but its variable 'value' is as a BinaryVectorVariable.
    This package also provides Getters for Tokens, enabling template building with the tools in cc.factorie.er.
    For example usage see cc.factorie.example.SpanNER1
 
    @author Andrew McCallum
    @since 0.8.1
 */
object TokenSeqs {

  /** A word token in a linear sequence of tokens.  It is a constituent of a TokenSeq.
      Its value is a BinaryVectorVariable, its feature vector.
      It provides access to its neighbors in the sequence and its label.  It also has an entity-relationship counterpart. */
  @DomainInSubclasses
  abstract class Token[S<:VariableSeq[This], This>:Null<:Token[S,This] with VarInTypedSeq[This,S]](val wordForm:String, features:Seq[String] = Nil)
  extends BinaryVectorVariable[String](features) with VarInTypedSeq[This,S] with Entity[This] with TokenInSeq[This] {
    this: This =>
    //this ++= features
    //def this(word:String) = this(word, Nil)
    type GetterType <: TokenGetter[S,This]
    class GetterClass extends TokenGetter[S,This]
    def matches(t2:Token[S,This]): Boolean = word == t2.word
    /** Return true if the first  character of the word is upper case. */
    def isCapitalized = java.lang.Character.isUpperCase(word(0))
    def isPunctuation = word.matches("\\{Punct}")
    def containsLowerCase = word.exists(c => java.lang.Character.isLowerCase(c))
    /* Return true if the word contains only digits. */
    def isDigits = word.matches("\\d+")
    /* Return true if the word contains at least one digit. */
    def containsDigit = word.matches(".*\\d.*")
    /** Return a string that captures the generic "shape" of the original word, 
        mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
        Skip more than 'maxRepetitions' of the same character class. */
    def wordShape(maxRepetitions:Int) = TokenSeqs.wordShape(word, maxRepetitions)
    def charNGrams(min:Int, max:Int): Seq[String] = TokenSeqs.charNGrams(word, min, max)
    def word = wordForm
  }

  
  /** Implementation of the entity-relationship language we can use with Token objects. */
  class TokenGetter[S<:VariableSeq[T],T>:Null<:Token[S,T]] extends EntityGetter[T] {
    def newTokenGetter = new TokenGetter[S,T];
    /** Go from a token to the next token. */
    def next = initManyToMany[T](newTokenGetter,
      (token:T) => if (!token.hasNext) Nil else List(token.next), 
      (token:T) => if (!token.hasPrev) Nil else List(token.prev))
    /** Go from a token to the previous token. */
    def prev = initManyToMany[T](newTokenGetter,
      (token:T) => if (!token.hasPrev) Nil else List(token.prev), 
      (token:T) => if (!token.hasNext) Nil else List(token.next))
    /** Go from a token to the collection of the next 'n' tokens. */
    def next(n:Int) = initManyToMany[T](newTokenGetter,
      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret },
      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret })
    /** Go from a token to the collection of the previous 'n' tokens. */
    def prev(n:Int) = initManyToMany[T](newTokenGetter,
      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret },
      (t:T) => { var i = n; var ret:List[T] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret })
    /** All the other tokens in the Sentence. */
    def sentenceTokens = initManyToMany[T](newTokenGetter,
      (token:T) => token.seq, 
      (token:T) => token.seq)
    /** Return a BooleanObservation with value true if the word of this Token is equal to 'w'.  
        Intended for use in tests in er.Formula, not as a feature itself.  
        If you want such a feature, you should += it to the Token (BinaryVectorVariable) */
    def isWord(w:String) = getOneToOne[BooleanObservationWithGetter](
      // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
      token => if (token.word == w) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
      bool => throw new Error("Constant bool shouldn't change"))
    /** Return a BooleanObservation with value true if the word of this Token is capitalized.  
        Intended for use in tests in er.Formula, not as a feature itself.  
        If you want such a feature, you should += it to the Token (BinaryVectorVariable) */
    def isCapitalized = getOneToOne[BooleanObservationWithGetter](
      // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
      token => if (java.lang.Character.isUpperCase(token.word.first)) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
      bool => throw new Error("Constant bool shouldn't change"))
  }

  
  
  
  // Companion object is below.
  class TokenSeq[T>:Null<:Token[This,T],This<:VariableSeq[T]] extends VariableSeq[T] {
    this: This =>
    /** Add new features created as conjunctions of existing features, with the given offsets.
        For example addNeighboringFeatures(List(0,0),List(-2,-1,0),List(0,1)) */
    def addNeighboringFeatureConjunctions(offsetConjunctions:Seq[Int]*): Unit = 
      addNeighboringFeatureConjunctions(null.asInstanceOf[String], offsetConjunctions:_*)
    /** Add new features created as conjunctions of existing features, with the given offsets, but only add features matching regex pattern. */
    def addNeighboringFeatureConjunctions(regex:String, offsetConjunctions:Seq[Int]*): Unit = {
      // First gather all the extra features here,...
      val newFeatures = Array.tabulate(this.size)(i => new ArrayBuffer[String])
      for (i <- 0 until size) {
        val token = this(i)
        val thisTokenNewFeatures = newFeatures(i)
        for (offsets <- offsetConjunctions) 
          thisTokenNewFeatures ++= appendConjunctions(regex, token, null, offsets).
            map(list => list.sortBy({case(f,o)=>f+o}).map({case(f,o)=>f+"@"+o}).mkString("_&_")) // TODO "f+o" is doing string concatenation, consider something faster
      }
      // ... then add them to each Token
      for (i <- 0 until size) {
        val token = this(i)
        token.zero
        token ++= newFeatures(i)
      }
      //if (size > 0) println("addNeighboringFeatureConjunctions "+first)
    }
    // Recursive helper function for previous method, expanding out cross-product of conjunctions in tree-like fashion.
    // 't' is the Token to which we are adding features; 'existing' is the list of features already added; 'offsets' is the list of offsets yet to be added
    private def appendConjunctions(regex:String, t:T, existing:ArrayBuffer[List[(String,Int)]], offsets:Seq[Int]): ArrayBuffer[List[(String,Int)]] = {
      val result = new ArrayBuffer[List[(String,Int)]];
      val offset: Int = offsets.first
      val t2 = t.next(offset)
      val adding: Seq[String] = 
        if (t2 == null) { if (t.position + offset < 0) List("<START>") else List("<END>") }
        else if (regex != null) t2.values.filter(str => str.matches(regex)) // Only include features that match pattern 
        else t2.values
      if (existing != null) {
        for (e <- existing; a <- adding) { val elt = (a,offset); if (!e.contains(elt)) result += (a,offset) :: e }
      } else {
        for (a <- adding) result += List((a,offset))
      }
      if (offsets.size == 1) result
      else appendConjunctions(regex, t, result, offsets.drop(1))
    }
    /** Copy features into each token from its preceding and following tokens, 
        with preceding extent equal to preOffset and following extent equal to -postOffset.
        In other words, to add features from the three preceeding tokens and the two following tokens,
        pass arguments (-3,2).
        Features from preceding tokens will have suffixes like "@-1", "@-2", etc.
        Features from following tokens will have suffixes like "@+1", "@+2", etc. 
        The functionality of this method is completely covered as a special case of addNeighboringFeatureConjunctions,
        but for the simple case, this one is easier to call. */
    def addNeighboringFeatures(preOffset:Int, postOffset:Int): Unit = {
      // First gather all the extra features here, then add them to each Token
      val extraFeatures = Array.fromFunction(i => new ArrayBuffer[String])(this.size)
      assert(preOffset < 1)
      val preSize = -preOffset; val postSize = postOffset
      for (i <- 0 until size) {
        val token = this(i)
        val thisTokenExtraFeatures = extraFeatures(i)
        // Do the preWindow features
        var t = token; var j = 0
        while (j < preSize && t.hasPrev) {
          t = t.prev; j += 1; val suffix = "@+"+j
          thisTokenExtraFeatures ++= t.values.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
        }
        // Do the postWindow features
        t = token; j = 0
        while (j < postSize && t.hasNext) {
          t = t.next; j += 1; val suffix = "@-"+j
          thisTokenExtraFeatures ++= t.values.map(str => str+suffix) // t.values is the list of Strings representing the current features of token t
        }
      }
      // Put the new features in the Token
      for (i <- 0 until size) (this(i)) ++= extraFeatures(i)
    }
    
    def print(out:java.io.OutputStream): Unit = {
      throw new Error("Not yet implemented")
    }
  }

  
  
  
  /** Tools for creating and evaluating TokenSeq 
   
      @author Andrew McCallum
      @since 0.8
   */
  object TokenSeq {
    import scala.util.matching.Regex
    import scala.io.Source
    /** Be default we make words from contiguous sequences of one of the three following character classes: alphabetic, digits, punctuation. */
    val defaultLexer = nonWhitespaceClasses
    val alphaLexer = new Regex("\\p{Alpha}+")
    val wordLexer = new Regex("\\w+")
    val nonWhitespaceLexer = new Regex("\\S+")
    val wordClassesLexer = new Regex("\\p{Alpha}+|\\p{Digit}+")
    val nonWhitespaceClasses = new Regex("\\p{Alpha}+|\\p{Digit}+|\\p{Punct}")
    private val whitespaceRegex = new Regex("\\s+")

    /** Construct and return a new TokenSeq (and its constituent Tokens and Labels) 
        from a source containing SGML markup to indicate the labels on some tokens. 
        Tokens not bounded by SGML will be given a Label with initial and true value 'backgroundLabelString'. 
        Token segmentation will be performed by the extent of regular expression matches to 'lexer'. */
    def fromSGML[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, featureFunction: String=>Seq[String], lexer:Regex): S = {
      val words = lexer.findAllIn(source.mkString)
      throw new Error("Not implemented yet.")
    }

    /** Construct and return a new TokenSeq (and its constituent Tokens and Labels) 
        from a source containing plain text.  Since the labels are unknown, all Labels
        will be given the initial and true value 'defaultLabelString'. */
    def fromPlainText[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, defaultLabelString:String, featureFunction: String=>Seq[String], lexer:Regex): S = {
      val seq = newSeq()
      lexer.findAllIn(source.mkString).foreach(word => {
        if (word.length > 0) {
          val token = newToken(word, defaultLabelString)
          token ++= featureFunction(word)
          seq += token
         }
      })
      seq
    }

    /** Create a TokenSeq from a source of characters that has "one word per line", 
        each line consisting of information about one token: a whitespace-separated list of elements, 
        in which the first element is the word itself and the last element is the true target label for the token.
        The CoNLL 2003 NER Shared Task is an example of such a format.
        Token.word will be set to the first element.
        All elements but the last will be passed to to 'featureFunction', 
        and its returned strings will be added as features to the BinaryVectorVariable.
        The initial and trueValue of the Label will be set from the last element.
        If ignoreLines is non-null, we skip any lines containing this pattern, for example pass "-DOCSTART-" for CoNLL 2003.
        */
    def fromOWPL[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, 
                                                 featureFunction:Seq[String]=>Seq[String], labelFunction:String=>String, 
                                                 documentBoundary:Regex, sentenceBoundary:Regex, ignoreLines:Regex): Seq[S] = {
      import scala.collection.mutable.ArrayBuffer
      var tokenCount = 0
      var seqs = new ArrayBuffer[S];
      var seq = newSeq()
      for (line <- source.getLines()) {
        if (sentenceBoundary != null && sentenceBoundary.findAllIn(line).hasNext && seq.length > 0) {
          //println("Completed sentence size=" + seq.size + " num sentences="+seqs.size)   
          seqs += seq
          seq = newSeq()
        } else if (documentBoundary != null && documentBoundary.findAllIn(line).hasNext) {
          //println("Completed document with boundary "+documentBoundary)
          if (seq.length > 0) { seqs += seq; seq = newSeq() }
          seqs += newSeq() // Insert an empty sentence to mark document boundary
        } else if (line.length < 2 || (ignoreLines != null && ignoreLines.findAllIn(line).hasNext)) {
          // Skip this line
        } else {
          val fields = line.split(' ')
          assert(fields.length == 4)
          val word = fields(0)
          val inFeatures = fields.slice(0, fields.length-1) // This used to be with ".force"
          val pos = fields(1)
          val label = labelFunction(fields.last.stripLineEnd)
          val token = newToken(word, label)
          token ++= featureFunction(inFeatures)
          seq += token
          tokenCount += 1
        }
      }
      //println("Loaded "+seqs.length+" sentences with "+wordCount+" words total from file "+filename)
      seqs
    }
    // TODO Waiting for Scala 2.8 default parameter values
    def defaultFeatureFunction(inFeatures:Seq[String]): Seq[String] = {
      val result = new ArrayBuffer[String]
      // Assume the first feature is the word
      result += "W="+inFeatures(0)
      result += "SHAPE="+wordShape(inFeatures(0), 2)
      result ++= charNGrams(inFeatures(0), 2, 5)
      result ++= inFeatures.drop(1)
      result
    }
    def defaultLabelFunction(inLabel:String): String = inLabel
    def fromOWPL[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, featureFunction:Seq[String]=>Seq[String], labelFunction:String=>String, documentBoundary:Regex): Seq[S] = fromOWPL(source, newToken, newSeq, featureFunction, labelFunction, documentBoundary, "\\A\\s*\\z".r, null)
    def fromOWPL[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, documentBoundary:Regex): Seq[S] = fromOWPL(source, newToken, newSeq, defaultFeatureFunction _, defaultLabelFunction _, documentBoundary)
    def fromOWPL[T>:Null<:Token[S,T],S<:TokenSeq[T,S]](source:Source, newToken:(String,String)=>T, newSeq:()=>S, documentBoundary:String): Seq[S] = fromOWPL(source, newToken, newSeq, defaultFeatureFunction _, defaultLabelFunction _, documentBoundary.r)
  }  
  
  
  
  
  
  // Feature extraction aids
  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def wordShape(word:String, maxRepetitions:Int): String = {
    val sb = new StringBuffer
    var i = 0; var c = 'x'; var prevc = 'x'; var repetitions = 0
    while (i < word.length) {
      val char = word(i); 
      if (Character.isUpperCase(char)) c = 'A'
      else if (Character.isLowerCase(char)) c = 'a'
      else if (Character.isDigit(char)) c = '1'
      else if (Character.isWhitespace(char)) c = ' '
      else c = char
      if (c == prevc) repetitions += 1
      else { prevc = c; repetitions = 0 }
      if (repetitions < maxRepetitions) sb.append(c)
      i += 1
    }
    sb.toString
  }
  def charNGrams(word:String, min:Int, max:Int): Seq[String] = {
    val w = "<"+word+">"
    val prefixes = for (e <- min+1 to Math.min(max+1, word.length)) yield w.substring(0, e)
    val suffices = for (b <- Math.max(w.length-1-max, 0) to w.length-1-min) yield w.substring(b, w.length)
    prefixes ++ suffices
    //for (i <- 0 until w.length; j <- min to max; if (i+j < w.length)) yield w.substring(i,i+j)
  }
  

  trait TokenInSeq[This<:TokenInSeq[This]] {
    def word: String
    def next: This
    def prev: This
    def hasNext: Boolean
    def hasPrev: Boolean
    def firstInSeq: This
    def tokensInSeq: Iterator[This] = new Iterator[This] {
      var t: This = firstInSeq
      def hasNext: Boolean = t != null
      def next: This = { val result = t; if (t.hasNext) t = t.next else t = null.asInstanceOf[This]; result }
    }
  }

  class Lexicon(val caseSensitive:Boolean) {
    import scala.io.Source
    import java.io.File
    /** Populate lexicon from file, with one entry per line, consisting of space-separated tokens. */
    def this(filename:String) = { this(false); this.++=(Source.fromFile(new File(filename))(scala.io.Codec.UTF8)) }
    var lexer = TokenSeq.nonWhitespaceClasses
    private class LexiconToken(val word:String) extends TokenInSeq[LexiconToken] {
      var next: LexiconToken = null
      var prev: LexiconToken = null
      def hasNext = next != null
      def hasPrev = prev != null
      def firstInSeq = if (prev == null) this else prev.firstInSeq
      def lengthToEnd: Int = if (next == null) 1 else 1 + next.lengthToEnd
      def length = firstInSeq.lengthToEnd
      def seq: Seq[LexiconToken] = {
        val result = new ArrayBuffer[LexiconToken];
        var t = firstInSeq; result += t
        while (t.hasNext) { t = t.next; result += t }
        result
      }
    }
    private def newLexiconTokens(words:Seq[String]): Seq[LexiconToken] = {
      val result = new ArrayBuffer[LexiconToken]
      var t: LexiconToken = null
      for (word <- words) {
        val t2 = new LexiconToken(word)
        t2.prev = t
        if (t != null) t.next = t2
        t = t2
        result += t2
      }
      result
    }
    private val contents = new HashMap[String,List[LexiconToken]];
    private def _key(s:String) = if (caseSensitive) s else s.toLowerCase
    private def +=(t:LexiconToken): Unit = {
      val key = _key(t.word)
      val old: List[LexiconToken] = contents.getOrElse(key, Nil)
      contents(key) = t :: old
    }
    private def addAll(ts:Seq[LexiconToken]): Unit = {
      //println("Lexicon adding "+ts.map(_.word))
      ts.foreach(t => this += t)
    }
    def +=(w:String): Unit = this.+=(new LexiconToken(w))
    def ++=(ws:Seq[String]): Unit = this.addAll(newLexiconTokens(if (caseSensitive) ws else ws.map(_.toLowerCase)))
    def ++=(source:Source): Unit = for (line <- source.getLines()) { this.++=(lexer.findAllIn(line).toList); /*println("TokenSeqs.Lexicon adding "+line)*/ }
    /** Is 'query' in the lexicon, accounting for lexicon phrases and the context of 'query' */
    def contains[T<:TokenInSeq[T]](query:T): Boolean = {
      //println("contains "+query.word+" "+query.hasPrev+" "+query)
      val key = _key(query.word)
      val entries = contents.getOrElse(key, Nil)
      for (entry <- entries) {
        var te = entry
        var tq = query
        var result = true
        // Go the beginning of this lexicon entry
        while (te.hasPrev && result) {
          if (!tq.hasPrev) return false
          te = te.prev; tq = tq.prev
        }
        //println("  Trying "+query.word+" "+entry.seq.map(_.word).toList)
        // Check for match all the way to the end of this lexicon entry
        do {
          if ((!caseSensitive && te.word != tq.word.toLowerCase) || (caseSensitive && te.word != tq.word)) result = false
          te = te.next; tq = tq.next
        } while (te != null && tq != null && result == true)   
        if (result && te == null) {
          //print(" contains length="+entry.length+"  "+entry.seq.map(_.word).toList)
          return true
        }
      }
      false
    }
    /** Is 'query' in the lexicon, ignoring context. */
    def containsSingle[T<:TokenInSeq[T]](query:T): Boolean = contents.contains(_key(query.word))
  }

  
}
