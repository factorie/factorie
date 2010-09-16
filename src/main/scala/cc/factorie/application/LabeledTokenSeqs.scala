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



package cc.factorie.application
import scala.reflect.Manifest
import cc.factorie._
import cc.factorie.er._
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}
import scala.util.Sorting

/** Predefined variables and factor templates for applying FACTORIE to sequences of Tokens, each paired with a categorical Label.
    The Token remembers its String 'word', but its variable 'value' is as a BinaryFeatureVectorVariable.
    This package also provides Getters for Tokens and Labels, enabling template building with the tools in cc.factorie.er.
    For exmaple usage see cc.factorie.example.ChainNER1
 
    @author Andrew McCallum
    @since 0.8
 */
object LabeledTokenSeqs {
    
  /** A word token in a linear sequence of tokens.  It is a constituent of a LabeledTokenSeq.
      Its value is a BinaryFeatureVectorVariable, its feature vector.
      It provides access to its neighbors in the sequence and its label.  It also has an entity-relationship counterpart. */
  @DomainInSubclasses
  abstract class Token[L<:Label[This,L], This>:Null<:Token[L,This] with VarInSeq[This]](val word:String, features:Seq[String] = Nil)
  extends BinaryFeatureVectorVariable[String](features) with VarInSeq[This] with Entity[This] with TokenInSeq[This] {
    this: This =>
    //def this(word:String) = this(word, Nil)
    type GetterType <: TokenGetter[L,This]
    class GetterClass extends TokenGetter[L,This]
    val label: L //= new Label(labelString, this)
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
    def wordShape(maxRepetitions:Int) = LabeledTokenSeqs.wordShape(word, maxRepetitions)
    def charNGrams(min:Int, max:Int): Seq[String] = LabeledTokenSeqs.charNGrams(word, min, max)
    private lazy val svmap = new HashMap[String,BinaryFeatureVectorVariable[String]]
    def subVector(regex:String): BinaryFeatureVectorVariable[String] = svmap.getOrElseUpdate(regex, newSubVector(regex))
    private def newSubVector(regex:String): BinaryFeatureVectorVariable[String] = {
      val result = new BinaryFeatureVectorVariable[String] { override def printName = "TokenSubVector" }
      result ++= this.values.filter(s => s.matches(regex))
      result
    }
  }
  
  /** Implementation of the entity-relationship language we can use with Token objects. */
  class TokenGetter[L<:Label[T,L],T>:Null<:Token[L,T]] extends EntityGetter[T] {
    def newLabelGetter = new LabelGetter[T,L]
    def newTokenGetter = new TokenGetter[L,T]
    /** Go from a token to its label. */
    def label = initOneToOne[L](newLabelGetter,
      token=>token.label.asInstanceOf[L], 
      label => label.token)
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
        If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
    def isWord(w:String) = getOneToOne[BooleanObservationWithGetter](
      // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
      token => if (token.word == w) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
      bool => throw new Error("Constant bool shouldn't change"))
    /** Return a BooleanObservation with value true if the word of this Token is capitalized.  
        Intended for use in tests in er.Formula, not as a feature itself.  
        If you want such a feature, you should += it to the Token (BinaryFeatureVectorVariable) */
    def isCapitalized = getOneToOne[BooleanObservationWithGetter](
      // TODO Consider making this more efficient by looking up an already-constructed instance, as in "object Bool"
      token => if (java.lang.Character.isUpperCase(token.word.head)) new BooleanObservationWithGetter(true) else new BooleanObservationWithGetter(false),
      bool => throw new Error("Constant bool shouldn't change"))
    def subVector(regex:String) = getOneWay(t=>t.subVector(regex))
  }

  /** A Label associated with a Token. */
  // NOTE: If you remove final, add a comment warning the user that different subclasses of will share the same Domain.
  // I don't think we should allow subclassing, hence the "final". -akm
  @DomainInSubclasses
  abstract class Label[T>:Null<:Token[This,T],This<:Label[T,This]](labelname: String, val token: T) extends LabelVariable(labelname) with Entity[This] {
    this: This =>
    type GetterType <: LabelGetter[T,This]
    class GetterClass extends LabelGetter[T,This]
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next: This = if (token.next == null) null.asInstanceOf[This] else token.next.label
    def prev: This = if (token.prev == null) null.asInstanceOf[This] else token.prev.label
  }
  
  // Define boilerplate, to support access to attributes in the entity-attribute-relationship syntax
  class LabelGetter[T>:Null<:Token[ThisLabel,T],ThisLabel<:Label[T,ThisLabel]] extends EntityGetter[ThisLabel] {
    def newTokenGetter = new TokenGetter[ThisLabel,T]
    def newLabelGetter = new LabelGetter[T,ThisLabel]
    def token = initOneToOne[T](newTokenGetter, label => label.token.asInstanceOf[T], token => token.label)
    def next = initManyToMany[ThisLabel](newLabelGetter,
      label => if (!label.token.hasNext) Nil else List(label.token.next.label),
      label => if (!label.token.hasPrev) Nil else List(label.token.prev.label))
    def prev = initManyToMany[ThisLabel](newLabelGetter,
      label => if (!label.token.hasPrev) Nil else List(label.token.prev.label),
      label => if (!label.token.hasNext) Nil else List(label.token.next.label))
  }
  
 
  // Companion object is below.
  class LabeledTokenSeq[T>:Null<:Token[L,T],L<:Label[T,L]] extends VariableSeq[T] {
    /** Return the collection of Label instances attached to these tokens. */
    def labels = this.map(_.label)
    /** Return the proportion of Labels whose current value is their trueValue. */
    def accuracy: Double = this.foldLeft(0)((sum,token) => if (token.label.valueIsTruth) sum + 1 else sum) / size.toDouble
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
            map(list => list.sortBy({case(f,o)=>f+o}).map({case(f,o)=>f+"@"+o}).mkString("_&_"))
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
      val offset: Int = offsets.head
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
      val extraFeatures = Array.tabulate(this.size)(i => new ArrayBuffer[String])
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
    
    def entities(background:String): Seq[(L,Seq[T])] = {
      val result = new ArrayBuffer[(L,Seq[T])]
      var label = head.label
      var entity: List[T] = Nil
      for (token <- this) {
        if (token.label.value != background) {
          if (token.label.value == label.value)
            entity = token :: entity
          else {
            if (entity.length > 0) result += ((label,entity.reverse))
            entity = token :: Nil
            label = token.label
          }
        } else {
          if (entity.length > 0) result += ((label,entity.reverse))
          entity = Nil
          label = token.label
        }
      }
      result
    }

    def print(out:java.io.OutputStream): Unit = {
      throw new Error("Not yet implemented")
    }
  }

  /** Tools for creating and evaluating LabeledTokenSeq 
   
      @author Andrew McCallum
      @since 0.8
   */
  object LabeledTokenSeq {
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

    /** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels) 
        from a source containing SGML markup to indicate the labels on some tokens. 
        Tokens not bounded by SGML will be given a Label with initial and true value 'backgroundLabelString'. 
        Token segmentation will be performed by the extent of regular expression matches to 'lexer'. */
    def fromSGML[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, backgroundLabelString:String, featureFunction: String=>Seq[String], lexer:Regex): LabeledTokenSeq[T,L] = {
      val words = lexer.findAllIn(source.mkString)
      throw new Error("Not implemented yet.")
    }

    /** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels) 
        from a source containing plain text.  Since the labels are unknown, all Labels
        will be given the initial and true value 'defaultLabelString'. */
    def fromPlainText[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, defaultLabelString:String, featureFunction: String=>Seq[String], lexer:Regex): LabeledTokenSeq[T,L] = {
      val seq = new LabeledTokenSeq[T,L]
      lexer.findAllIn(source.mkString).foreach(word => {
        val token = newToken(word, defaultLabelString)
        token ++= featureFunction(word)
        seq += token
      })
      seq
    }

    /** Create a LabeledTokenSeq from a source of characters that has "one word per line", 
        each line consisting of information about one token: a whitespace-separated list of elements, 
        in which the first element is the word itself and the last element is the true target label for the token.
        The CoNLL 2003 NER Shared Task is an example of such a format.
        Token.word will be set to the first element.
        All elements but the last will be passed to to 'featureFunction', 
        and its returned strings will be added as features to the BinaryFeatureVectorVariable.
        The initial and trueValue of the Label will be set from the last element.
        If ignoreLines is non-null, we skip any lines containing this pattern, for example pass "-DOCSTART-" for CoNLL 2003.
        */
    def fromOWPL[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, featureFunction:Seq[String]=>Seq[String], documentBoundary:Regex, sentenceBoundary:Regex, ignoreLines:Regex): Seq[LabeledTokenSeq[T,L]] = {
      import scala.collection.mutable.ArrayBuffer
      var tokenCount = 0
      var seqs = new ArrayBuffer[LabeledTokenSeq[T,L]];
      var seq = new LabeledTokenSeq[T,L]
      for (line <- source.getLines()) {
        if (sentenceBoundary != null && sentenceBoundary.findAllIn(line).hasNext && seq.length > 0) {
          //println("Completed sentence size=" + seq.size + " num sentences="+seqs.size)   
          seqs += seq
          seq = new LabeledTokenSeq
        } else if (documentBoundary != null && documentBoundary.findAllIn(line).hasNext) {
          //println("Completed document with boundary "+documentBoundary)
          if (seq.length > 0) { seqs += seq; seq = new LabeledTokenSeq }
          seqs += new LabeledTokenSeq // Insert an empty sentence to mark document boundary
        } else if (line.length < 2 || (ignoreLines != null && ignoreLines.findAllIn(line).hasNext)) {
          // Skip this line
        } else {
          val fields = line.split(' ')
          val word = fields(0)
          val label = fields.last.stripLineEnd  // label is the last feature
          val token = newToken(word, label)
          val inFeatures = fields.slice(0, fields.length-1) // Used to also have here ".force"
          token ++= featureFunction(inFeatures)
          seq += token
          tokenCount += 1
        }
      }
      // gdruck: This is necessary because a file
      // might not end with a sentence boundary.
      if (seq.length > 0) {
        seqs += seq
      }
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
    def fromOWPL[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, featureFunction:Seq[String]=>Seq[String], documentBoundary:Regex): Seq[LabeledTokenSeq[T,L]] = fromOWPL(source, newToken, featureFunction, documentBoundary, "\\A\\s*\\z".r, null)
    def fromOWPL[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, documentBoundary:Regex): Seq[LabeledTokenSeq[T,L]] = fromOWPL(source, newToken, defaultFeatureFunction _, documentBoundary)
    def fromOWPL[T>:Null<:Token[L,T],L<:Label[T,L]](source:Source, newToken:(String,String)=>T, documentBoundary:String): Seq[LabeledTokenSeq[T,L]] = fromOWPL(source, newToken, defaultFeatureFunction _, documentBoundary.r)
    
    class PerLabelEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](val labelValue: String)(implicit m:Manifest[L]) {
      var fp = 0
      var fn = 0
      var tp = 0
      private val targetIndex = Domain[L](m).index(labelValue)

      def ++=(seqs:Seq[LabeledTokenSeq[T,L]]) = seqs.foreach(this += _)
      def +=(seq:LabeledTokenSeq[T,L]): Unit = +=(seq.map(_.label))
      def +=(labels: Seq[L])(implicit m:Manifest[L]): Unit = {
        for (l <- labels) {
          val trueIndex = l.trueIntValue
          val predIndex = l.intValue
          if (targetIndex == trueIndex) {
            if (trueIndex == predIndex)
              tp += 1
            else
              fp += 1
          } else if (targetIndex == predIndex) {
            if (trueIndex == predIndex)
              tp += 1
            else
              fn += 1
          }
        }
      }
      def accuracy: Double = throw new Error
      def precision: Double = if (tp + fp == 0.0) 0.0 else tp.toDouble / (tp + fp)
      def recall: Double = if (tp + fn == 0.0) 0.0 else tp.toDouble / (tp + fn)
      def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
      def correctCount = tp
      def missCount = fn
      def alarmCount = fp
      override def toString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelValue, f1, precision, recall, tp, fp, fn, tp+fn, tp+fp) 
    }

/*
    class MetaLabelEvaluation[T<:Token[L,T],L<:Label[T,L]](val backgroundLabelValue:String)(implicit m:Manifest[L])
    {
      //
      //for f1 only? TODO: include pr and re
      var stddev : Double = 0
      var variance : Double = 0
      var avg : Double = 0
      var stderr : Double = 0
      var numTrials : Double = 0
    }
*/

    class LabelEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](val backgroundLabelValue:String)(implicit m:Manifest[L]) {
      import scala.collection.mutable.HashMap
      def this(labels:Seq[L])(implicit m:Manifest[L]) = { this("O"); this += labels }
      def this(lab:String, labels:Seq[L])(implicit m:Manifest[L]) = { this(lab); this += labels }
      //def this(labels:Seq[LabeledTokenSeq]) = { this("O"); this.+=(labels.flatMap(_.labels)) }
      var fp = 0
      var fn = 0
      var tp = 0
      //println("Evaluation Labels: "+Domain[Label].toList)
      private val labelEval: HashMap[String,PerLabelEvaluation[T,L]] = { 
        val h = new HashMap[String,PerLabelEvaluation[T,L]];
        h ++= Domain[L](m).map(labelString => (labelString, new PerLabelEvaluation[T,L](labelString)))
        h
      }
      /** Return the LabelEvaluation specific to labelString. */
      def apply(labelString:String) = labelEval(labelString)
      def +=(labels: Seq[L]): Unit = {
        labelEval.values.foreach(eval => { 
          eval += labels
          if (eval.labelValue != backgroundLabelValue) {
            fp += eval.fp
            fn += eval.fn
            tp += eval.tp
          }
        })
      }
      def accuracy: Double = tp.toDouble / ( tp + fp)
      def precision: Double = if (tp + fp == 0.0) 0.0 else tp.toDouble / (tp + fp)
      def recall: Double = if (tp + fn == 0.0) 0.0 else tp.toDouble / (tp + fn)
      def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
      def summaryString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp)
      override def toString = {
        val sb = new StringBuffer
        sb.append("ACCURACY "+accuracy)
        sb.append("\n")
        sb.append(summaryString)
        sb.append("\n")
        labelEval.values.foreach(e => { sb.append(e.toString); sb.append("\n") })
        sb.toString
      } 
    }

    // TODO Add more combinations of arguments
    // TODO Why does this existential type not work?
    //def labelEvaluation[L<:Label[T forSome {type T <: Token[L,T]},L]](labels:Seq[L])(implicit m:Manifest[L]) = new LabelEvaluation[L](labels)
    def labelEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](labels:Seq[L])(implicit m:Manifest[L]) = new LabelEvaluation[T,L](labels)
    
    /** Evalute in terms of correct entire segments.  
        The field start and end boundaries must be perfect to count as correct.  No partial credit. 
        For example, this is the standard for results on CoNLL 2003. */
    class PerSegmentEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](val labelName:String, val labelValueStart: Regex, val labelValueContinue: Regex) {
      //println("PerSegmentEvaluation "); println(labelName); println(labelValueStart); println(labelValueContinue); println
      //if (labelValueContinue == null) labelValueContinue = labelValueStart // Waiting for Scala 2.8 default parameters
      var trueCount, predictedCount, correctCount = 0 // per segment
      var predictedStart, trueStart = false
      def ++=(seqs:Seq[LabeledTokenSeq[T,L]]) = seqs.foreach(+= _)
      def +=(seq:LabeledTokenSeq[T,L]): Unit = +=(seq.map(_.label))
      /** Add the given sequence of labels to the statistics for this evalution.
          Even though you may be tempted to put all Label instances across all sentences in a single Seq[] and pass them in here, 
          note that you risk getting slightly incorrect results at document boundaries: when one document ends 
          in a mention and the next document begins with the same mention type, 
          they will be counted as only one mention, when they should have been counted as two. */
      def +=(labels: Seq[Label[T,L]]): Unit = {
        //println("PerSegmentEvaluation += "+labels.size)
        for (position <- 0 until labels.length) {
          val label = labels(position)
          //print("\n"+label.token.word+"="+label.trueValue+"/"+label.value+" ")
          predictedStart = false; trueStart = false
          // Find out if we are at the beginning of a segment.  
          // This complicated conditional is necessary to make the start pattern "(B|I)-" work for both BIO and IOB formats.
          // We are at a start if either (a) only labelValueStart matches, or (b) labelValueContinue matches and the previous label doesn't match
          // The (b) case makes it work for IOB notation, in which "B-*" is only used at the boundary between two like-categoried mentions.
          if ((labelValueStart.findAllIn(label.value).hasNext && !labelValueContinue.findAllIn(label.value).hasNext)
              || (labelValueContinue.findAllIn(label.value).hasNext 
                  && (!label.hasPrev || (!labelValueStart.findAllIn(label.prev.value).hasNext && !labelValueContinue.findAllIn(label.prev.value).hasNext)))) {
            predictedCount += 1
            predictedStart = true
            //print("ps ")
          }
          if ((labelValueStart.findAllIn(label.trueValue).hasNext && !labelValueContinue.findAllIn(label.trueValue).hasNext)
              || (labelValueContinue.findAllIn(label.trueValue).hasNext 
                  && ((!label.hasPrev) || (!labelValueStart.findAllIn(label.prev.trueValue).hasNext && !labelValueContinue.findAllIn(label.prev.trueValue).hasNext)))) {
            trueCount += 1
            trueStart = true
            //print("ts ")
          }
          // Truth and prediction both agree that a segment is starting here, let's see if they end in the same place
          if (predictedStart && trueStart) {
            //print(" pts ")
            //print("%s=%s ".format(label.token.word, label.value))
            var predictedContinue, trueContinue = false
            var j = position + 1
            var stopSearchForSegmentEnd = false
            while (j < labels.length && !stopSearchForSegmentEnd) {
              val label2 = labels(j)
              predictedContinue = labelValueContinue.findAllIn(label2.value).hasNext
              trueContinue = labelValueContinue.findAllIn(label2.trueValue.toString).hasNext
              //print("j="+j+predictedContinue+trueContinue)
              //if (predictedContinue) print("pc ")
              //if (trueContinue) print("tc ")
              if (!predictedContinue || !trueContinue) {
                if (predictedContinue == trueContinue) {
                  correctCount += 1 // Both sequences ended at the same position: correct
                  //print("%s=%s/%s correct".format(label2.token.word, label2.trueValue.toString, label2.value))
                } //else print("%s=%s %s=%s/%s @%d wrong".format(if (label2.hasPrev) label2.prev.token.word else "(null)", if (label2.hasPrev) label2.prev.value else "(null)", label2.token.word, label2.trueValue, label2.value, j-position))
                stopSearchForSegmentEnd = true
              } //else print("%s=%s ".format(label2.token.word, label2.value))
              j += 1
            }
            // Handle special case for the end of the sequence
            // srr: fixed this to check not only whether trueContinue==predictedContinue, but if both are continuing.
            if (j == labels.length && predictedContinue && trueContinue) trueCount += 1
          }
        }
      }
      def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
      def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
      def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
      def alarmCount = predictedCount - correctCount
      def missCount = trueCount - correctCount
      def tp = correctCount
      def fn = missCount
      def fp = alarmCount
      override def toString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelName, f1, precision, recall, tp, fp, fn, trueCount, predictedCount) 
    }
    
    // Some utilities for automatically filling in values 
    private val defaultStartPrefix = "(B|I)-" // Although just "B-" would be enough for BIO, "(B|I)-" is needed for IOB
    private val defaultContinuePrefix = "I-"
    // Assume that the first two characters of each label are the "B-" or "I-" prefix.  Skip the label "O" because it is less than 3 chars long
    private def labelStringsToBase(labelVals:Seq[String]): Seq[String] = {
      val result = new HashSet[String]
      labelVals.foreach(s => if (s.length > 2) result += s.substring(2))
      result.toSeq
    }
    
    class SegmentEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](baseLabelStrings: Seq[String], startPrefix:String, continuePrefix:String) {
      def this()(implicit m:Manifest[L]) = this(labelStringsToBase(Domain[L](m).toSeq), defaultStartPrefix, defaultContinuePrefix)
      def this(labels:Seq[L])(implicit m:Manifest[L]) = { this(); this.+=(labels) }
      private val evals = new HashMap[String,PerSegmentEvaluation[T,L]]
      private var labelCount = 0
      private var labelCorrectCount = 0
      evals ++= baseLabelStrings.map(s => (s, new PerSegmentEvaluation[T,L](s, (startPrefix+s).r, (continuePrefix+s).r)))
      /** Return the LabelEvaluation specific to labelString. */
      def apply(labelString:String) = evals(labelString)
      def +=(labels: Seq[L]): Unit = {
        evals.values.foreach(eval => eval += labels)
        labelCount += labels.length
        labels.foreach(label => if (label.valueIsTruth) labelCorrectCount += 1)
      }
      // This is a per-label measure
      def tokenAccuracy = labelCorrectCount.toDouble / labelCount
      // The rest are per-segment
      def correctCount = evals.values.foldLeft(0)(_+_.correctCount)
      def predictedCount = evals.values.foldLeft(0)(_+_.predictedCount)
      def trueCount = evals.values.foldLeft(0)(_+_.trueCount)
      def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
      def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
      def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
      def alarmCount = predictedCount - correctCount
      def missCount = trueCount - correctCount
      def tp = correctCount
      def fn = missCount
      def fp = alarmCount
      def summaryString = "%-8s f1=%-6f p=%-6f r=%-6f (tp=%d fp=%d fn=%d true=%d pred=%d) acc=%-5f (%d/%d)\n".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp, tokenAccuracy, labelCorrectCount, labelCount)
      override def toString = {
        val sb = new StringBuffer
        //sb.append("ACCURACY "+tokenAccuracy+" ("+labelCorrectCount+"/"+labelCount+")")
        //sb.append("\n")
        sb.append(summaryString)
        evals.values.foreach(e => { sb.append(e.toString); sb.append("\n") })
        sb.toString
      } 
    }

    // TODO Add more combinations of arguments
    def segmentEvaluation[T>:Null<:Token[L,T],L<:Label[T,L]](labels:Seq[L])(implicit m:Manifest[L]) = new SegmentEvaluation[T,L](labels)

  def segmentEvaluationAndGetF1[T>:Null<:Token[L,T],L<:Label[T,L]](labels:Seq[L])(implicit m:Manifest[L]) : Double = new SegmentEvaluation[T,L](labels).f1

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
    val prefixes = for (e <- min+1 to math.min(max+1, word.length)) yield w.substring(0, e)
    val suffices = for (b <- math.max(w.length-1-max, 0) to w.length-1-min) yield w.substring(b, w.length)
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
  }

  class Lexicon(val caseSensitive:Boolean) {
    import scala.io.Source
    import java.io.File
    def this(filename:String) = { this(false); this.++=(Source.fromFile(new File(filename))) }
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
    def ++=(source:Source): Unit = for (line <- source.getLines()) this.++=(line.trim.split("\\s+"))
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

  /**A proposer that enforces BIO constraints*/
  abstract class BIOProposer[T>:Null<:Token[L,T],L<:Label[T,L]](model:Model) extends MHSampler[L](model)
  {
    def labelSpace : Array[String]
    def labels : Seq[Label[T,L]] //def labels = this.map(_.label)
    def propose(context:L)(implicit delta:DiffList) : Double =
      {
  //val labelSpace = Domain[Label]
  val label = labels(random.nextInt(labels.size))
  //var label = labels(indices.get(index))
  var newLabel = labelSpace(random.nextInt(labelSpace.length))
  if(newLabel.startsWith("I-"))
    {
      val suffix = newLabel.substring(2,newLabel.length)
      if(label.hasPrev && label.prev.value.indexOf(suffix) == -1)
        label.prev.set("B-"+suffix)(delta)
      if(!label.hasPrev)
        newLabel="B-"+suffix
    }
  
  if(newLabel.startsWith("B-"))
    {
      val suffix = newLabel.substring(2,newLabel.length)
      if(label.hasNext && label.next.value.indexOf("I-") != -1 && label.next.value.indexOf(suffix) == -1)
        {
    //TODO check if label.next.next isn't violated
    if(random.nextBoolean)
      label.next.set("I-"+suffix)(delta)
    else 
      label.next.set("O")(delta)
        }
    }
  label.set(newLabel)(delta)
  0.0 //TODO calculate this precisely
      }
  }

/*
  class FirstOrderDecoder[T,L](amodel : Model)
  {
    var model : Model = amodel
    var transitionTemplates : ArrayBuffer[DotTemplate]
    var observationTemplates : ArrayBuffer[DotTemplate]
    var labelPriorTemplates : ArrayBuffer[DotTemplate]
   
    def setModel(amodel:Model) : Unit =
      {
  if(model == amodel)
    return
  model = amodel
  for(template <- model.templatesOf[DotTemplate])
    {
      if(template.isInstanceOf[DotTemplate[L, L]])
        System.out.println("SWEET")
      else System.out.println("ASDFSADFSF")
    }
      }
   

    def decodeBIO[T,L](sequences : Seq[LabeledTokenSeq[T,L]]) : Unit
    {
      
    }

    def decodeBIO[T,L](sequence : LabeledTokenSeq[T,L])
    {
      
    }
  }
*/
}

