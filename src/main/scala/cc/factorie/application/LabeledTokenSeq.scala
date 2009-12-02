package cc.factorie.application
import cc.factorie.er._
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import scala.util.Sorting

/** Predefined variables and factor templates for applying FACTORIE to sequences of Tokens, each paired with a categorical Label.
    The Token remembers its String 'word', but its variable 'value' is as a BinaryVectorVariable.
    This package also provides Getters for Tokens and Labels, enabling template building with the tools in cc.factorie.er.
    For exmaple usage see cc.factorie.example.ChainNER1 */
object LabeledTokenSeqs {
    
  /** A word token in a linear sequence of tokens.  It is a constituent of a LabeledTokenSeq.
      Its value is a BinaryVectorVariable, its feature vector.
      It provides access to its neighbors in the sequence and its label.  It also has an entity-relationship counterpart. */
  final class Token(val word:String, features:Seq[String], labelString:String) extends BinaryVectorVariable[String] with VarInSeq[Token] with Entity[Token] {
    type GetterType = TokenGetter
    class GetterClass extends TokenGetter
    val label: Label = new Label(labelString, this)
    /** Return true if the first  character of the word is upper case. */
    def isCapitalized = java.lang.Character.isUpperCase(word(0))
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
    private lazy val svmap = new HashMap[String,BinaryVectorVariable[String]]
    def subVector(regex:String): BinaryVectorVariable[String] = svmap.getOrElseUpdate(regex, newSubVector(regex))
    private def newSubVector(regex:String): BinaryVectorVariable[String] = {
      val result = new BinaryVectorVariable[String] { override def printName = "TokenSubVector" }
      result ++= this.values.filter(s => s.matches(regex))
      result
    }
    this ++= features
  }
  
  /** Implementation of the entity-relationship language we can use with Token objects. */
  class TokenGetter extends EntityGetter[Token] {
  	/** Go from a token to its label. */
    def label = getOneToOne[Label](
      token=>token.label, 
      label => label.token)
    /** Go from a token to the next token. */
    def next = getManyToMany[Token](
      (token:Token) => if (!token.hasNext) Nil else List(token.next), 
      (token:Token) => if (!token.hasPrev) Nil else List(token.prev))
    /** Go from a token to the previous token. */
    def prev = getManyToMany[Token](
      (token:Token) => if (!token.hasPrev) Nil else List(token.prev), 
      (token:Token) => if (!token.hasNext) Nil else List(token.next))
    /** Go from a token to the collection of the next 'n' tokens. */
    def next(n:Int) = getManyToMany[Token](
    	(t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret },
      (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret })
    /** Go from a token to the collection of the previous 'n' tokens. */
    def prev(n:Int) = getManyToMany[Token](
    	(t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret },
      (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret })
    /** All the other tokens in the Sentence. */
    def sentenceTokens = getManyToMany[Token](
      (token:Token) => token.seq, 
      (token:Token) => token.seq)
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
    def subVector(regex:String) = getOneWay(t=>t.subVector(regex))
  }

  /** A Label associated with a Token. */
  // NOTE: If you remove final, add a comment warning the user that different subclasses of will share the same Domain.
  // I don't think we should allow subclassing, hence the "final". -akm
  final class Label(labelname: String, val token: Token) extends LabelVariable(labelname) with Entity[Label] {
    type GetterType = LabelGetter
    class GetterClass extends LabelGetter
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next = token.next.label
    def prev = token.prev.label
  }
  
  // Define boilerplate, to support access to attributes in the entity-attribute-relationship syntax
  class LabelGetter extends EntityGetter[Label] {
    def token = getOneToOne[Token](label => label.token, token => token.label)
    def next = getManyToMany[Label](
      label => if (!label.token.hasNext) Nil else List(label.token.next.label),
      label => if (!label.token.hasPrev) Nil else List(label.token.prev.label))
    def prev = getManyToMany[Label](
      label => if (!label.token.hasPrev) Nil else List(label.token.prev.label),
      label => if (!label.token.hasNext) Nil else List(label.token.next.label))
  }
  
 
  // Companion object is below.
  class LabeledTokenSeq extends VariableSeq[Token] {
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
      // First gather all the extra features here, then add them to each Token
      val newFeatures = Array.fromFunction(i => new ArrayBuffer[String])(this.size)
      for (i <- 0 until size) {
        val token = this(i)
        val thisTokenNewFeatures = newFeatures(i)
        for (offsets <- offsetConjunctions) 
          thisTokenNewFeatures ++= appendConjunctions(regex, token, null, offsets)
      }
      for (i <- 0 until size) {
      	val token = this(i)
      	token.zero
        token ++= newFeatures(i)
      }
      //if (size > 0) println("addNeighboringFeatureConjunctions "+first)
    }
    // Recursive helper function for previous method, expanding out cross-product of conjunctions in tree-like fashion.
    // 't' is the Token to which we are adding features; 'existing' is the list of features already added; 'offsets' is the list of offsets yet to be added
    private def appendConjunctions(regex:String, t:Token, existing:ArrayBuffer[String], offsets:Seq[Int]): ArrayBuffer[String] = {
      val result = new ArrayBuffer[String];
      val offset: Int = offsets.first
      val t2 = t.next(offset)
      val adding: Seq[String] = 
        if (t2 == null) { if (t.position + offset < 0) List("<START>") else List("<END>") }
        else if (regex != null) t2.values.filter(str => str.matches(regex)) // Only include features that match pattern 
        else t2.values
      if (existing != null) {
        if (offset == 0)
          for (e <- existing; a <- adding) result += e+"_&_"+a
        else
          for (e <- existing; a <- adding) result += e+"_&_"+a+"@"+offset
      } else {
        if (offset == 0)
        	for (a <- adding) result += a
        else
        	for (a <- adding) result += a+"@"+offset
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
  	def fromSGML(source:Source, backgroundLabelString:String, lexer:Regex): LabeledTokenSeq = {
  			val words = lexer.findAllIn(source.mkString)
  			throw new Error("Not implemented yet.")
  	}

  	/** Construct and return a new LabeledTokenSeq (and its constituent Tokens and Labels) 
  			from a source containing plain text.  Since the labels are unknown, all Labels
  			will be given the initial and true value 'defaultLabelString'. */
  	def fromPlainText(source:Source, defaultLabelString:String, featureFunction: String=>Seq[String], lexer:Regex): LabeledTokenSeq = {
  			val seq = new LabeledTokenSeq
  			lexer.findAllIn(source.mkString).foreach(word => seq += new Token(word, featureFunction(word), defaultLabelString))
  			seq
  	}

  	/** Create a LabeledTokenSeq from a source of characters that has "one word per line", 
  			each line consisting of information about one token: a whitespace-separated list of elements, 
  			in which the first element is the word itself and the last element is the true target label for the token.
  			The CoNLL 2003 NER Shared Task is an example of such a format.
  			Token.word will be set to the first element.
  			All elements but the last will be passed to to 'featureFunction', 
  			and its returned strings will be added as features to the BinaryVectorVariable.
  			The initial and trueValue of the Label will be set from the last element.
  			If ignoreLines is non-null, we skip any lines containing this pattern, for example pass "-DOCSTART-" for CoNLL 2003.
  			If seqSeparator is null, we separate sequences by lines consisting only of carriage return. */
    def fromOWPL(source:Source, featureFunction:Seq[String]=>Seq[String], documentBoundary:Regex, sentenceBoundary:Regex, ignoreLines:Regex): Seq[LabeledTokenSeq] = {
  		import scala.collection.mutable.ArrayBuffer
  		var tokenCount = 0
  		var seqs = new ArrayBuffer[LabeledTokenSeq];
  		var seq = new LabeledTokenSeq
  		for (line <- source.getLines) {
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
  				assert(fields.length == 4)
  				val word = fields(0)
  				val inFeatures = fields.slice(0, fields.length-1).force
  				val pos = fields(1)
  				val label = fields.last.stripLineEnd
  				seq += new Token(word, featureFunction(inFeatures), label)
  				tokenCount += 1
  			}
  		}
  		//println("Loaded "+seqs.length+" sentences with "+wordCount+" words total from file "+filename)
  		seqs
  	}
    // TODO Waiting for Scala 2.8 default parameter values
    def fromOWPL(source:Source, featureFunction:Seq[String]=>Seq[String], documentBoundary:Regex): Seq[LabeledTokenSeq] = fromOWPL(source, featureFunction, documentBoundary, "\\A\\s*\\z".r, null)
    def fromOWPL(source:Source, documentBoundary:Regex): Seq[LabeledTokenSeq] = fromOWPL(source, (f:Seq[String]) => f, documentBoundary)
    def fromOWPL(source:Source, documentBoundary:String): Seq[LabeledTokenSeq] = fromOWPL(source, (f:Seq[String]) => f, documentBoundary.r)

    
    class PerLabelEvaluation(val labelValue: String) {
    	var fp = 0
    	var fn = 0
    	var tp = 0
    	private val targetIndex = Domain[Label].index(labelValue)

    	def ++=(seqs:Seq[LabeledTokenSeq]) = seqs.foreach(+= _)
    	def +=(seq:LabeledTokenSeq): Unit = +=(seq.map(_.label))
    	def +=(labels: Seq[Label]): Unit = {
    		for (l <- labels) {
    			val trueIndex = l.trueIndex
    			val predIndex = l.index
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

    class LabelEvaluation(val backgroundLabelValue:String) {
      import scala.collection.mutable.HashMap
      def this(labels:Seq[Label], bg:String) = { this(bg); this.+=(labels) }
      def this(labels:Seq[Label]) = { this("O"); this.+=(labels) }
      //def this(labels:Seq[LabeledTokenSeq]) = { this("O"); this.+=(labels.flatMap(_.labels)) }
    	var fp = 0
    	var fn = 0
    	var tp = 0
    	//println("Evaluation Labels: "+Domain[Label].toList)
    	private val labelEval = 
    		new HashMap[String,PerLabelEvaluation] ++ Domain[Label].map(labelString => (labelString, new PerLabelEvaluation(labelString)))
    	/** Return the LabelEvaluation specific to labelString. */
    	def apply(labelString:String) = labelEval(labelString)
    	def +=(labels: Seq[Label]): Unit = {
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
    def labelEvaluation(labels:Seq[Label]) = new LabelEvaluation(labels)
    
    /** Evalute in terms of correct entire segments.  
        The field start and end boundaries must be perfect to count as correct.  No partial credit. 
        For example, this is the standard for results on CoNLL 2003. */
    class PerSegmentEvaluation(val labelName:String, val labelValueStart: Regex, val labelValueContinue: Regex) {
      //println(labelName); println(labelValueStart); println(labelValueContinue); println
      //if (labelValueContinue == null) labelValueContinue = labelValueStart // Waiting for Scala 2.8 default parameters
      var trueCount, predictedCount, correctCount = 0 // per segment
      var labelCount, correctLabelCount = 0 // per label, included here just because it is easy
      var predictedStart, trueStart = false
      def ++=(seqs:Seq[LabeledTokenSeq]) = seqs.foreach(+= _)
      def +=(seq:LabeledTokenSeq): Unit = +=(seq.map(_.label))
      /** Add the given sequence of labels to the statistics for this evalution.
          Even though you may be tempted to put all Label instances across all sentences in a single Seq[] and pass them in here, 
          note that you risk getting slightly incorrect results at document boundaries: when one document ends 
          in a mention and the next document begins with the same mention type, 
          they will be counted as only one mention, when they should have been counted as two. */
      def +=(labels: Seq[Label]): Unit = {
        for (position <- 0 until labels.length) {
          val label = labels(position)
          //print("\n"+label.trueValue+"/"+label.value+" ")
          labelCount += 1
          if (label.valueIsTruth) correctLabelCount += 1
          predictedStart = false; trueStart = false
          if ((!label.hasPrev || label.prev.index != label.index) && labelValueStart.findAllIn(label.value).hasNext) { // TODO is there a more canonical way to ask if a regex matches a string?
            predictedCount += 1
            predictedStart = true
            //print("ps ")
          }
          if ((!label.hasPrev || label.prev.trueIndex != label.trueIndex) && labelValueStart.findAllIn(label.trueValue.toString).hasNext) {
            trueCount += 1
            trueStart = true
            //print("ts ")
          }
          // Truth and prediction both agree that a segment is starting here, let's see if they end in the same place
          if (predictedStart && trueStart) {
            //print(" pts ")
            var predictedContinue, trueContinue = false
            var j = position + 1
            var stopSearchForSegmentEnd = false
            while (j < labels.length && !stopSearchForSegmentEnd) {
              val label2 = labels(j)
              predictedContinue = labelValueContinue.findAllIn(label2.value).hasNext
              trueContinue = labelValueContinue.findAllIn(label2.trueValue.toString).hasNext
              //print("j="+j+predictedContinue+trueContinue)
              if (!predictedContinue || !trueContinue) {
                if (predictedContinue == trueContinue) correctCount += 1 // Both sequences ended at the same position: correct
                stopSearchForSegmentEnd = true
              }
              j += 1
            }
            // Handle special case for the end of the sequence
            if (j == labels.length && predictedContinue == trueContinue) trueCount += 1
          }
        }
      }
      def tokenAccuracy = correctLabelCount.toDouble / labelCount
      def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
      def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
      def f1 = if (recall+precision == 0.0) 0.0 else (2.0 * recall * precision) / (recall + precision)
      def alarmCount = predictedCount - correctCount
      def missCount = trueCount - correctCount
      def tp = correctCount
      def fn = missCount
      def fp = alarmCount
      override def toString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format(labelName, f1, precision, recall, tp, fp, fn, trueCount, predictedCount) 
    }
    
    // Some utilities for automatically filling in values 
    private val defaultStartPrefix = "B-" // "B|I-" needed for IOB
    private val defaultContinuePrefix = "I-"
    // Assume that the first two characters of each label are the "B-" or "I-" prefix.  Skip the label "O" because it is less than 3 chars long
    private def labelStringsToBase(labelVals:Seq[String]): Seq[String] = {
      val result = new HashSet[String]
      labelVals.foreach(s => if (s.length > 2) result += s.substring(2))
      result.toSeq
    }
    
    class SegmentEvaluation(baseLabelStrings: Seq[String], startPrefix:String, continuePrefix:String) {
      def this() = this(labelStringsToBase(Domain[Label].toSeq), defaultStartPrefix, defaultContinuePrefix)
      def this(labels:Seq[Label]) = { this(); this.+=(labels) }
      private val evals = new HashMap[String,PerSegmentEvaluation]
      evals ++ baseLabelStrings.map(s => (s, new PerSegmentEvaluation(s, (startPrefix+s).r, (continuePrefix+s).r)))
      /** Return the LabelEvaluation specific to labelString. */
      def apply(labelString:String) = evals(labelString)
      def +=(labels: Seq[Label]): Unit =
        evals.values.foreach(eval => eval += labels)
      def correctCount = evals.values.foldLeft(0)(_+_.correctCount)
      def predictedCount = evals.values.foldLeft(0)(_+_.predictedCount)
      def trueCount = evals.values.foldLeft(0)(_+_.trueCount)
      def tokenAccuracy = evals.values.foldLeft(0)(_+_.correctLabelCount).toDouble / evals.values.foldLeft(0)(_+_.labelCount)
      def precision = if (predictedCount == 0) 1.0 else correctCount.toDouble / predictedCount
      def recall = if (trueCount == 0) 1.0 else correctCount.toDouble / trueCount
      def f1: Double = if (precision + recall == 0.0) 0.0 else 2.0 * precision * recall / (precision + recall)
      def alarmCount = predictedCount - correctCount
      def missCount = trueCount - correctCount
      def tp = correctCount
      def fn = missCount
      def fp = alarmCount
      def summaryString = "%-8s f1=%-8f p=%-8f r=%-8f (tp=%d fp=%d fn=%d true=%d pred=%d)".format("OVERALL", f1, precision, recall, tp, fp, fn, tp+fn, tp+fp)
      override def toString = {
        val sb = new StringBuffer
        sb.append("ACCURACY "+tokenAccuracy+" ("+evals.values.foldLeft(0)(_+_.correctLabelCount)+"/"+evals.values.foldLeft(0)(_+_.labelCount)+")")
        sb.append("\n")
        sb.append(summaryString)
        sb.append("\n")
        evals.values.foreach(e => { sb.append(e.toString); sb.append("\n") })
        sb.toString
      } 
    }

    // TODO Add more combinations of arguments
    def segmentEvaluation(labels:Seq[Label]) = new SegmentEvaluation(labels)

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

}

/** By making this an object also, we support inclusion by "import cc.factorie.application.LabeledTokenSeq._" */
//object LabeledTokenSeqs extends LabeledTokenSeqs {}
