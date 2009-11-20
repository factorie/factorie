package cc.factorie

// Consider putting this in a cc.factorie.nlp package directory?

/** Defines Variable classes useful for typically NLP, with conveniet er2-style entity-attribute-relationship access. */
  
object nlp {
  //import cc.factorie.er2._
  
  /** A word token in a linear sequence of tokens.  It is a constituent of a Sentence.  
      It provides access to its attributes through the er2-style entity-attribute-relationship language. */
  class Token(val word:String, features:Seq[String], labelString:String) extends BinaryVectorVariable[String] with VarInSeq[Token] /*with AccessorType*/ {
  	//type AccessorType = TokenAccessor[Token,Token];
  	val label: Label = new Label(labelString, this)
    this ++= features
  }
  
  /*
  // Define boilerplate, to support access to attributes in the entity-attribute-relationship syntax
  class TokenAccessor[A,B](prefix:Accessor[A,B], forward:B=>Iterable[Token], backward:Token=>Iterable[B]) extends MultiAccessor(prefix, forward, backward) {
  	def label = new LabelAccessor(this, (t:Token)=>List(t.label), (l:Label)=>List(l.token))
    /** Go from a token to the next token. */
    def next = new TokenAccessor(this, (t:Token) => if (!t.hasNext) Nil else List(t.next), (t:Token) => if (!t.hasPrev) Nil else List(t.prev))
    /** Go from a token to the previous token. */
    def prev = new TokenAccessor(this, (t:Token) => if (!t.hasPrev) Nil else List(t.prev), (t:Token) => if (!t.hasNext) Nil else List(t.next))
    /** Go from a token to the collection of the next 'n' tokens. */
    def next(n:Int) = new TokenAccessor(this, 
        (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret },
        (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret })
    /** Go from a token to the collection of the previous 'n' tokens. */
    def prev(n:Int) = new TokenAccessor(this, 
        (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasPrev && i > 0) { ret = t.prev :: ret; i += 1}; ret },
        (t:Token) => { var i = n; var ret:List[Token] = Nil; while (t.hasNext && i > 0) { ret = t.next :: ret; i += 1}; ret })
  	/** All the other tokens in the Sentence. */
  	def sentenceTokens = new TokenAccessor(this, (t:Token) => t.seq, (t:Token) => t.seq)
  	// def isWord(w:String) // Consider how to create arbitrary Observation variables created and returned on the fly.
  	//  Only need to go in one direction, since the Observation variables never change.
  }*/

  class Label(labelname: String, val token: Token) extends cc.factorie.Label(labelname) {
    def hasNext = token.hasNext && token.next.label != null
    def hasPrev = token.hasPrev && token.prev.label != null
    def next = token.next.label
    def prev = token.prev.label
  }
  
  /*
  // Define boilerplate, to support access to attributes in the entity-attribute-relationship syntax
  class LabelAccessor[A,B](prefix:Accessor[A,B], forward:B=>Iterable[Label], backward:Label=>Iterable[B]) extends MultiAccessor(prefix, forward, backward) {
    def token = new TokenAccessor(this, (l:Label)=>List(l.token), (t:Token)=>List(t.label))
    def next = new LabelAccessor(this, (l:Label)=>List(l.next), (l:Label)=>List(l.prev))
    def prev = new LabelAccessor(this, (l:Label)=>List(l.prev), (l:Label)=>List(l.next))
  }*/
  
  
  class Sentence extends VariableSeq[Token]
  
}
