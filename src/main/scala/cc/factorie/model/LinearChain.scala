package cc.factorie.model

import factorie._

trait LinearChainModel extends Model {
	var feedForward = true
	var feedBackward = true

	class Label(labelname: String, val token: Token) extends LabelVariable(labelname) {
	  def hasNext = this.token.hasNext && this.token.next.label != null && feedBackward
	  def hasPrev = this.token.hasPrev && this.token.prev.label != null && feedForward
	}

	class Token(val word: String, features: Seq[String], labelString: String) extends BinaryVectorVariable[String] with VarInSeq[Token] {
	  def this(w:String, ls:String) = this(w, Nil, ls)
		this ++= features
		val label: Label = if (labelString != null) new Label(labelString, this) else null
	}
 
  class Sentence extends VariableSeq[Token]
  
 
	/** Bias term just on labels */
	class LabelTemplate extends TemplateWithDotStatistics1[Label] 

	/** Factor between two successive labels */
	class TransitionTemplate extends TemplateWithDotStatistics2[Label, Label] {
		def unroll1(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label) else Nil
		def unroll2(label: Label) = if (label.hasNext) Factor(label, label.token.next.label) else Nil
	}

	/** Factor between label and observed token */
	class LabelTokenTemplate extends TemplateWithDotStatistics2[Label, Token] {
		def unroll1(label: Label) = Factor(label, label.token)
		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
	}
 
  /** Factor between label, its token and the previous Label */
	class TransitionTokenTemplate extends TemplateWithDotStatistics3[Label, Label, Token] {
		def unroll1(label: Label) = if (label.hasNext) Factor(label, label.token.next.label, label.token.next) else Nil
		def unroll2(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label, label.token) else Nil
		def unroll3(token: Token) = throw new Error("Token values shouldn't change")
	}

	/** A token bi-gram conjunction  */
	class LabelBiTokenTemplate extends TemplateWithDotStatistics3[Label, Token, Token] {
		def unroll1(label: Label) = if (label.token.hasPrev) Factor(label, label.token, label.token.prev) else Nil
		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
		def unroll3(token: Token) = throw new Error("Token values shouldn't change")
	}

}
