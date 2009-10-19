package cc.factorie.model

import factorie._

trait LinearChainModel extends Model {
	var feedForward = true
	var feedBackward = true

	class Label(labelname: String, val token: Token) extends cc.factorie.Label(labelname) {
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
	abstract class LabelTemplate extends TemplateWithExpStatistics1[Label] 

	/** Factor between two successive labels */
	abstract class TransitionTemplate extends TemplateWithExpStatistics2[Label, Label] {
		def unroll1(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label) else Nil
		def unroll2(label: Label) = if (label.hasNext) Factor(label, label.token.next.label) else Nil
	}

	/** Factor between label and observed token */
	abstract class LabelTokenTemplate extends TemplateWithExpStatistics2[Label, Token] {
		def unroll1(label: Label) = Factor(label, label.token)
		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
	}
 
  /** Factor between label, its token and the previous Label */
	abstract class TransitionTokenTemplate extends TemplateWithExpStatistics3[Label, Label, Token] {
		def unroll1(label: Label) = if (label.hasNext) Factor(label, label.token.next.label, label.token.next) else Nil
		def unroll2(label: Label) = if (label.hasPrev) Factor(label.token.prev.label, label, label.token) else Nil
		def unroll3(token: Token) = throw new Error("Token values shouldn't change")
	}

	/** A token bi-gram conjunction  */
	abstract class LabelBiTokenTemplate extends TemplateWithExpStatistics3[Label, Token, Token] {
		def unroll1(label: Label) = if (label.token.hasPrev) Factor(label, label.token, label.token.prev) else Nil
		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
	}

}
