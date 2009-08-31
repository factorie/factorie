package cc.factorie.model

import factorie._
//import factorie.util.Implicits._

trait LinearChainModel extends Model {
	var feedForward = true
	var feedBackward = true

	class Label(labelname: String, val token: Token) extends super.Label(labelname)

	class Token(val word: String, features: Seq[String], labelString: String) extends VectorVariable[String] with VarInSeq[Token] {
		this ++= features
		val label: Label = if (labelString != null) new Label(labelString, this) else null
	}

	/**Bias term just on labels */
	abstract class LabelTemplate extends TemplateWithNeighbors1[Label] {
		addModelTemplates(this)
	}

	/**Factor between two successive labels */
	abstract class TransitionTemplate extends TemplateWithNeighbors2[Label, Label] {
		def unroll1(label: Label) = if (label.token.hasNext && label.token.next.label != null && feedBackward) Factor(label, label.token.next.label) else Nil

		def unroll2(label: Label) = if (label.token.hasPrev && label.token.prev.label != null && feedForward) Factor(label.token.prev.label, label) else Nil
		addModelTemplates(this)
	}

	/**Factor between label and observed token */
	abstract class LabelTokenTemplate extends TemplateWithNeighbors2[Label, Token] {
		def unroll1(label: Label) = Factor(label, label.token)

		def unroll2(token: Token) = throw new Error("Token values shouldn't change")
		addModelTemplates(this)
	}

	/**A token bi-gram conjunction  */
	abstract class LabelBiTokenTemplate extends TemplateWithNeighbors3[Label, Token, Token] {
		def unroll1(label: Label) = if (label.token.hasPrev) Factor(label, label.token, label.token.prev) else Nil

		def unroll2(token: Token) = throw new Error("Token values shouldn't change")

		def unroll3(token: Token) = throw new Error("Token values shouldn't change")
		addModelTemplates(this)
	}

}
