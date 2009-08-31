package cc.factorie.model

import cc.factorie._

class ClassifierModel extends Model {
	class Label(name: String, val instance: Instance) extends EnumVariable(name)
	class Instance(labelString: String, features: Iterable[String]) extends VectorVariable[String] {
		this ++= features
		var label: Label = new Label(labelString, this)
	}

	/**Bias term just on labels */
	class LabelTemplate extends TemplateWithNeighbors1[Label] with DenseLogLinearScoring {
		addModelTemplates(this)
	}

	/**Factor between label and observed document */
	class LabelInstanceTemplate extends TemplateWithNeighbors2[Label, Instance] with DenseLogLinearScoring {
		def unroll1(label: Label) = Factor(label, label.instance)

		def unroll2(instance: Instance) = throw new Error("Instance VectorVariable shouldn't change")
		addModelTemplates(this)
	}
}
