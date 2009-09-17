package cc.factorie.model

import cc.factorie._

class ClassifierModel extends Model {
	class Label(name: String, val instance: Instance) extends cc.factorie.Label(name)
	class Instance(labelString: String, features: Iterable[String]) extends VectorVariable[String] {
		this ++= features
		var label: Label = new Label(labelString, this)
	}

	/**Bias term just on labels */
	class LabelTemplate extends TemplateWithExpStatistics1[Label] with DenseLogLinearScoring 

	/**Factor between label and observed document */
	class LabelInstanceTemplate extends TemplateWithExpStatistics2[Label, Instance] with DenseLogLinearScoring {
		def unroll1(label: Label) = Factor(label, label.instance)
		def unroll2(instance: Instance) = throw new Error("Instance VectorVariable shouldn't change")
	}
}
