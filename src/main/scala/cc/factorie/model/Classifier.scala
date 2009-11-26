package cc.factorie.model

import cc.factorie._

class ClassifierModel extends Model {
	class Label(name: String, val instance: Instance) extends LabelVariable(name)
	class Instance(labelString: String, features: Iterable[String]) extends BinaryVectorVariable[String] {
		this ++= features
		var label: Label = new Label(labelString, this)
	}

	/**Bias term just on labels */
	class LabelTemplate extends TemplateWithDotStatistics1[Label] 

	/**Factor between label and observed document */
	class LabelInstanceTemplate extends TemplateWithDotStatistics2[Label, Instance] {
		def unroll1(label: Label) = Factor(label, label.instance)
		def unroll2(instance: Instance) = throw new Error("Instance VectorVariable shouldn't change")
	}
}
