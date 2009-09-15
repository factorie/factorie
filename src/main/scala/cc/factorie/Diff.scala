package cc.factorie

import scala.collection.mutable.{ArrayBuffer}
//import scala.reflect.Manifest
//import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
//import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
//import cc.factorie.util.Implicits._

// Diffs

// TODO consider adding state that throws error if users try to "undo" or "redo" twice in a row.
/**A change record for a variable, holding its old and new values */
trait Diff {
	def variable: Variable
	def redo: Unit
	def undo: Unit
}

abstract class AutoDiff(implicit d:DiffList) extends Diff {
	if (d != null) d += this
	redo
	override def toString = this.getClass.toString
}

/**The default DiffList is null, and therefore calls to
Variable.set that don't set a DiffList do not accumulate Diffs */
//implicit val nullDiffList : DiffList = null
 
/**A collection of changes to variables; the result of a "jump" in configuration */
class DiffList extends ArrayBuffer[Diff] {
	def redo: Unit = this.foreach(d => d.redo)
	def undo: Unit = this.reverse.foreach(d => d.undo)
	// def factors(templates:TemplateList[_]) : Iterable[Factor] 
	/**Return the sum of the trueScore's of all the changed variables. */
	//def trueScore(model:Model): Double = if (this.length == 0) 0.0 else model.truthTemplates.score(this)
	def score(model:Model) = model.score(this) // TODO Should we provide this kind of syntax reversal, or only provide "one" way to do things?
	def scoreAndUndo(model:Model): Double = {
		if (this.length == 0) return 0.0  // short-cut the simple case
		var s = model.score(this)
		//println("Score: " + s)
		//log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
		this.undo
		// We need to re-calculate the Factors list because the structure may have changed
		s -= model.score(this)
		//log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
		s
	}
	def scoreAndUndo(model1:Model, model2:Model) : (Double, Double) = {
		var s1 = model1.score(this)
		var s2 = model2.score(this)
		this.undo
		s1 -= model1.score(this)
		s2 -= model2.score(this)
		(s1, s2)
	}
}
