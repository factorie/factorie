package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging}
import cc.factorie.util.Implicits._

trait Diffs requires Model {

	// TODO consider adding state that throws error if users try to "undo" or "redo" twice in a row.

	/**A change record for a variable, holding its old and new values */
	trait Diff {
		def variable: Variable
		def redo: Unit
		def undo: Unit
	}

	// TODO think about whether or not this is a good idea...
	//abstract class CreationDiff(implicit d:DiffList) extends Diff { var done = true }

	abstract class AutoDiff(implicit d: DiffList) extends Diff {
		if (d != null) d += this
		redo
		override def toString = this.getClass.toString
	}

 
 
	/**The default DiffList is null, and therefore calls to
	Variable.set that don't set a DiffList do not accumulate Diffs */
	//implicit val nullDiffList : DiffList = null

	/**A collection of changes to variables; the result of a "jump" in configuration */
	class DiffList extends ArrayBuffer[Diff] with ConsoleLogging {
		def redo: Unit = this.foreach(d => d.redo)
		def undo: Unit = this.reverse.foreach(d => d.undo)
		/**Return the sum of the trueScore's of all the changed variables. */
		def trueScore: Double = {
			//println("calculating true score of " + this);
			var sum = 0.0
			for (d <- this) {
				//println("variable " + d.variable)
				if (d.variable != null) {
					val s = d.variable.trueScore
					sum += s
				}
			}
			sum
			//this.sum(d => {println(d.variable); if (d.variable != null) d.variable.trueScore else 0.0})
		}

		/**Return the sum of scores of all factors that touch changed variables. */
		def factors: Iterable[Factor] = modelTemplates.flatMap(template => template.factors(this))

		def factorsOf[T <: Template](implicit m: Manifest[T]): Iterable[T#Factor] = modelTemplatesOf(m).flatMap(template => template.factors(this))

		def factorsFiltered(test: Template => Boolean): Iterable[Factor] =
			modelTemplates.filter(test).flatMap(template => template.factors(this))

		def scoreFactors: Iterable[Factor] = modelScoreTemplates.flatMap(_ factors (this))

		/**Gather the uniq'ed "neighbor invocations" of each factor and return the sum of their scores */
		//def modelScore : Double = scoreFactors.sum(_ score)
		def modelScore: Double = {
			//      var sum = 0.0
			//			for (template <- modelScoreTemplates){
			//				println("Template: " + template)
			//				for (factor <- template.factors(this)){
			//					println("Factor score of " + factor + ":" + factor.score)
			//
			//				}
			//			}
			val s = modelScoreTemplates.map(_ factors(this)).foldLeft(0.0)((total, fs) => total + fs.sum(_ score))
			//scoreFactors.sum(_ score)
			s
		}

		/**Return the sum of scores of all factors that touch changed variables, and whose Template's pass the test. */
		def modelScoreFilter(test: (Template) => Boolean): Double =
			modelScoreTemplates.filter(test).flatMap(_ factors (this)).sum(_ score)

		def scoreAndUndo: Double = {
			var s = modelScore
			//println("Score: " + s)
			log(Log.DEBUG)("DiffList scoreAndUndo  pre-undo score=" + s)
			this.undo
			// We need to re-calculate the Factors list because the structure may have changed
			s -= modelScore
			log(Log.DEBUG)("DiffList scoreAndUndo post-undo score=" + s)
			s
		}
	}
 
 
}
