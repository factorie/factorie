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

trait Model extends Domains with Variables with Proposals with Templates with Diffs {

	// Random number generator specific to the Model
	def randomSeed = 0
	implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
 
	/* Replaced by sample() method in cc.factorie.util.Implicits converstion of Iterable 
 	def randomVariable[V](vars: RandomAccessSeq[V]): V = vars(random.nextInt(vars.size));
	def randomVariable[V](vars: RandomAccessSeq[V], test: V => boolean): V = {
		val filteredVars = vars.filter(test)
		filteredVars(random.nextInt(filteredVars.size))
	}*/

  // The collection of Domains of Variables within the Model	
 	val modelDomains = new HashSet[Domain[_ <: Variable]]()

 	// Management of Factor Templates within the Model
  // TODO AKM: All this cruft really annoys me, but it was added for speed in DiffList.modelScore.  Does it really take so much time to post-filter Templates?
	var _modelTemplates: List[Template] = Nil
	var _modelScoreTemplates: List[Template] = Nil
	def modelTemplates = _modelTemplates
	def addModelTemplate(temp: Template): Unit = addModelTemplates(List(temp))
	def addModelTemplates(temps: Template*): Unit = addModelTemplates(temps)
	def addModelTemplates(temps: Collection[Template]): Unit = {
		for (temp <- temps; if (!_modelTemplates.contains(temp)))
			_modelTemplates = temp :: _modelTemplates
		_modelScoreTemplates = _modelTemplates.filter(!_.isInstanceOf[NoScore]).toList
	}
	def clearModelTemplates = _modelTemplates = Nil
	def modelTemplatesOf[T <: Template](implicit m: Manifest[T]): Iterable[T] = {
		for (t <- modelTemplates; if (m.erasure.isAssignableFrom(t.getClass))) yield t.asInstanceOf[T]
	}
	def modelScoreTemplates = _modelScoreTemplates
	def modelMarginalSamplesTemplates = _modelTemplates.filter(!_.isInstanceOf[MarginalSamples])

	// Helper functions for assessing the model state
	def worldTrueScore(vars: Iterable[Variable]): Double = vars.sum(_ trueScore)
	def worldAccuracy(vars: Collection[Variable]): Double = worldTrueScore(vars) / vars.size

}