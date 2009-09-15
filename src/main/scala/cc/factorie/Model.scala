package cc.factorie

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer, FlatHashTable}
import scala.reflect.Manifest
import scala.util.Random
import scala.Math
import scala.util.Sorting
import scalala.tensor.Vector
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.{SparseVector, SparseBinaryVector, SingletonBinaryVector}
import cc.factorie.util.{Log, ConsoleLogging, LinkedHashSet}
import cc.factorie.util.Implicits._

class Model(templates:Iterable[Template]) extends TemplateList[Template] {
  def this() = this(Nil)
  def this(templates:Template*) = this(templates)
  this ++= templates

	// Random number generator specific to the Model
	//def randomSeed = 0
	//implicit lazy val random: Random = if (randomSeed < 0) new Random() else new Random(randomSeed)
 
  /** Collection of templates that score the model according to model parameters. */
 	//val modelTemplates = new TemplateList[Template]
  /** Collection of templates that score the model according to distance to the truth. */
  //val truthTemplates = new TemplateList[Template]
  /** Collection of templates that check assertions, for debugging. */
  //val checkTemplates = new TemplateList[Template]
  
	// Helper functions for assessing the model state
	//def variablesTrueScore(vars: Iterable[Variable]): Double = truthTemplates.score(vars)
	//def variablesAccuracy(vars: Collection[Variable]): Double = variablesTrueScore(vars) / vars.size

 
}