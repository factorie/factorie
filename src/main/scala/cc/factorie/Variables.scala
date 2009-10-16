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

	/**A variable whose value is a set of other variables */
	abstract class SetVariable[X]() extends Variable with TypedVariable {
		type ValueType = X
		type VariableType <: SetVariable[X]
		private val _members = new HashSet[X]
		def members: scala.collection.Set[X] = _members
		def size = _members.size
		def contains(x:X) = _members.contains(x)
		def add(x: X)(implicit d: DiffList): Unit = if (!_members.contains(x)) {
			if (d != null) d += new SetVariableAddDiff(x)
			_members += x
		}
		def remove(x: X)(implicit d: DiffList): Unit = if (_members.contains(x)) {
			if (d != null) d += new SetVariableRemoveDiff(x)
			_members -= x
		}
		case class SetVariableAddDiff(added: X) extends Diff {
			//        Console.println ("new SetVariableAddDiff added="+added)
			def variable: SetVariable[X] = SetVariable.this
			def redo = _members += added //if (_members.contains(added)) throw new Error else
			def undo = _members -= added
		}
		case class SetVariableRemoveDiff(removed: X) extends Diff {
			//        Console.println ("new SetVariableRemoveDiff removed="+removed)
			def variable: SetVariable[X] = SetVariable.this
			def redo = _members -= removed
			def undo = _members += removed //if (_members.contains(removed)) throw new Error else
		}
	}

