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

/** A Model in FACTORIE consists of a collection of factor Templates and methods that operate on the collection.
    Most of these methods are implemented in TemplateList.
 	  @author Andrew McCallum
    @since 0.8
    @see Template
    @see TemplateList
 */
class Model(templates:Iterable[Template]) extends TemplateList[Template] {
  def this() = this(Nil)
  def this(templates:Template*) = this(templates)

  this ++= templates
  
  def save(dirname:String): Unit = {
    import java.io.File
    println("Saving model "+getClass.getName+" to "+dirname)
    val f = new File(dirname)
    // Recursively delete all files in directory "f"
    def delete(f:File): Boolean = { if (f.isDirectory) f.listFiles.forall(f2 => delete(f2)) else f.delete }
    if (f.exists) if (!delete(f)) throw new Error("Error deleting directory "+dirname)
    f.mkdir
    this.foreach(_.save(dirname))
  }
 
  def load(dirname:String): Unit = {
    this.foreach(_.load(dirname))
  }
}

