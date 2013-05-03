package cc.factorie.la

/**
 * User: apassos
 * Date: 5/1/13
 * Time: 1:40 PM
 */

class Tensors extends Map[Any,Tensor] {
  self =>
  protected val _map = new scala.collection.mutable.LinkedHashMap[Any,Tensor] {
    override def default(f:Any) = { val t = defaultTensor(f); this(f) = t; t }
  }
  def get(key:Any): Option[Tensor] = Some(_map(key))
  def iterator = _map.iterator
  def +[B>:Tensor](other:(Any,B)): Map[Any,B] = throw new Error("Tensors are not composable")

  def defaultTensor(key:Any): Tensor = throw new Error("Key not found.")
  def -(key:Any) = throw new Error("Tensors does not support key removal.")
  def -(other: Tensors) = { val newT = copy; newT += (other,-1); newT }
  def zero(): Unit = _map.values.foreach(_.zero())
  def +=(w:Tensors, f:Double): Unit = { w.keys.foreach(k => this(k) += (w(k),f))}
  def +=(w:Tensors): Unit = this += (w, 1.0)
  def dot(w:Tensors): Double = w.keys.map(k => w(k).dot(this(k))).sum
  def oneNorm: Double = values.map(_.oneNorm).sum
  def twoNorm: Double = math.sqrt(twoNormSquared)
  def twoNormSquared: Double = values.map(_.twoNormSquared).sum
  def different(w:Tensors, tolerance:Double): Boolean = w.keys.forall(k => this(k).different(w(k), tolerance))
  def containsNaN(): Boolean = _map.values.exists(_.containsNaN)
  def :=(other: Tensors): Unit = keys.foreach(k => this(k) := other(k))
  def *=(other: Double): Unit = keys.foreach(k => this(k) *= other)
  def copy: Tensors = {
    val copyTensor = new Tensors { override def defaultTensor(key: Any) = self.apply(key).copy }
    keys.foreach(copyTensor(_))
    copyTensor
  }
  def blankDenseCopy: Tensors = new Tensors { override def defaultTensor(key:Any) = Tensor.newDense(self(key)) }
  def blankSparseCopy: Tensors = new Tensors { override def defaultTensor(key:Any) = Tensor.newSparse(self(key)) }
  def toArray: Array[Double] = { val a = new Array[Double](_map.values.map(_.length).sum); var offset = 0; _map.values.foreach(t => { System.arraycopy(t.asArray, 0, a, offset, t.length); offset += t.length }); a }
  def append(k: Any, v: Tensor) = _map(k) = v
  def this(items: Seq[(Any,Tensor)]) = { this(); items.foreach(i => append(i._1,i._2)) }
}


