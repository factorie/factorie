package cc.factorie.variable

import scala.collection.mutable.{LinkedHashMap, HashMap}

/**
 * @author John Sullivan
 */
class BagOfWords(initialWords: Iterable[String] = null, initialBag: Map[String, Double] = null) {
  var variable: BagOfWordsVariable = null
  protected var _l2Norm = 0.0
  protected var _l1Norm = 0.0
  protected var _bag = new LinkedHashMap[String, Double]
  def clear(): Unit = {
    _l2Norm = 0.0
    _l1Norm = 0.0
    _bag = new LinkedHashMap[String, Double]
  }
  def cosineSimilarity(that: BagOfWords, deduct: BagOfWords): Double = {
    val numerator: Double = this.deductedDot(that, deduct)
    if (numerator != 0.0) {
      val thatL2Norm = Math.sqrt(deduct.l2Norm * deduct.l2Norm + that.l2Norm * that.l2Norm - 2 * (deduct * that))
      val denominator: Double = this.l2Norm * thatL2Norm
      if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
    } else 0.0
  }
  def cosineSimilarity(that: BagOfWords): Double = {
    val numerator: Double = this * that
    val denominator: Double = this.l2Norm * that.l2Norm
    if (denominator == 0.0 || denominator != denominator) 0.0 else numerator / denominator
  }
  def ++=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this +=(s, w)
  def --=(that: BagOfWords): Unit = for ((s, w) <- that.iterator) this -=(s, w)

  def sizeHint(n: Int) = _bag.sizeHint(n)
  if (initialWords != null) for (w <- initialWords) this +=(w, 1.0)
  if (initialBag != null) for ((k, v) <- initialBag) this +=(k, v)
  def l2Norm = scala.math.sqrt(_l2Norm)
  def l1Norm = _l1Norm
  def asHashMap: HashMap[String, Double] = {
    val result = new HashMap[String, Double]; result ++= _bag; result
  }
  override def toString = _bag.toString()
  def apply(s: String): Double = _bag.getOrElse(s, 0.0)
  def contains(s: String): Boolean = _bag.contains(s)
  def size = _bag.size
  def iterator = _bag.iterator
  def *(that: BagOfWords): Double = {
    if (that.size < this.size) return that * this
    var result = 0.0
    for ((k, v) <- iterator) result += v * that(k)
    result
  }
  def deductedDot(that: BagOfWords, deduct: BagOfWords): Double = {
    var result = 0.0
    if (deduct eq this) for ((k, v) <- iterator) result += v * (that(k) - v)
    else for ((k, v) <- iterator) result += v * (that(k) - deduct(k))
    result
  }
  def +=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      //if(w!=1.0)println("  add: "+w)
      _l1Norm += w
      _l2Norm += w * w + 2 * this(s) * w
      _bag(s) = _bag.getOrElse(s, 0.0) + w
    }
  }
  def -=(s: String, w: Double = 1.0): Unit = {
    if (w != 0.0) {
      _l1Norm -= w
      _l2Norm += w * w - 2.0 * this(s) * w
      //if(w!=1.0)println("  remove: "+w)
      if (withinEpsilon(w, _bag(s))) _bag.remove(s)
      else _bag(s) = _bag.getOrElse(s, 0.0) - w
    }
  }
  @inline final def withinEpsilon(v1: Double, v2: Double, epsilon: Double = 0.000001): Boolean = if (v1 == v2) true else math.abs(v1 - v2) <= epsilon
  def addBag(that: BagOfWords) = {
    for ((k, v) <- that.iterator) this +=(k, v)
  }
  def removeBag(that: BagOfWords) = for ((k, v) <- that.iterator) this -=(k, v)
}

class BagOfWordsVariable(initialWords: Iterable[String] = Nil, initialMap: Map[String, Double] = null) extends Var with Iterable[(String, Double)] {
  // Note that the returned value is not immutable.
  type Value = BagOfWords
  def value = _members
  def clear() = _members.clear
  protected val _members: BagOfWords = {
    val result = new BagOfWords(initialWords)
    if (initialMap != null) for ((k, v) <- initialMap) result +=(k, v)
    result.variable = this
    result
  }
  def members: BagOfWords = _members
  def iterator = _members.iterator
  override def size = _members.size
  def contains(x: String) = _members.contains(x)
  def accept(): Unit = {}
  def add(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddStringDiff(x, w)
    _members +=(x, w)
  }
  def remove(x: String, w: Double = 1.0)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveStringDiff(x, w)
    _members -=(x, w)
  }
  def add(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableAddBagDiff(x)
    _members.addBag(x)
  }
  def remove(x: BagOfWords)(implicit d: DiffList): Unit = {
    if (d != null) d += new BagOfWordsVariableRemoveBagDiff(x)
    _members.removeBag(x)
  }
  final def +=(x: String, w: Double = 1.0): Unit = add(x, w)(null)
  final def -=(x: String, w: Double = 1.0): Unit = remove(x, w)(null)
  final def +=(x: BagOfWords): Unit = add(x)(null)
  final def -=(x: BagOfWords): Unit = remove(x)(null)
  final def ++=(xs: Iterable[String]): Unit = xs.foreach(add(_)(null))
  final def --=(xs: Iterable[String]): Unit = xs.foreach(remove(_)(null))
  final def ++=(xs: HashMap[String, Double]): Unit = for ((k, v) <- xs) add(k, v)(null)
  final def --=(xs: HashMap[String, Double]): Unit = for ((k, v) <- xs) remove(k, v)(null)

  def ++(that:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
    val n = new BagOfWordsVariable()
    this.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    that.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    n
  }

  def --(that:BagOfWordsVariable)(implicit d:DiffList):BagOfWordsVariable = {
    val n = new BagOfWordsVariable()
    this.foreach{ case(word, value) =>
      n.add(word, value)(d)
    }
    that.foreach{ case(word, value) =>
      n.remove(word,value)(d)
    }
    n
  }


  case class BagOfWordsVariableAddStringDiff(added: String, w: Double) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members +=(added, w)
    def undo() = _members -=(added, w)
    override def toString = "BagOfWordsVariableAddStringDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveStringDiff(removed: String, w: Double) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members -=(removed, w)
    def undo() = _members +=(removed, w)
    override def toString = "BagOfWordsVariableRemoveStringDiff of " + removed + " from " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableAddBagDiff(added: BagOfWords) extends Diff {
    // Console.println ("new SetVariableAddDiff added="+added)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.addBag(added)
    def undo() = _members.removeBag(added)
    override def toString = "BagOfWordsVariableAddBagDiff of " + added + " to " + BagOfWordsVariable.this
  }
  case class BagOfWordsVariableRemoveBagDiff(removed: BagOfWords) extends Diff {
    //        Console.println ("new SetVariableRemoveDiff removed="+removed)
    def variable: BagOfWordsVariable = BagOfWordsVariable.this
    def redo() = _members.removeBag(removed)
    def undo() = _members.addBag(removed)
    override def toString = "BagOfWordsVariableRemoveBagDiff of " + removed + " from " + BagOfWordsVariable.this
  }
}