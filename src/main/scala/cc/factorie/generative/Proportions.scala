///* Copyright (C) 2008-2010 University of Massachusetts Amherst,
//   Department of Computer Science.
//   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
//   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//    http://www.apache.org/licenses/LICENSE-2.0
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License. */
//
//package cc.factorie.generative
//import cc.factorie._
//import cc.factorie.util.DoubleSeq
//
//trait ProportionGeneratingFactor extends GenerativeFactor {
//  def sampleProportions: Proportions
//  def pr(p:Proportions): Double
//  def logpr(p:Proportions): Double
//}
//
//
//trait ProportionsValue extends IndexedSeq[Double] {
//  def length: Int
//  def apply(i:Int): Double
//  def sampleInt: Int = maths.nextDiscrete(this)
//  def maxInt: Int = { 
//    var i = 0; var maxi = 0; var maxd = 0.0
//    while (i < length) { 
//      if (this(i) > maxd) { maxd = this.apply(i); maxi = i }
//      i += 1 
//    } 
//    i
//  }
//  def entropy: Double = maths.entropy(this)
//  def klDivergence(p:ProportionsValue): Double = maths.klDivergence(this, p)
//  def jsDivergence(p:ProportionsValue): Double = maths.jensenShannonDivergence(this, p)
//}
//class ProportionsArrayValue(value:Array[Double]) extends ProportionsValue {
//  private val array = value
//  final def apply(i:Int) = array(i)
//  final def length = array.length
//  final def update(i:Int, v:Double) = array(i) = v
//}
//
//trait Proportions extends Variable /*with DiscreteGenerating*/ with IndexedSeqEqualsEq[Double] with ProportionsValue
//with VarAndValueGenericDomain[Proportions,ProportionsValue]
//{
//  def value = this.asInstanceOf[Value]
//  override def sampleInt = maths.nextDiscrete(this)(cc.factorie.random) // TODO Avoid the inefficiency of asSeq
//  def pr(index:Int) = apply(index)
//  def logpr(index:Int) = math.log(apply(index))
//  def maxPrIndex: Int = { var maxIndex = 0; var i = 1; while (i < length) { if (this(i) > this(maxIndex)) maxIndex =i; i += 1 }; maxIndex }
//  override def toString = this.take(10).mkString(printName+"(", ",", ")")
//
//  class DiscretePr(val index:Int, val pr:Double)
//  class DiscretePrSeq(val maxLength:Int) extends Seq[DiscretePr] {
//    def this(maxLength:Int, contents:Seq[Double]) = { this(maxLength); var i = 0; while (i < contents.length) { this += (i, contents(i)); i += 1 } }
//    private val _seq = new Array[DiscretePr](maxLength)
//    private var _length: Int = 0
//    def length = _length
//    def apply(i:Int) = _seq(i)
//    def iterator: Iterator[DiscretePr] = new Iterator[DiscretePr] {
//      var i = 0
//      def hasNext = i < _length
//      def next = { i += 1; _seq(i-1) }
//    }
//    def +=(index:Int, pr:Double): Unit = {
//      if (_length < maxLength || (pr > _seq(_length-1).pr && pr > 0.0)) {
//       if (_length < maxLength) { _seq(_length) = new DiscretePr(index, pr); _length += 1 }
//       else if (pr > _seq(_length-1).pr) _seq(_length-1) = new DiscretePr(index, pr)
//       var i = _length - 1
//       while (i > 0 && _seq(i).pr > _seq(i-1).pr) {
//         val tmp = _seq(i)
//         _seq(i) = _seq(i-1)
//         _seq(i-1) = tmp
//         i -= 1
//       }
//      }
//    }
//  }
//  def top(n:Int): Seq[DiscretePr] = new DiscretePrSeq(n, this)
//}
//
///** Proportions for which the indicies correspond to CategoricalValues.
//    The abstract method 'categoricalDomain' must be supplied.  */
//trait CategoricalProportions[A] extends Proportions {
//  def categoricalDomain: CategoricalDomain[A]
//  // TODO change "value:String" to "category:A"
//  class DiscretePr(override val index:Int, override val pr:Double, val value:String) extends super.DiscretePr(index, pr)
//  override def top(n:Int): Seq[DiscretePr] =
//    super.top(n).map(dpr => new DiscretePr(dpr.index, dpr.pr, categoricalDomain.getCategory(dpr.index).toString))
//  def topValues(n:Int) = top(n).toList.map(_.value)
//  // TODO Make a topCategories method
//}
//
//trait MutableProportions extends Proportions /*with MutableGeneratedVar*/ /*with Estimation[MutableProportions]*/ {
//  def set(p:ProportionsValue)(implicit d:DiffList): Unit
//  //def defaultEstimator: Estimator[MutableProportions] = MutableProportionsEstimator
//}
//
//class DenseProportions(p:Seq[Double]) extends MutableProportions {
//  //def this(ps:Double*) = this(ps)
//  def this(dim:Int) = this(Seq.fill(dim)(1.0/dim))
//  private var _p = new Array[Double](p.length)
//  if (p != Nil) this := p else setUniform(null)
//  @inline final def length: Int = _p.size
//  @inline final def apply(index:Int) = _p(index)
//  def set(p:ProportionsValue)(implicit d:DiffList): Unit = set(p.asInstanceOf[Seq[Double]])
//  def set(p:Seq[Double])(implicit d:DiffList): Unit = {
//    assert(p.size == _p.size, "size mismatch: new="+p.size+", orig="+_p.size)
//    val newP = p.toArray // TODO Make a copy, just in case it was already an array?
//    if (d ne null) d += ProportionsDiff(_p, newP)
//    _p = newP
//  }
//  def :=(p:Seq[Double]) = set(p)(null)
//  def setUniform(implicit d:DiffList): Unit = set(new UniformProportions(length))
//  case class ProportionsDiff(oldP:Array[Double], newP:Array[Double]) extends Diff {
//    def variable = DenseProportions.this
//    def undo = _p = oldP
//    def redo = _p = newP
//  }
//}
//
//
//object MaximizeProportions extends Maximize[MutableProportions,Nothing] {
//  override def attempt(variables:Iterable[Variable], varying:Iterable[Variable], model:Model, qModel:Model): Boolean = {
//    if (varying.size != 0) return false
//    if (variables.size != 1) return false
//    variables.head match {
//      case mp: MutableProportions => {
//        model match {
//          case model:GenerativeModel => apply(mp, model, qModel)
//          case _ => return false
//        }
//      }
//      case _ => return false
//    }
//    true
//  }
//  def apply(variables:Iterable[MutableProportions], varying:Iterable[Nothing], model:Model, qModel:Model): Unit = {
//    if (variables.size != 1) throw new Error
//    apply(variables.head, model.asInstanceOf[GenerativeModel], qModel)
//  }
//  //def apply(p:MutableProportions, model:GenerativeModel, qModel:Model = null): Unit = if (apply(p, model.factors(Seq(p)), qModel) == false) throw new Error("Not handled.")
//  def apply(p:MutableProportions, model:GenerativeModel, qModel:Model): Unit = {
//    // Zero an accumulator
//    var e: DenseCountsProportions = null
//    p match {
//      case p:DenseCountsProportions => { e = p; e.zero() }
//      case _ => e = new DenseCountsProportions(p.length)
//    }
//    // Initialize with prior; find the factor that is the parent of "p", and use its Dirichlet masses for initialization
//    model.parentFactor(p) match {
//      case f:Dirichlet.Factor => e.set(f._2)(null)
//      case _ => throw new Error
//    }
//    //factors.collectFirst({case f:Dirichlet.Factor if (f.child == p) => e.set(f._2)(null)})
//    // Incorporate children
//    @inline def incrementForDiscreteVar(dv:DiscreteVar, incr:Double): Unit = {
//      val qFactors = if (qModel eq null) Nil else qModel.factors(Seq(dv))
//      if (qFactors.size == 0)
//        e.increment(dv.intValue, 1.0)(null)
//      else qFactors.head match {
//        // The child has a Q distribution
//        case qd:Discrete.Factor => forIndex(qd._2.length)(i => e.increment(i, qd._2(i))(null))
//      } // TODO We should check that qFactors.size == 1
//    }
//    for (factor <- model.extendedChildFactors(p)) factor match {
//      //case parent:GenerativeFactor if (parent.child == p) => {} // Parent factor of the Proportions we are estimating already incorporated above
//      // The array holding the mixture components; the individual components (DiscreteMixture.Factors) will also be among the extendedChildFactors
//      case m:Mixture.Factor => {}
//      // A simple DiscreteVar child of the Proportions
//      case d:Discrete.Factor => incrementForDiscreteVar(d._1, 1.0)
//      // A DiscreteVar child of a Mixture[Proportions]
//      case dm:DiscreteMixture.Factor => {
//        val gate = dm._2
//        val qFactors = if (qModel eq null) Nil else qModel.factors(Seq(gate))
//        val mixtureIndex = dm._2.indexOf(e) // Yipes!  Linear search.
//        if (qFactors.size == 0) {
//          if (dm._3.intValue == mixtureIndex) incrementForDiscreteVar(dm._1, 1.0)
//        } else qFactors.head match {
//          // The gate has a Q distribution
//          case qd:Discrete.Factor => incrementForDiscreteVar(dm._1, qd._2(mixtureIndex))
//        }
//      }
//      case pdm:PlatedDiscreteMixture.Factor => {
//        require(qModel.factors(pdm.variables).size == 0) // We don't yet handle variational qModel of PlatedDiscreteMixture.Factor
//        val mixtureIndex = pdm._2.indexOf(e)
//        forIndex(pdm._1.length)(i => {
//          if (pdm._3.intValue(i) == mixtureIndex) e.increment(pdm._3.intValue(i), 1.0)(null)
//        })
//      }
//    }
//    if (p ne e) p.set(e)(null)
//  }
//}
//
//class DiracProportions(val length:Int, val peakIndex:Int) extends Proportions {
//  @inline final def apply(index:Int) = if (index == peakIndex) 1.0 else 0.0
//}
//
//class UniformProportions(val length:Int) extends Proportions {
//  @inline final def apply(index:Int) = 1.0 / length
//}
//
//class GrowableUniformProportions(val sizeProxy:Iterable[_]) extends Proportions {
//  // I used to have GrowableUniformProportions(val sizeProxy:{def size:Int}), but this results in java.lang.reflect.Method.invoke at runtime
//  def length = sizeProxy.size
//  @inline final def apply(index:Int) = {
//    val result = 1.0 / length
//    assert(result > 0 && result != Double.PositiveInfinity, "GrowableUniformProportions domain size is negative or zero.")
//    result
//  }
//}
//
//
//trait CountsProportions extends Proportions {
//  def zero(): Unit
//  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit 
//}
//
//class DenseCountsProportions(len:Int) extends MutableProportions with CountsProportions {
//  def this(p:Seq[Double]) = { this(p.length); this.set(p)(null) }
//  protected var _counts = new Array[Double](len) // TODO Make this private and provide a method _setCounts
//  protected var _countsTotal = 0.0
//  def length = _counts.length
//  def countsSeq: Seq[Double] = _counts.toSeq
//  def counts(index:Int) = _counts(index)
//  def countsTotal  = _countsTotal
//  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = { 
//    _counts(index) += incr; _countsTotal += incr
//    assert(_counts(index) >= 0, "counts("+index+")="+_counts(index)+" after incr="+incr)
//    assert(_countsTotal >= 0, "countsTotal="+_countsTotal+" after incr="+incr)
//    if (d ne null) d += DenseCountsProportionsDiff(index, incr)
//  }
//  def increment(incrs:Seq[Double])(implicit d:DiffList): Unit = {
//    forIndex(incrs.length)(i => increment(i, incrs(i)))
//  }
//  def set(p:ProportionsValue)(implicit d:DiffList): Unit = { zero(); increment(p.toSeq) }
//  def set(p:Seq[Double])(implicit d:DiffList): Unit = { zero(); increment(p.toSeq) }
//  def apply(index:Int): Double = { // TODO ! Remove this check below.
//    if (index >= _counts.length) 0.0 // This can happen in GrowableDenseCountsProportions when the domain grows but _counts doesn't
//    //if (index >= _counts.length) throw new Error("index="+index+" length="+this.length+" _counts.length="+_counts.length)
//    else if (_countsTotal == 0) 1.0 / length
//    else _counts(index) / _countsTotal
//  }
//  def zero(): Unit = { java.util.Arrays.fill(_counts, 0.0); _countsTotal = 0.0 }
//  @deprecated def setFrom(v:Variable)(implicit d:DiffList): Unit = v match {
//    case dcp:DenseCountsProportions if (dcp == this) => {}
//  }
//  /*def setCollapsed: Unit = {
//    //parentFactor.family.resetCollapsedChild(parentFactor)
//    this.zero()
//    // TODO Check to make sure that both "updates" below return true indicating success
//    val b1 = parentFactor.updateCollapsedChild(parentFactor)
//    val b2 = childFactors.forall(f => f.updateCollapsedParents(f, 1.0))
//    require(b1)
//    require(b2)
//    //for (factor <- childFactors) factor match { case f:Discrete.Factor => increment(f._1.intValue, 1.0)(null) }
//  }*/
//  //class DiscretePr(override val index:Int, override val pr:Double, val count:Double) extends super.DiscretePr(index,pr)
//  //override def top(n:Int): Seq[DiscretePr] = this.toArray.zipWithIndex.sortBy({case (p,i) => -p}).take(n).toList.map({case (p,i)=>new DiscretePr(i,p,counts(i))}).filter(_.pr > 0.0)
//  case class DenseCountsProportionsDiff(index:Int, incr:Double) extends Diff {
//    def variable = DenseCountsProportions.this
//    def undo = { _counts(index) -= incr; _countsTotal -= incr; assert(_counts(index) >= 0.0) }
//    def redo = { _counts(index) += incr; _countsTotal += incr; assert(_counts(index) >= 0.0) }
//  }
//}
//
//
//class GrowableDenseCountsProportions(val dimensionDomain:DiscreteDomain, initialCapacity:Int) extends DenseCountsProportions(initialCapacity) {
//  def this(dd:DiscreteDomain) = this (dd, 32)
//  private var _size: Int = 0
//  override def length: Int = math.max(_size, dimensionDomain.size) // new Exception().printStackTrace()
//  override def counts(index:Int):Double = if (index < _counts.size) _counts(index) else 0.0
//  protected def ensureCapacity(size:Int): Unit = if (_counts.size < size) {
//    val newSize = math.max(_counts.size * 2, size)
//    //println("GrowableDenseCountsProportions "+this.hashCode+" growing from "+_counts.size+" to capacity "+size+".  New size="+newSize)
//    val newCounts = new Array[Double](newSize)
//    Array.copy(_counts, 0, newCounts, 0, _counts.size)
//    _counts = newCounts
//    //println("GrowableDenseCountsProportions "+this.hashCode+" done growing.")
//  }
//  override def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
//    ensureCapacity(index+1)
//    //if (index >= _size) { println("GrowableDenseCountsProportions.increment growing index="+index); _size = index + 1 }
//    if (index >= _size) { _size = index + 1 }
//    //if (index >= _size) { _size = 100000 }
//    super.increment(index, incr)
//    //super.increment(_size, incr)
//  }
//}
//
//
///*object DenseProportions {
//  implicit val denseProportionsEstimator = new Estimator[DenseProportions] {
//    def estimate(p:DenseProportions, model:Model): Unit = {
//      val counts = new Array[Double](p.length)
//      for (child <- p.children) child match { case child:DiscreteVar => counts(child.intValue) = counts(child.intValue) + 1.0 }
//      for (i <- 0 until p.length) counts(i) /= p.children.size
//      p.set(counts)(null) // TODO Should we have a DiffList here?
//    }
//  }
//}*/
//
//
///*object DenseCountsProportions {
//  implicit val denseCountsProportionsEstimator = new Estimator[DenseCountsProportions] {
//    def estimate(p:DenseCountsProportions, model:Model): Unit = {
//      p.zero
//      for (child <- p.children) child match { case child:DiscreteVar => p.increment(child.intValue, 1.0)(null) }
//    }
//  }
//}*/
//
//
//
//
////trait SparseVectorIncrementableCounts extends IncrementableCounts {
////  protected val _counts = new SparseVector(this.length) { def length = size }
////}
//
//
//// List of (index,count), sorted in descending order by count, stored sparsely
//// Useful in Proportions where sampling is efficient because outcomes are considered in order of highest-count first.
//class SortedSparseCounts(dim:Int, capacity:Int = 2, val keepTrimmed:Boolean = false) {
//  /** Initialize counts from an unsorted list of indices */
//  def this(dim:Int, initial:Array[Int], keepDense: Boolean) = {
//    this(dim, initial.length)
//    dbuf = new Array[Int](dim)
//    var nonZeroCount = 0
//    _countsTotal = initial.length
//    var i = initial.length - 1
//    while (i >= 0) {
//      val j = initial(i)
//      if (dbuf(j) == 0) nonZeroCount += 1
//      dbuf(j) += 1
//      i -= 1
//    }
//    i = 0
//    while (i < dim) {
//      if (dbuf(i) > 0) {
//        buf(siz) = coti(dbuf(i), i)
//        siz += 1
//      }
//      i += 1
//    }
//    trim()
//    if (!keepDense) dbuf = null
//    // TODO Sort more efficiently
//    for (i <- 1 until siz) bubbleDownFrom(i)
//    //assert(countsTotal == calculatedCountsTotal) // TODO Remove this
//    //assert(check, counts.toString)
//  }
//  def this(dim:Int, initial:Array[Int]) = this(dim, initial, false)
//  require(dim > 1)
//
//  val length: Int = dim
//  def numPositions: Int = siz
//  private var _countsTotal: Int = 0 // total of all counts in buf
//  def countsTotal = _countsTotal
//  def calculatedCountsTotal = (0 until siz).foldLeft(0)((sum,i) => sum + co(buf(i))) // just for error checking
//  // Make sure we have enough bits to represent the dimension of the multinomial
//  private val topicMask = if (java.lang.Integer.bitCount(dim) == 1) dim-1 else java.lang.Integer.highestOneBit(dim) * 2 - 1
//  private val topicBits = java.lang.Integer.bitCount(topicMask)
//  private var siz = 0 // number of used entries in buf
//  private var dbuf: Array[Int] = null // dense buf, allocated only if keepDense=true
//  private var buf = new Array[Int](capacity) // stores both count and topic packed into a single Int, indexed by pos
//  //def buffer = buf // TODO Remove this method
//  protected def ensureCapacity(cap:Int): Unit = {
//    if (buf.length < cap) {
//      val newbuf = new Array[Int](cap+1) // allocate 1 extra space
//      System.arraycopy(buf, 0, newbuf, 0, buf.length)
//      buf = newbuf
//    }
//  }
//  def trim(): Unit = {
//    if (siz < buf.length-1) { // Don't bother if we'll only save 1 space
//      val newbuf = new Array[Int](siz)
//      System.arraycopy(buf, 0, newbuf, 0, siz)
//      buf = newbuf
//    }
//  }
//  require (dim < Math.MAX_SHORT)
//  //private val _posIndex: Array[Short] = if (keepIndex) Array.fill[Short](dim)(-1) else null
//  private def ti(coti:Int) = coti & topicMask // topic from packed count&index 
//  private def co(coti:Int) = coti >> topicBits // count from packed count&index
//  private def coti(count:Int, index:Int): Int = { assert(index < dim); (count << topicBits) | index }
//  protected def bubbleDownFrom(pos:Int): Unit = {
//    val newb = buf(pos)
//    var i = pos - 1
//    while (i >= 0 && buf(i) < newb) {
//      val tmp = buf(i); buf(i) = newb; buf(i+1) = tmp // swap
//      i -= 1
//    }
//  }
//  protected def bubbleUpFrom(pos:Int): Unit = {
//    //assert(check, counts.toString)
//    //val prevCounts = new scala.collection.mutable.ArrayBuffer[String]; prevCounts += counts.toString
//    val newb = buf(pos)
//    var i = pos + 1
//    while (i < siz && buf(i) > newb) {
//      val tmp = buf(i); buf(i) = newb; buf(i-1) = tmp // swap
//      //prevCounts += counts.toString
//      i += 1
//    }
//    //assert(check, "pos="+pos+" newb=("+ti(newb)+","+co(newb)+")\n"+prevCounts.mkString("\n")+"\n"+counts.toString)
//  }
//  def deletePosition(pos:Int): Unit = {
//    require(co(buf(pos)) == 0) // otherwise we need to adjust _countsTotal and dbuf(index) = 0
//    if (pos < siz - 1) System.arraycopy(buf, pos+1, buf, pos, siz-(pos+1))
//    siz -= 1
//    if (keepTrimmed && siz < buf.length - 3) { // Only try shrinking if we have 3 extra spaces
//      val newbuf = new Array[Int](siz+1) // Allocate 1 extra space
//      System.arraycopy(buf, 0, newbuf, 0, siz)
//      buf = newbuf
//    }
//  }
//  def countAtPosition(pos:Int) = co(buf(pos))
//  def indexAtPosition(pos:Int) = ti(buf(pos))
//  def incrementCountAtPosition(pos:Int, incr:Int): Unit = {
//    //val prevCounts = counts.toString
//    //val prevTi = ti(buf(pos))
//    //assert(check, prevCounts)
//    if (dbuf ne null) dbuf(ti(buf(pos))) += incr
//    //val newb = buf(pos) + (incr << topicBits)
//    val newCount = co(buf(pos)) + incr
//    val newb = coti(newCount, ti(buf(pos)))
//    //assert(ti(newb) == prevTi)
//    buf(pos) = newb
//    _countsTotal += incr
//    assert(newCount >= 0)
//    if (newCount == 0) deletePosition(pos)
//    else if (incr > 0) bubbleDownFrom(pos)
//    else if (incr < 0) bubbleUpFrom(pos)
//    //assert(countsTotal == calculatedCountsTotal) // TODO Remove this
//    //assert(check, "\npos="+pos+" incr="+incr+" newCount="+newCount+"\n"+prevCounts+"\n"+counts.toString) // TODO Remove this
//  }
//  // TODO Make this do binary search instead of linear search
//  def positionOfIndex(index:Int): Int = {
//    var i = 0
//    while (i < siz) {
//      if (ti(buf(i)) == index) return i
//      i += 1
//    }
//    -1
//  }
//  def countOfIndex(index:Int): Int = {
//    if (dbuf ne null) {
//      dbuf(index)
//    } else {
//      val pos = positionOfIndex(index)
//      if (pos == -1) 0 else co(buf(pos))
//    }
//  }
//  def incrementCountAtIndex(index:Int, incr:Int): Unit = {
//    //assert(check, counts.toString)
//    //val prevCounts = counts.toString
//    val pos = positionOfIndex(index)
//    if (pos == -1) {
//      if (incr <= 0) {
//        System.err.println(this.counts.toString)
//        throw new Error("index="+index+" count="+incr) // TODO just test "incr"
//      }
//      ensureCapacity(siz+1)
//      buf(siz) = coti(incr, index)
//      if (dbuf ne null) { /* assert(dbuf(index) == 0);*/ dbuf(index) = incr }
//      _countsTotal += incr
//      siz += 1
//      bubbleDownFrom(siz-1)
//      //println("SortedSparseCounts pos="+pos+" siz="+siz+" coti="+coti(incr, index))
//      //assert(countsTotal == calculatedCountsTotal, "ct="+countsTotal+" cct="+calculatedCountsTotal) // Remove this
//      /*if (!check) {  // TODO Remove this
//        println(prevCounts)
//        println(counts.toString)
//        assert(false)
//      }*/
//    } else {
//      incrementCountAtPosition(pos, incr)
//      //assert(check, counts.toString) // TODO Remove this
//    }
//  }
//  // Next method for CountsProportions trait
//  def zero(): Unit = {
//    siz = 0
//    _countsTotal = 0
//    if (dbuf ne null) java.util.Arrays.fill(dbuf, 0)
//  }
//
//  def counts: Iterable[(Int,Int)] = // (count,index)
//    for (i <- 0 until siz) yield (ti(buf(i)), co(buf(i)))
//  def forCounts(f:(Int,Int)=>Unit): Unit = 
//    for (i <- 0 until siz) f(ti(buf(i)), co(buf(i)))
//  def printCounts(domain:CategoricalDomain[String]): Unit = {
//    for (i <- 0 until siz) print(domain.getCategory(ti(buf(i)))+"="+co(buf(i))+" ")
//    println
//  }
//  /** Return false if there is internal inconsistency */
//  def check: Boolean = {
//    return true
//    for (i <- 0 until siz-1) {
//      val b1 = buf(i); val b2 = buf(i+1)
//      val ti1 = ti(b1); val ti2 = ti(b2) 
//      val co1 = co(b1); val co2 = co(b2)
//      if (ti1 == ti2) return false
//      if (co1 < co2) return false
//      if (co1 == 0 || co2 == 0) return false
//    }
//    if (siz == 1 && co(buf(0)) == 0) return false // To catch the siz==1 case 
//    true
//  }
//}
//
//class SortedSparseCountsProportions(dim:Int) extends SortedSparseCounts(dim) with CountsProportions {
//  // TODO We need somehow to say that this isDeterministic function of this.prior.
//  var prior: Masses = null
//  def apply(index:Int): Double = {
//    if (prior eq null) {
//      if (countsTotal == 0) 1.0 / length
//      else countOfIndex(index).toDouble / countsTotal
//    } else {
//      if (countsTotal == 0) prior(index) / prior.massesTotal
//      else countOfIndex(index).toDouble / countsTotal
//    }
//  }
//  def increment(index:Int, incr:Double)(implicit d:DiffList): Unit = {
//    assert(d eq null)
//    assert(incr.floor == incr)
//    incrementCountAtIndex(index, incr.toInt) 
//  }
//  override def top(n:Int): Seq[DiscretePr] =
//    for (i <- 0 until math.min(n, numPositions)) yield 
//      new DiscretePr(indexAtPosition(i), countAtPosition(i).toDouble / countsTotal)
//
//}
//
//
//
