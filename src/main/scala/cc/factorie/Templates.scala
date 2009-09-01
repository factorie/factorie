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

trait Templates requires Model {
  //this : Model =>

	/** A single factor in a factor graph.  In other words, a factor
		template packaged with a set of variables neighboring the
		factor.  Factor inherits from Iterable[Factor] so that we can
		return a single Factor when an Iterable[Factor] is required. */
	trait Factor extends Product with Iterable[Factor] with Ordered[Factor] {
		//type TemplateType <: Template
		//def template : TemplateType
		def numVariables: Int = this.productArity
		def variable(index: Int): Variable = this.productElement(index).asInstanceOf[Variable]
		def variables: Iterable[Variable] = for (i <- 0 until productArity) yield variable(i)
		def randomVariable: Variable = variable(random.nextInt(productArity))
		def score: Double
		def vector: Vector
		/**A Factor can act as as singleton Iterable[Factor].  This makes it easier to return a single Factor from unroll* methods. */
		def elements: Iterator[Factor] = Iterator.single(this)
		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Factor) = List(that, this)
		/**Add this Factor to the FactorList of all the Variables that are this factor's neighbors */
		def addToVariables = variables.filter(_.isInstanceOf[FactorList]).map(_.asInstanceOf[FactorList].addFactor(this))
		// Implement Ordered, such that worst (lowest) scores are considered "high"
		def compare(that: Factor) = {val d = that.score - this.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
		// Implement equality based on class assignability and Variable contents equality
		def canEqual(other: Any) = other.isInstanceOf[Factor]
		override def equals(other: Any): Boolean = {
			//(this eq other) ||
			if (!canEqual(other)) return false
			val fother = other.asInstanceOf[Factor]
			(this.numVariables == fother.numVariables &&
							(0 until numVariables).forall(i => this.variable(i).hashCode == fother.variable(i).hashCode &&
											this.variable(i) == fother.variable(i)))
		}
		var _hashCode = -1
		override def hashCode: Int = {if (_hashCode == -1) _hashCode = variables.sumInts(_ hashCode); _hashCode}
		override def toString = this.getClass.getName + variables.foldLeft("(")(_ + _.toString + ",") + ")"
	}

	/** Unroll a collection of Variables in a Model, and return all Factors that touch those Variables. */
	def unroll(variables: Iterable[Variable]): Iterable[Factor] = {
		var factors = new HashSet[Factor]
		// unroll all factors touching all v in variables
		for (v <- variables; t <- modelTemplates)
			t.asInstanceOf[Neighbors].unroll0(v).foreach(f => factors += f)
		// Make sure each variables factor list starts empty
		variables.filter(_.isInstanceOf[FactorList]).foreach(_.asInstanceOf[FactorList].clearFactors)
		// Add relevant factors to each relevant neighboring variable
		factors.foreach(_.addToVariables)
		factors.toSeq
	}

	// TODO not yet used
	trait Beliefs {
		this: Factor =>
		val belief: Vector
	}

	/**A container for the sufficient statistics of a Factor. */
	// TODO Make this also extend Product, support scoring, etc, like Factor
	trait Suff extends Iterable[Suff] {
		def template: Template
		def vector: Vector
		/**A Suff can act as as singleton Iterable[Suff].
		This makes it easier to return a single Suff from unroll* methods. */
		def elements: Iterator[Suff] = Iterator.single(this)
		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Suff) = List(that, this)
	}

	trait LogLinearScoring extends Template {
		type TemplateType <: LogLinearScoring
		def weights: Vector
		def score(s: S): Double
	}

	trait DenseLogLinearScoring extends Template {
		type TemplateType <: DenseLogLinearScoring
		lazy val weights = {freezeDomains; new DenseVector(suffsize)}
		def score(s: S) = weights dot s.vector
	}

	trait SparseLogLinearScoring extends Template {
		type TemplateType <: SparseLogLinearScoring
		lazy val weights = new SparseVector(suffsize)
		def score(s: S) = weights dot s.vector
	}

	//trait LogLinearTemplate extends LogLinearScoring with Template

	//trait LogLinearTemplate extends Template with LogLinearScoring

	/**The template for many factors.  Stores its parameters and has methods for templating behavior */
	trait Template {
		//template =>
		type S <: Suff // The case class of sufficient statistics
		type N <: Factor // The case class of neighbors, defined in subtrait of Neighbors
		val sDomains: Array[IndexedDomain[_]]
		lazy val suffsize: Int = sDomains.productInts(_ allocSize)
		protected def freezeDomains: Unit = sDomains.foreach(_ freeze)
		def factors(difflist: Iterable[Diff]): Iterable[Factor] // TODO consider trying to make return type Iterable[N]
		//def vector(s:S) : Vector // TODO consider putting this here instead of in Factor, to allow Template subclasses to change behavior. -akm
		def score(s: S): Double
		type TemplateType <: Template
		//def template : TemplateType = this
		//def thisTemplate = this
		trait Factor extends Templates.this.Factor {
			def template: TemplateType = Template.this.asInstanceOf[TemplateType]
		}
	}

	abstract class Template1[S1 <: IndexedVariable](implicit vm1: Manifest[S1]) extends Template {
		case class Suff(s1: S1) extends Templates.this.Suff with Iterable[Suff] {
			override def template = Template1.this
			def productArity = 1
			def productElement(index: Int) = index match {case 0 => s1}
			def vector = s1.vector
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1))
		//def indices(s:S) = s.s1.indices
		//def vector(s:S) = s.s1.vector
	}

	private def flatOuter(vector1: Vector, vector2: Vector) = vector1 match {
		case v1: SingletonBinaryVector => vector2 match {
			case v2: SingletonBinaryVector =>
				new SingletonBinaryVector(v1.size * v2.size, v1.singleIndex * v2.size + v2.singleIndex)
			case v2: SparseBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v2.activeDomain.size);
						var i = 0;
						for (i2 <- v2.activeDomain) {
							arr(i) = v1.singleIndex * v2.size + i2;
							i += 1;
						};
						arr
					})
		}
		case v1: SparseBinaryVector => vector2 match {
			case v2: SingletonBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v1.activeDomain.size);
						var i = 0;
						for (i1 <- v1.activeDomain) {
							arr(i) = i1 * v2.size + v2.singleIndex;
							i += 1;
						};
						arr
					})
			case v2: SparseBinaryVector =>
				new SparseBinaryVector(v1.size * v2.size,
					{
						val arr = new Array[Int](v1.activeDomain.size * v2.activeDomain.size);
						var i = 0;
						for (i1 <- v1.activeDomain; i2 <- v2.activeDomain) {
							arr(i) = i1 * v2.size + i2;
							i += 1;
						};
						arr
					})
		}
	}

	abstract class Template2[S1 <: IndexedVariable, S2 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2]) extends Template {
		case class Suff(s1: S1, s2: S2) extends Templates.this.Suff with Iterable[Suff] {
			def template = Template2.this

			def productArity = 2

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2}
			//def indices = Template2.this.indices(this)
			//def vector = Template2.this.indices(this)
			def vector = flatOuter(s1.vector, s2.vector)
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2))
		//def indices(s:S) : Seq[Int] =  for (i <- s.s1.indices; j <- s.s2.indices) yield i * sDomains(1).size + j
	}

	abstract class Template3[S1 <: IndexedVariable, S2 <: IndexedVariable, S3 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2], vm3: Manifest[S3]) extends Template {
		case class Suff(s1: S1, s2: S2, s3: S3) extends Templates.this.Suff with Iterable[Suff] {
			def template = Template3.this

			def productArity = 3

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2; case 2 => s3}
			//def indices = Template3.this.indices(this)
			def vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2), IndexedDomain(vm3))
		//def indices(s:S) = for (i <- s.s1.indices; j <- s.s2.indices; k <- s.s3.indices) yield i * sDomains(1).size * sDomains(2).size + j * sDomains(2).size + k
	}

	abstract class Template4[S1 <: IndexedVariable, S2 <: IndexedVariable, S3 <: IndexedVariable, S4 <: IndexedVariable](implicit vm1: Manifest[S1], vm2: Manifest[S2], vm3: Manifest[S3], vm4: Manifest[S4]) extends Template {
		case class Suff(s1: S1, s2: S2, s3: S3, s4: S4) extends Templates.this.Suff with Iterable[Suff] {
			def template = Template4.this

			def productArity = 4

			def productElement(index: Int) = index match {case 0 => s1; case 1 => s2; case 2 => s3; case 3 => s4}
			//def indices = Template4.this.indices(this)
			def vector = flatOuter(s1.vector, flatOuter(s2.vector, flatOuter(s3.vector, s4.vector)))
			//def score = template.score(this)
		}
		type S = Suff
		val sDomains = Array[IndexedDomain[_]](IndexedDomain(vm1), IndexedDomain(vm2), IndexedDomain(vm3), IndexedDomain(vm4))
		/*def indices(s:S) = for (i <- s.s1.indices; j <- s.s2.indices; k <- s.s3.indices; l <- s.s4.indices) yield
i * sDomains(1).size * sDomains(2).size * sDomains(3).size + j * sDomains(2).size * sDomains(3).size + k * sDomains(3).size + l */
	}

	class LinkedHashSet[A] extends scala.collection.mutable.Set[A] with FlatHashTable[A] {
		var list = List[A]()

		override def initialSize: Int = 32

		def contains(elem: A): Boolean = containsEntry(elem)

		def +=(elem: A) {add(elem)}

		def add(elem: A): Boolean = {
			if (addEntry(elem)) {
				list = elem :: list
				true
			} else false
		}

		def -=(elem: A) {remove(elem)}

		def remove(elem: A): Boolean = removeEntry(elem) match {
			case None => false
			case Some(elem) => list = list.filter(_ != elem); true
		}

		override def clear() {list = Nil; super.clear()}

		override def elements = list.elements
	}

	trait Neighbors extends Template {
		//this: Template =>
		val nDomains: Array[Domain[_]] // TODO Change this into a Tuple so that we can catch n-arg errors at compile time.

		/**A version of unroll0 that takes the Diff object instead of just the variable */
		def unroll(d: Diff): Iterable[N] = if (d.variable == null) Nil else unroll0(d.variable)

		def unroll0(v: Variable): Iterable[N]

		def factors(difflist: Iterable[Diff]): Iterable[N] = {
			var result = new LinkedHashSet[N]()
			difflist.foreach(diff => result ++= unroll(diff))
			result.toList
		}

		def _sufficient(f: N): Iterable[S]
		//def thisTemplate = this
		//type TemplateType <: Neighbors
		trait Factor extends super.Factor with Iterable[Factor] {
			//override def template : TemplateType = Neighbors.this
			//def template = thisTemplate
			private def thisn: N = this.asInstanceOf[N] // TODO Surely there is a way to avoid this silliness
			def score: Double = _sufficient(thisn) match {
				case single: Suff => Neighbors.this.score(single.asInstanceOf[S])
				case multi: Iterable[_] => multi.asInstanceOf[Iterable[S]].foldLeft(0.0)(_ + Neighbors.this.score(_))
			}

			def vector: Vector = _sufficient(thisn) match {
				case single: Suff => single.asInstanceOf[S].vector
				case multi: Iterable[_] => {
					val iter = multi.asInstanceOf[Iterable[S]].elements
					if (iter.hasNext) {
						val first: Vector = iter.next.vector
						var vec = new SparseVector(first.size) // TODO if 'first' is SparseBinaryVector this should be Sparse also
						vec += first
						while (iter.hasNext)
							vec += iter.next.vector
						vec
					} else {
						// an empty iterator over Suff's.  Just return a (sparse) vector of zero's.
						new SparseVector(suffsize)
					}
				}
			}
			// Support for BP message passing
			case class MessageTo(v: EnumVariable[_]) {
				lazy private val msg = new Array[Double](v.domain.size)

				def factor = Factor.this

				def message = msg

				def update: MessageTo = {
					if (!v.isConstant)
						for (i <- 0 until msg.length) msg(i) = 0.0 // TODO surely there is a faster way
					else {
						val msgNeighbors = variables.filter(v2 => v2 != v).map(v2 => v2.asInstanceOf[EnumVariable[_]]).toList
						def nextValues(vs: List[EnumVariable[_]]): Boolean = {
							if (vs.first.hasNextValue) {vs.first.setNextValue; true}
							else if (vs.tail != Nil) {if (!vs.first.isConstant) vs.first.setFirstValue; nextValues(vs.tail)}
							else false
						}
						for (i <- 0 until v.domain.size) {
							v.setByIndex(i)(null)
							msgNeighbors.foreach(_ setFirstValue)
							do {
								msg(i) += Factor.this.score * msgNeighbors.product(n => Factor.this.messageFrom(n).message(n.index))
							} while (nextValues(msgNeighbors))
						}
					}
					this // Return this so we can say messageTo(v).update.message
				}
			}
			case class MessageFrom(v: EnumVariable[_] with FactorList) {
				lazy private val msg = new Array[Double](v.domain.size)

				def factor = Factor.this

				def message = msg

				def update: MessageFrom = {
					for (i <- 0 until v.domain.size) {
						//msg(i) = v.factors.filter(_.!=(this)).product(_.messageTo(v)(i))
						msg(i) = 1.0
						for (f <- v.factors; if (f != this))
							msg(i) *= f.asInstanceOf[Factor].messageTo(v).message(i)
					}
					this // Return this so we can say messageFrom(v).update.message
				}
			}
			lazy private val _msgTo: Array[MessageTo] = variables.map(v => MessageTo(v.asInstanceOf[EnumVariable[_]])).toSeq.toArray
			lazy private val _msgFrom: Array[MessageFrom] = variables.map(v => MessageFrom(v.asInstanceOf[EnumVariable[_] with FactorList])).toSeq.toArray

			def messageTo(v: EnumVariable[_]) = _msgTo(variables.toSeq.indexOf(v))

			def messageFrom(v: EnumVariable[_]) = _msgFrom(variables.toSeq.indexOf(v))

			def messageTo(vi: Int) = _msgTo(vi)

			def messageFrom(vi: Int) = _msgFrom(vi)
			//def neighborDistribution : Array[Double]
			//def sufficientDistribution : Array[Double]
		}
	}


	trait MarginalSamples extends Template {
		lazy val samples = {freezeDomains; new Array[Double](suffsize)}

		def clearSamples = for (i <- 0 until samples.length) samples(i) = 0.0 // TODO surely there is a faster way
	}

	trait NoScore extends Template {
		override final def score(s: S) = 0
	}

	trait Neighbors1[N1 <: Variable] extends Neighbors {
		//this: Template =>
		type N = Factor
		//override val nDomains : Tuple1[Domain[N1]]  // I'm considering making it a Tuple, but for now its an Array -akm
		def unroll0(v: Variable): Iterable[N] =
			if (v.domain == nDomains(0)) unroll1(v.asInstanceOf[N1])
			else Nil

		def unroll1(v: N1): Iterable[Factor] = new Factor(v)

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.n1)

		def sufficient(v1: N1): Iterable[S]
		//case class Factor(n1:N1) extends super.Factor with Iterable[Factor]
		case class Factor(n1: N1) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors1[N1 <: IndexedVariable](implicit vm1: Manifest[N1])
					extends Template1[N1]()(vm1) with Neighbors1[N1]
	{
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]

		override def _sufficient(f: Factor): Iterable[S] = Suff(f.n1) // TODO consider a way to just return f??
		override def sufficient(v1: N1): Iterable[S] = Suff(v1)
	}

	trait Neighbors2[N1 <: Variable, N2 <: Variable] extends Neighbors {
		//this: Template =>
		type N = Factor
		//val n1 : Domain[N1] // = Domains.getFromClass[N1](Class[N1])
		//val n2 : Domain[N2] // = Domains.getFromClass[N2](Class[N2])
		def unroll0(v: Variable): Iterable[Factor] = {
			var ret = new ListBuffer[Factor]
			if (v.domain == nDomains(0)) ret ++= unroll1(v.asInstanceOf[N1])
			if (v.domain == nDomains(1)) ret ++= unroll2(v.asInstanceOf[N2])
			ret
		}

		def unroll1(v: N1): Iterable[Factor]

		def unroll2(v: N2): Iterable[Factor]

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.v1, f.v2)

		def sufficient(v1: N1, v2: N2): Iterable[S]
		case class Factor(v1: N1, v2: N2) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors2[N1 <: IndexedVariable, N2 <: IndexedVariable](implicit vm1: Manifest[N1], vm2: Manifest[N2])
					extends Template2[N1, N2]()(vm1, vm2) with Neighbors2[N1, N2]
	{
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]
		//val n1 : Domain[N1] = domain1; val n2 : Domain[N2] = domain2
		override def _sufficient(f: Factor): Iterable[S] = Suff(f.v1, f.v2)

		override def sufficient(v1: N1, v2: N2): Iterable[S] = Suff(v1, v2)
	}

	trait Neighbors3[N1 <: Variable, N2 <: Variable, N3 <: Variable] extends Neighbors {
		this: Template =>
		type N = Factor
		//val n1 : Domain[N1]; val n2 : Domain[N2]; val n3 : Domain[N3]
		def unroll0(v: Variable): Iterable[Factor] = {
			var ret = new ListBuffer[Factor]
			if (v.domain == nDomains(0)) ret ++= unroll1(v.asInstanceOf[N1])
			if (v.domain == nDomains(1)) ret ++= unroll2(v.asInstanceOf[N2])
			if (v.domain == nDomains(2)) ret ++= unroll3(v.asInstanceOf[N3])
			ret
		}

		def unroll1(v: N1): Iterable[Factor]

		def unroll2(v: N2): Iterable[Factor]

		def unroll3(v: N3): Iterable[Factor]

		def _sufficient(f: Factor): Iterable[S] = sufficient(f.n1, f.n2, f.n3)

		def sufficient(v1: N1, v2: N2, v3: N3): Iterable[S]
		case class Factor(n1: N1, n2: N2, n3: N3) extends super.Factor with Iterable[Factor]
	}

	abstract class TemplateWithNeighbors3[N1 <: IndexedVariable, N2 <: IndexedVariable, N3 <: IndexedVariable](implicit vm1: Manifest[N1], vm2: Manifest[N2], vm3: Manifest[N3]) extends Template3[N1, N2, N3]()(vm1, vm2, vm3) with Neighbors3[N1, N2, N3] {
		// val n1 : Domain[N1] = domain1; val n2 : Domain[N2] = domain2; val n3 : Domain[N3] = domain3
		val nDomains = sDomains.asInstanceOf[Array[Domain[_]]]

		override def _sufficient(f: Factor): Iterable[S] = Suff(f.n1, f.n2, f.n3)

		override def sufficient(v1: N1, v2: N2, v3: N3): Iterable[S] = Suff(v1, v2, v3)
	}

 
 
 	/*
		abstract class GaussianFactor extends Factor3(Real, Real, Real) with HashSet[Real] {
			def score(mean:Real, variance:Real, value:Real) =
		}*/


}
