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

	// Templates
	
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
		def randomVariable(implicit random:Random): Variable = variable(random.nextInt(productArity))
		def score: Double
		def vector: Vector // TODO Remove this.  Not all Factors have a vector
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


	// TODO not yet used
	trait Beliefs {
		this: Factor =>
		val belief: Vector
	}

	/**A container for the sufficient statistics of a Factor. */
	// TODO Make this also extend Product, support scoring, etc, like Factor
	trait Stat extends Iterable[Stat] {
		def template: Template
		def vector: Vector // TODO remove this.  Not all Stat's have vectors
		/**A Stat can act as as singleton Iterable[Stat].
		This makes it easier to return a single Stat from unroll* methods. */
		def elements: Iterator[Stat] = Iterator.single(this)
		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Stat) = List(that, this)
	}

   

	/** The template for many factors.  Manages its connections to neighboring variables.
  	* Stores its parameters and has methods for templating behavior */
	trait Template {
	  type TemplateType <: Template
		type S <: Stat // The case class of sufficient statistics
		//type N <: Factor // The case class of neighbors, defined in subtrait of Neighbors
    val statClasses = new ArrayBuffer[Class[IndexedVariable]] {
      var frozen :Boolean = false;
      def freeze = frozen = true;
      override def ensureSize(s:Int) = if (frozen) throw new IllegalStateException("Template already .init'ed.") else super.ensureSize(s)
    }
	  def statDomains : Seq[IndexedDomain[_]] = {
	    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
	    statClasses.map(IndexedDomain.get(_))
	  }	
    def freezeDomains : Unit = {
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.foreach(IndexedDomain.get[IndexedVariable](_).freeze)
    }	
    lazy val statsize : Int = {
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.productInts(IndexedDomain.get[IndexedVariable](_).allocSize)
    }	
 		/**A version of unroll0 that takes the Diff object instead of just the variable */
		def unroll(d: Diff): Iterable[Factor] = if (d.variable == null) Nil else unroll0(d.variable)
		def unroll0(v: Variable): Iterable[Factor]  // TODO change name to factors(v:Variable)
		def factors(v:Variable) : Iterable[Factor] = unroll0(v)
		def template : TemplateType = this.asInstanceOf[TemplateType]
		trait Factor extends cc.factorie.Factor { def template : TemplateType = Template.this.asInstanceOf[TemplateType] }
    trait Stat extends cc.factorie.Stat { override def template : TemplateType = Template.this.asInstanceOf[TemplateType] }
    // TODO Should the above TemplateType's be instead this.type?  It would be nice to avoid the need for TemplateType
		def factors(difflist: DiffList): Iterable[Factor] = {
			var result = new LinkedHashSet[Factor]()
			difflist.foreach(diff => result ++= unroll(diff))
			result.toList // TODO is this necessary?
		}
		def factors(variables:Iterable[Variable]) : Iterable[Factor] = {
			var result = new LinkedHashSet[Factor]()
			variables.foreach(v => result ++= factors(v))
			result.toList // TODO is this necessary?
		}
    //class Factor extends Templates.Factor
    def score(s:S) : Double
		def scoreStats(ss:Iterable[_<:S]) : Double = ss.foldLeft(0.0)(_ + score(_))
		def vectorStats(ss:Iterable[_<:S]) : Vector = {
		  val iter = ss.elements
		  if (iter.hasNext) {
		    val first: Vector = iter.next.vector
        if (!iter.hasNext) // There is only one there
          first
          else {
            var vec = new SparseVector(first.size) // TODO if 'first' is SparseBinaryVector this should be Sparse also
            vec += first
            while (iter.hasNext)
              vec += iter.next.vector
            vec
          }	
		  } else { // an empty iterator over Suff's.  Just return a (sparse) vector of zero's.
		    new SparseVector(statsize)
		  }
		}
    /** Perform the outer-product of two vectors, to yield */
    protected def flatOuter(vector1: Vector, vector2: Vector) : Vector = vector1 match {
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
 
    // TODO implement this!
    private def unflattenOuter(weightIndex:Int, dimensions:Int*) : Array[Int] = new Array[Int](2) 

	}	
 
 
 
	trait MarginalSamples extends Template {
		lazy val samples = {freezeDomains; new Array[Double](statsize)}
		def clearSamples = for (i <- 0 until samples.length) samples(i) = 0.0 // TODO surely there is a faster way
	}

 
	trait LogLinearScoring extends Template {
		type TemplateType <: LogLinearScoring
		def weights: Vector
		//def score(s:S): Double
	}
	trait DenseLogLinearScoring extends Template {
		type TemplateType <: DenseLogLinearScoring
		lazy val weights = {freezeDomains; new DenseVector(statsize)}
		def score(s:S) = weights dot s.vector
	}
	trait SparseLogLinearScoring extends Template {
		type TemplateType <: SparseLogLinearScoring
		lazy val weights = new SparseVector(statsize)
		def score(s:S) = weights dot s.vector
	}
	//trait LogLinearTemplate extends LogLinearScoring with Template
	//trait LogLinearTemplate extends Template with LogLinearScoring

 	abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template {
 	  val nc1 = nm1.erasure
    override def unroll0(v:Variable): Iterable[Factor] =
      if (nc1.isAssignableFrom(v.getClass)) unroll1(v.asInstanceOf[N1])
      else Nil
		def unroll1(v:N1): Iterable[Factor] = new Factor(v)
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1)
 	  def statistic(v1:N1) : Iterable[S]
		case class Factor(n1:N1) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic1[S1<:IndexedVariable] extends Template {
    case class Stat(s1:S1) extends super.Stat with Iterable[Stat] {
      def vector : Vector = s1.vector
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1]) : this.type = { statClasses += m1.erasure.asInstanceOf[Class[IndexedVariable]]; statClasses.freeze; this }  
  }
  abstract class TemplateWithStatistic1[N1<:IndexedVariable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistic1[N1]	{
		def statistic(v1:N1): Iterable[Stat] = Stat(v1)
		init(nm1)
	}

  abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
    override def unroll0(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
      if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])	
      if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])	
			ret
		}
		def unroll1(v:N1): Iterable[Factor] 
		def unroll2(v:N2): Iterable[Factor]
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1, f.n2)
 	  def statistic(v1:N1, v2:N2) : Iterable[S]
		case class Factor(n1:N1, n2:N2) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic2[S1<:IndexedVariable,S2<:IndexedVariable] extends Template {
    this : Template =>
    case class Stat(s1:S1, s2:S2) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, s2.vector)
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithStatistic2[N1<:IndexedVariable,N2<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with Statistic2[N1,N2]	{
		def statistic(v1:N1,v2:N2): Iterable[Stat] = Stat(v1,v2)
		init(nm1, nm2)
	}

  abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
 	  val nc3 = nm3.erasure
    override def unroll0(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
			if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
			if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
			if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
			ret
		}
		def unroll1(v:N1): Iterable[Factor]
		def unroll2(v:N2): Iterable[Factor]
		def unroll3(v:N3): Iterable[Factor]
 	  def _statistic(f:Factor) : Iterable[S] = statistic(f.n1, f.n2, f.n3)
 	  def statistic(v1:N1, v2:N2, v3:N3) : Iterable[S]
		case class Factor(n1:N1, n2:N2, n3:N3) extends super.Factor with Iterable[Factor] {
		  def score : Double = scoreStats(_statistic(this)) // which is implemented in the Template
		  def vector : Vector = vectorStats(_statistic(this)) // which is implemented in the Template
		}	
 	}
  trait Statistic3[S1<:IndexedVariable,S2<:IndexedVariable,S3<:IndexedVariable] extends Template {
    case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
    } 
    type S = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]], m3.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithStatistic3[N1<:IndexedVariable,N2<:IndexedVariable,N3<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with Statistic3[N1,N2,N3]	{
		def statistic(v1:N1,v2:N2,v3:N3): Iterable[Stat] = Stat(v1,v2,v3)
		init(nm1, nm2, nm3)
	}

