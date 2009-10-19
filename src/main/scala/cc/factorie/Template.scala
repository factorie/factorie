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
		def template : Template
		def numVariables: Int 
		def variable(index: Int): Variable
		def variables: Iterable[Variable] = for (i <- 0 until numVariables force) yield variable(i)
		def statistic : Statistic
		def randomVariable(implicit random:Random): Variable = variable(random.nextInt(numVariables))
		//def score: Double
		//def vector: Vector // TODO Remove this.  Not all Factors have a vector
		/**A Factor can act as as singleton Iterable[Factor].  This makes it easier to return a single Factor from unroll* methods. */
		def elements: Iterator[Factor] = Iterator.single(this)
		/**A Factor can be placed into a List with another with this method.
		This makes it easier to return 2-3 Factors from unroll* methods via Factor() :: Factor() :: Factor() */
		def ::(that: Factor) = List(that, this)
		/**Add this Factor to the FactorList of all the Variables that are this factor's neighbors */
		def addToVariables = variables.filter(_.isInstanceOf[FactorList]).map(_.asInstanceOf[FactorList].addFactor(this))
		// Implement Ordered, such that worst (lowest) scores are considered "high"
		def compare(that: Factor) = {val d = that.statistic.score - this.statistic.score; if (d > 0.0) 1 else if (d < 0.0) -1 else 0}
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
		def factorName = template.factorName
		override def toString = {
		  val sb = new StringBuilder; sb append factorName; sb append '(';
		  val iter = variables.elements
		  while (iter.hasNext) { sb append iter.next.toString; if (iter.hasNext) sb append "," } 
		  sb append ")"
		  sb.toString
    }
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
		def score : Double
		/** A Stat can act as as singleton Iterable[Stat].
				This makes it easier to return a single Stat from unroll* methods. */
		def elements: Iterator[Stat] = Iterator.single(this)
		/** A Stat can be placed into a List with another with this method.
				This makes it easier to return 2-3 Factors from unroll* methods via Stat() :: Stat() :: Stat() */
		def ::(that: Stat) = List(that, this)
	}

	trait Statistic extends Iterable[Stat] {
	  def score : Double
	}
   

	/** The template for many factors.  Manages its connections to neighboring variables.
  	* Stores its parameters and has methods for templating behavior */
	trait Template {
	  type TemplateType <: Template
	  type FactorType <: Factor
	  type StatType <: Stat
    type StatisticType <: Statistic
		trait Factor extends cc.factorie.Factor { 
      override def template : TemplateType = Template.this.asInstanceOf[TemplateType] // Why isn't "def template" in Template enough?
		  def statistics : Iterable[StatType]
   		def statistic : StatisticType = Template.this.statistic(this.statistics) // TODO verify that this gets override definitions of statistic()
		}
	  var factorName = "Factor"
	  def name(n:String) : this.type = { factorName = n; this }
	  def =:(n:String) : this.type = name(n) // TODO Is this syntax too weird?
    trait Stat extends cc.factorie.Stat { 
      override def template : TemplateType = Template.this.asInstanceOf[TemplateType] 
      def score = Template.this.score(this.asInstanceOf[StatType]) // TODO can we find a way to get rid of this cast?
    }
    class Statistic(val ss:Iterable[StatType]) extends cc.factorie.Statistic with Iterable[StatType] { /* or just "Stat" */
      def elements = ss.elements
      def template : TemplateType = Template.this.asInstanceOf[TemplateType] 
      def score = ss.foldLeft(0.0)(_ + Template.this.score(_)) // TODO verify that this gets overriden definitions of score(_)
    }
 		/**A version of factors that takes the Diff object instead of just the variable */
		def factors(d: Diff): Iterable[Factor] = if (d.variable == null) Nil else factors(d.variable)
		def factors(v: Variable): Iterable[Factor]
		def factors(difflist: DiffList): Iterable[Factor] = {
			var result = new LinkedHashSet[Factor]()
			difflist.foreach(diff => result ++= factors(diff))
			result.toList // TODO is this necessary?
		}
		def factors(variables:Iterable[Variable]) : Iterable[Factor] = {
			var result = new LinkedHashSet[Factor]()
			variables.foreach(v => result ++= factors(v))
			result.toList // TODO is this necessary?
		}
 		def stats(v:Variable): Iterable[Stat]
    def score(s:StatType) : Double
		//def scoreStats(ss:Iterable[_<:S]) : Double = ss.foldLeft(0.0)(_ + score(_))
    def statistic(ss:Iterable[StatType]) : StatisticType = new Statistic(ss).asInstanceOf[StatisticType] // TODO is there some way to avoid this cast?
	}
 
	trait ExpTemplate extends Template {
	  // The oddity of the next two methods are simply to provide a more intuitive compilation error message when
	  // a FATORIE user leaves out PerceptronLearning or somesuch from their Template definition
	  //def score(s:StatType) = 0.0 // to be overriden in learning methods
	  //def learningMethod : String
    // But then I gave up on this approach, when I saw Templates like TrueIndexedValueTemplate,
    // which don't do any learning, but do define their own score.
    // TODO In the FACTORIE documentation, write a note about "compiler error "score" undefined, could mean that you've left out the learning method.
		val statClasses = new ArrayBuffer[Class[IndexedVariable]] {
      var frozen :Boolean = false;
      def freeze = frozen = true;
      override def ensureSize(s:Int) = if (frozen) throw new IllegalStateException("Template already .init'ed.") else super.ensureSize(s)
    }
	  def statDomains : Seq[IndexedDomain[_]] = {
	    if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
	    statClasses.map(Domain.get[IndexedVariable](_))
	  }	
    def freezeDomains : Unit = {
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.foreach(Domain.get[IndexedVariable](_).freeze)
    }	
    lazy val statsize : Int = {
      //val d = Domain.get[IndexedVariable](statClasses(0))
      //println("ExpTemplate domain allocSize "+d.allocSize)
      if (statClasses.isEmpty) throw new IllegalStateException("You must call .init on this Template before use.")
      statClasses.productInts(Domain.get[IndexedVariable](_).allocSize)
    }	
    type StatType <: Stat
    trait Stat extends super.Stat {
      def vector : Vector
    }
    override type StatisticType = Statistic
    class Statistic(ss:Iterable[_<:StatType]) extends super.Statistic(ss) {
    	lazy val vector : Vector = {
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
		}
    override def statistic(ss:Iterable[StatType]) : StatisticType = new Statistic(ss)
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
	} // end of ExpTemplate
 
 
	trait MarginalSamples extends ExpTemplate {
		lazy val samples = {freezeDomains; new Array[Double](statsize)}
		def clearSamples = for (i <- 0 until samples.length) samples(i) = 0.0 // TODO surely there is a faster way
	}

 
	trait LogLinearScoring extends ExpTemplate {
		type TemplateType <: LogLinearScoring
		def weights: Vector
	}
	trait DenseLogLinearScoring extends ExpTemplate {
		type TemplateType <: DenseLogLinearScoring
		lazy val weights = {freezeDomains; new DenseVector(statsize)}
		override def score(s:StatType) = weights dot s.vector
	}
	trait SparseLogLinearScoring extends ExpTemplate {
		type TemplateType <: SparseLogLinearScoring
		lazy val weights = new SparseVector(statsize)
		override def score(s:StatType) = weights dot s.vector
	}

 	abstract class Template1[N1<:Variable](implicit nm1: Manifest[N1]) extends Template {
 	  val nc1 = nm1.erasure
    // TODO create methods like this for all Templates and put abstract version in Template
    def respondsTo[NN](implicit m:Manifest[NN]) = nc1.isAssignableFrom(m.erasure)
    def factors(v:Variable): Iterable[Factor] = {
      if (nc1.isAssignableFrom(v.getClass)) unroll1(v.asInstanceOf[N1])
      else Nil
    }
		def unroll1(v:N1): Iterable[Factor] = new Factor(v)
 	  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1)
 	  def statistics(v1:N1) : Iterable[StatType]
 	  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
		case class Factor(n1:N1) extends super.Factor with Iterable[Factor] {
		  def numVariables = 1
		  def variable(i:Int) = i match { case 0 => n1; case _ => throw new IndexOutOfBoundsException(i.toString) }
		  def statistics : Iterable[StatType] = _statistics(this)
		}	
 	}
  trait Statistics1[S1<:Variable] extends Template {
    case class Stat(s1:S1) extends super.Stat with Iterable[Stat] // { def vector : Vector = s1.vector } 
    type StatType = Stat
  }
  trait ExpStatistics1[S1<:IndexedVariable] extends ExpTemplate {
    type StatType = Stat
    case class Stat(s1:S1) extends super.Stat with Iterable[Stat] {
      def vector : Vector = s1.vector
    } 
    def init(implicit m1:Manifest[S1]) : this.type = { statClasses += m1.erasure.asInstanceOf[Class[IndexedVariable]]; statClasses.freeze; this }  
  }
  abstract class TemplateWithStatistics1[N1<:Variable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with Statistics1[N1]	{
		def statistics(v1:N1): Iterable[Stat] = Stat(v1)
	}
  abstract class TemplateWithExpStatistics1[N1<:IndexedVariable](implicit nm1:Manifest[N1]) extends Template1[N1]()(nm1) with ExpStatistics1[N1]	{
		def statistics(v1:N1): Iterable[Stat] = Stat(v1)
		init(nm1)
	}

  abstract class Template2[N1<:Variable,N2<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
    override def factors(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
      if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])	
      if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])	
			ret
		}
		def unroll1(v:N1): Iterable[Factor] 
		def unroll2(v:N2): Iterable[Factor]
 	  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2)
 	  def statistics(v1:N1, v2:N2) : Iterable[StatType]
 	  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
		case class Factor(n1:N1, n2:N2) extends super.Factor with Iterable[Factor] {
		  def numVariables = 2
		  def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case _ => throw new IndexOutOfBoundsException(i.toString) }
		  def statistics : Iterable[StatType] = _statistics(this)
		}	
 	}
  trait Statistics2[S1<:Variable,S2<:Variable] extends Template {
    case class Stat(s1:S1, s2:S2) extends super.Stat with Iterable[Stat] 
    type StatType = Stat
  }
  trait ExpStatistics2[S1<:IndexedVariable,S2<:IndexedVariable] extends ExpTemplate {
    case class Stat(s1:S1, s2:S2) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, s2.vector)
    } 
    type StatType = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithExpStatistics2[N1<:IndexedVariable,N2<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2]) extends Template2[N1,N2]()(nm1,nm2) with ExpStatistics2[N1,N2]	{
		def statistics(v1:N1,v2:N2): Iterable[Stat] = Stat(v1,v2)
		init(nm1, nm2)
	}
  /*
  trait Proposer2[P1<:Variable,P2<:Variable] extends Template {
    case class Stat(s1:S1, s2:S2) extends super.Stat with Iterable[Stat] 
    type StatType = Stat
  }*/

  abstract class Template3[N1<:Variable,N2<:Variable,N3<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
 	  val nc3 = nm3.erasure
    override def factors(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
			if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
			if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
			if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
			ret
		}
		def unroll1(v:N1): Iterable[Factor]
		def unroll2(v:N2): Iterable[Factor]
		def unroll3(v:N3): Iterable[Factor]
 	  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2, f.n3)
 	  def statistics(v1:N1, v2:N2, v3:N3) : Iterable[StatType]
 	  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
		case class Factor(n1:N1, n2:N2, n3:N3) extends super.Factor with Iterable[Factor] {
 	  	def numVariables = 3
		  def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case 2 => n3; case _ => throw new IndexOutOfBoundsException(i.toString) }
		  def statistics : Iterable[StatType] = _statistics(this)
		}	
 	}
  trait Statistics3[S1<:Variable,S2<:Variable,S3<:Variable] extends Template {
    case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with Iterable[Stat] 
    type StatType = Stat
  }
  trait ExpStatistics3[S1<:IndexedVariable,S2<:IndexedVariable,S3<:IndexedVariable] extends ExpTemplate {
    case class Stat(s1:S1, s2:S2, s3:S3) extends super.Stat with Iterable[Stat] {
      lazy val vector : Vector = flatOuter(s1.vector, flatOuter(s2.vector, s3.vector))
    } 
    type StatType = Stat
    def init(implicit m1:Manifest[S1], m2:Manifest[S2], m3:Manifest[S3]) : this.type = { statClasses ++= List(m1.erasure.asInstanceOf[Class[IndexedVariable]], m2.erasure.asInstanceOf[Class[IndexedVariable]], m3.erasure.asInstanceOf[Class[IndexedVariable]]); this }  
  }
  abstract class TemplateWithExpStatistics3[N1<:IndexedVariable,N2<:IndexedVariable,N3<:IndexedVariable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3]) extends Template3[N1,N2,N3]()(nm1,nm2,nm3) with ExpStatistics3[N1,N2,N3]	{
		def statistics(v1:N1,v2:N2,v3:N3): Iterable[Stat] = Stat(v1,v2,v3)
		init(nm1, nm2, nm3)
	}

  abstract class Template4[N1<:Variable,N2<:Variable,N3<:Variable,N4<:Variable](implicit nm1:Manifest[N1], nm2:Manifest[N2], nm3:Manifest[N3], nm4:Manifest[N4]) extends Template {
 	  val nc1 = nm1.erasure
 	  val nc2 = nm2.erasure
 	  val nc3 = nm3.erasure
 	  val nc4 = nm4.erasure
    override def factors(v: Variable): Iterable[Factor] = {
      var ret = new ListBuffer[Factor]
			if (nc1.isAssignableFrom(v.getClass)) ret ++= unroll1(v.asInstanceOf[N1])
			if (nc2.isAssignableFrom(v.getClass)) ret ++= unroll2(v.asInstanceOf[N2])
			if (nc3.isAssignableFrom(v.getClass)) ret ++= unroll3(v.asInstanceOf[N3])
			if (nc4.isAssignableFrom(v.getClass)) ret ++= unroll4(v.asInstanceOf[N4])
			ret
		}
		def unroll1(v:N1): Iterable[Factor]
		def unroll2(v:N2): Iterable[Factor]
		def unroll3(v:N3): Iterable[Factor]
		def unroll4(v:N4): Iterable[Factor]
 	  def _statistics(f:Factor) : Iterable[StatType] = statistics(f.n1, f.n2, f.n3, f.n4)
 	  def statistics(v1:N1, v2:N2, v3:N3, v4:N4) : Iterable[StatType]
 	  def stats(v:Variable) = factors(v).flatMap(_statistics(_))
		case class Factor(n1:N1, n2:N2, n3:N3, n4:N4) extends super.Factor with Iterable[Factor] {
 	  	def numVariables = 4
		  def variable(i:Int) = i match { case 0 => n1; case 1 => n2; case 2 => n3; case 3 => n4; case _ => throw new IndexOutOfBoundsException(i.toString) }
		  def statistics : Iterable[StatType] = _statistics(this)
		}	
 	}

