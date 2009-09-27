package cc.factorie.example

import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet,ListBuffer}
import scala.collection.jcl.WeakHashMap
import scala.util.Sorting
import scala.reflect.Manifest
import scala.util.matching.Regex
import java.io.File

import cc.factorie.util.Stopwords
import cc.factorie.util.Implicits._

object LDADemo {
  
  // Declare different types of variables
	object Beta extends Dirichlet[Word](0.01)
  class Topic extends Multinomial[Word] with MixtureComponent[Topic]
  class Z extends MultinomialMixtureChoice[Topic,Word,Z]
  Domain.alias[Z,Topic]
  object Alpha extends Dirichlet[Z](1.0)
	class Theta extends Multinomial[Z]
	class Word(s:String) extends EnumVariable(s) with MultinomialOutcome[Word]
 	class Document(val file:String) extends ArrayBuffer[Word] { var theta:Theta = _	}

  def main(args: Array[String]) : Unit = {
  	// Read observed data and create Documents
		val documents = new ListBuffer[Document];
		val lexer = new Regex("[a-zA-Z]+")
		for (directory <- if (args.length > 0) args else List("/Users/mccallum/research/data/text/nipstxt/nips05")) {
			for (file <- new File(directory).listFiles; if (file.isFile)) {
				val d = new Document(file.toString)
				d ++= lexer.findAllIn(file.contentsAsString).toList.map(_ toLowerCase).filter(!Stopwords.contains(_)).map(new Word(_))
				documents += d
			}
		}
		println("Read "+documents.size+" documents with "+documents.foldLeft(0)(_+_.size)+" tokens and "+Domain[Word].size+" types.")
  
		// Create random variables
    val numTopics = 5
    val topics : Array[Topic] = for (i <- Array.range(0, numTopics)) yield new Topic ~ Beta
		val zs = new ArrayBuffer[Z] 	
  	for (document <- documents) {
  		document.theta = new Theta ~ Alpha
  		for (word <- document) {
  			val z = new Z ~ document.theta
  			word ~ z
  			zs +=z 
  		}
  	}

		// Fit model
    val model = new Model(new MultinomialMixtureChoiceTemplate)
		val sampler = new GibbsSampler(model)
    for (i <- 1 to 50) {
    	sampler.sample(zs,10)
    	println ("\nIteration "+i)
    	topics.foreach(t => println("Topic "+t.index+"  "+t.top(20).map(_._1)))
    }	


    
    
    
    
    


		0
	}
  
  
  
  //val topics sample(numTopics): Dirichlet(0.001)
  //val thetas sample(numDocs): Dirichlet(0.1)
  //val zs sample(numDocs,length) thetas(di)
  //val words sample(numDocs,length) topics(zi)

  val random = new scala.util.Random(1)
  
  // The variable types
  
  trait GenerativeDistribution {
    def estimate : Unit
  }
  
  // Consider using DenseVector instead of Array[Double] everywhere in this file

  // Make a general class Dirichlet to cover both constant and estimated varieties
  abstract class Dirichlet2[O<:MultinomialOutcome[O]](initialAlpha:Array[Double])(implicit m:Manifest[O]) extends Variable with GenerativeDistribution {
  	type VariableType <: Dirichlet[O]
  	class DomainInSubclasses
    def this(initialAlpha:Double)(implicit m:Manifest[O]) = this(Array.make(Domain[O](m).allocSize, initialAlpha))(m)
    val outcomeDomain = Domain[O](m) // TODO unfortunately this gets repeatedly set
    assert(initialAlpha.length == outcomeDomain.allocSize)
    val alpha : Array[Double] = { val a = new Array[Double](initialAlpha.length); Array.copy(initialAlpha, 0, a, 0, a.length); a } 
    def apply(index:Int) = alpha
    lazy val sum = alpha.foldLeft(0.0)(_+_)
    def generate(o:Multinomial[O]) = {}
    def ungenerate(o:Multinomial[O]) = {}

  }
  
  trait DirichletMomentMatchingEstimator requires Dirichlet[_] extends GenerativeDistribution {
  	val samples = new HashSet[Multinomial[_]]
    def generate(o:Multinomial[_]) = if (samples.contains(o)) throw new Error("Already generated Multinomial "+o) else samples += o 
    def ungenerate(o:Multinomial[_]) = samples -= o
    def mean : Array[Double]
    def estimate : Unit = throw new Error("Not yet implemented")
  }

  // Does not have its own Domain.  Size of alpha is Domain of O
  class Dirichlet[O<:MultinomialOutcome[O]](val alpha:Double)(implicit m:Manifest[O]) extends Variable {
    type VariableType <: Dirichlet[O]
    class DomainInSubclasses
    type OutcomeDomainType = O
    val outcomeDomain = Domain[O](m)
    def apply(index:Int) = alpha
    lazy val sum = alpha * outcomeDomain.size
    def generate(o:Multinomial[_])(implicit d:DiffList) = {}
    def ungenerate(o:Multinomial[_])(implicit d:DiffList) = {}

  }
  
  // Does not have its own Domain.  Size of pr is Domain of O
  // TODO should this Iterate over [O] or over [O#VariableType#ValueType] ???
  class Multinomial[O<:MultinomialOutcome[O]](implicit m:Manifest[O]) extends Variable with GenerativeDistribution with Iterable[O#VariableType#ValueType] {
    type VariableType <: Multinomial[O]
    class DomainInSubclasses
    type OutcomeDomainType = O
    val outcomeDomain = Domain[O](m) // TODO unfortunately this gets fetched and stored repeatedly for each instance
    lazy val counts = new Array[Double](outcomeDomain.allocSize)
    var total :Double = 0.0
    def size = counts.length
    var source : Dirichlet[O] = _
    def setSource(dir:Dirichlet[O])(implicit d:DiffList) : Unit = {
      if (d != null) d += MultinomialSetSourceDiff(source, dir)
      if (source != null) source.ungenerate(this) 
      source = dir
      source.generate(this)
    }
    def ~(d:Dirichlet[O]) : this.type /*Multinomial[V]*/ = { setSource(d)(null); this }
    def generate(o:O)(implicit d:DiffList) = { 
    	//println("Multinomial.outcomeDomain.size="+outcomeDomain.size+" generate "+o+" size="+size); Console.flush; 
    	counts(o.index) += 1.0; total += 1.0
      if (d != null) d += MultinomialGenerateDiff(o.index)
    }
    def ungenerate(o:O)(implicit d:DiffList) = { 
      counts(o.index) -= 1.0; assert(counts(o.index) >= 0.0)
      total -= 1.0; assert(total >= 0.0)
      if (d != null) d += MultinomialUngenerateDiff(o.index)
    }
    def estimate : Unit = {} // Nothing to be done; constantly keeps itself estimated
    def nextOutcomeValue : O#VariableType#ValueType = outcomeDomain.get(nextSample) 
    def nextSample : Int = {
      val s = random.nextDouble
      var sum = 0.0
      var i = 0
      val size = outcomeDomain.size
      while (i < size) {
        sum += pr(i)
        if (sum >= s) return i
      }
      return size - 1
    }
    def elements : Iterator[O#VariableType#ValueType] = new Iterator[O#VariableType#ValueType] {
      def hasNext = true
      def next = nextOutcomeValue
    }
    def pr(index:Int) : Double = {
      //println("Multinomial.pr "+counts(index)+" "+source(index)+" "+total+" "+source.sum)
      if (source != null)
      	(counts(index) + source(index)) / (total + source.sum)
      else
         counts(index) / total
    }
    def pr(o:O#VariableType) : Double = pr(o.index)
    def logpr(index:Int) = Math.log(pr(index))
    def logpr(indices:Seq[Int]) : Double = indices.foldLeft(0.0)(_+logpr(_))
    def top(n:Int) = counts.zipWithIndex.sortReverse({case (c,i)=>c}).take(n).toList.map({case (c,i)=>(outcomeDomain.get(i),pr(i),c)})
    override def toString = "Multinomial(count="+total+")"
    case class MultinomialGenerateDiff(i:Int) extends Diff {
      def variable = Multinomial.this
      def redo = { counts(i) += 1.0; total += 1.0 }
      def undo = { counts(i) -= 1.0; total -= 1.0 }
    }
    case class MultinomialUngenerateDiff(i:Int) extends Diff {
      def variable = Multinomial.this
      def redo = { counts(i) -= 1.0; total -= 1.0 }
      def undo = { counts(i) += 1.0; total += 1.0 }
    }
    case class MultinomialSetSourceDiff(oldSource:Dirichlet[O], newSource:Dirichlet[O]) extends Diff {
      def variable = Multinomial.this
      def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
      def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
    }
  }
  
  
  // Trait for any distribution that might be picked as part of a Multinomial mixture.
  // Creates is own Domain.  Number of components in the mixture is the size of the domain.  Values of the domain are these MixtureComponents.
  // Note that this is not a MultinomialOutcome, it is the *value* of MultinomialOutcome.
  // The MultinomialOutcome is MixtureChoice
  // class Theta extends Multinomial[Z]
  // class Topic extends Multinomial[Word] with MixtureComponent[Topic]
  // class Z extends MultinomialMixtureChoice[Topic,Theta,Word]
  // class Z extends MultinomialMixtureChoice[Topic,Z]
  // class Word(s:String) extends EnumVariable(s) with MultinomialOutcome[Word] with MultinomialSource[Topic] // optionally to know the type of the generating Multinomial
  trait MixtureComponent[This<:MixtureComponent[This]] extends TypedSingleIndexedVariable[This] /*with MultinomialOutcome[This]*/ {
    this : This =>
    type VariableType = This  // can we get away with this = ?
    //type DomainType = IndexedDomain[VariableType] // TODO Why is this necessary?
    //override type ValueType = This
    //override final def domain : Domain[This] = super.domain // TODO I think this line can be commented out???
    //def domain : Domain[This] = Domain[This].get(this.getClass)
    indx = domain.asInstanceOf[IndexedDomain[This]].index(this) // TODO can we avoid this cast?
    //println("Creating MixtureComponent this.getClass = "+this.getClass.toString+" index="+indx+" domain size="+domain.asInstanceOf[IndexedDomain[This]].size)
    override final def setByIndex(index:Int)(implicit d:DiffList) : Unit = new Error
  }

  // Order of type arguments: M==The distribution being selected, M==The multinomial distribution from which the selection is made
  class MultinomialMixtureChoice[M<:Multinomial[O] with MixtureComponent[M],O<:MultinomialOutcome[O],This<:MultinomialMixtureChoice[M,O,This]](implicit mm:Manifest[M/*#OutcomeDomainType*/]) extends MultinomialOutcome[This]
  {
    //type VariableType = MultinomialMixtureChoice[C,This]
    this : This =>
    type VariableType = This // TODO is this right?
    type ValueType = M
    class DomainInSubclasses
    //override type OutcomeDomainType = M
    //println("new MultinomialMixtureChoice "+this.getClass.getName+" and manifold "+mm+" domain.size="+domain.size)
    //println("new MultinomialMixtureChoice "+this.getClass.getName+" Domain[Z].size="+Domain[Z].size)
  	def multinomial : M = domain.get(index)
  	indx = random.nextInt(domain.size) // TODO is this how indx should be initialized?
  	private var _outcome : O = _
  	def outcome : O = _outcome
  	def setOutcome(o:O) = 
  		if (_outcome != null) throw new Error
  		else { _outcome = o } 
  	override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
  	  if (_outcome == null) throw new Error("No outcome yet set.")
  		multinomial.ungenerate(outcome)
  		super.setByIndex(newIndex)
  		multinomial.generate(outcome)
    }
    override def ~(m:Multinomial[This]) : this.type = { // For the generation of the choice
      //indx = m.nextSample
      /*super.*/setSource(m)(null)
      this // this.asInstanceOf[MultinomialChoice[M,O]]
    }
  }
    
  // TODO Consider renaming this MultinomialSample, because the instances of this class are idividual samples (e.g. token).  "outcome" may indicate the value (e.g. type) 
  trait MultinomialOutcome[This<:MultinomialOutcome[This]] extends SingleIndexedVariable {
  	this : This =>
    class DomainInSubclasses
    type OutcomeDomainType = This
    var source : Multinomial[This] = _
    def setSource(m:Multinomial[This])(implicit d:DiffList) : Unit = {
      if (m == null) throw new IllegalArgumentException("MultinomialOutcome cannot have null source")
      if (d != null) d += new MultinomialOutcomeSourceChangeDiff(source, m)
      if (source != null) source.ungenerate(this)
      source = m
      //println("Multinomial Outcome setSource on outcome "+this+" index="+index)
      source.generate(this)
  	}
    def ~(m:Multinomial[This]) : this.type = {
      setByIndex(m.nextSample)(null)
      setSource(m)(null); 
      this 
    }
    def ~[M<:Multinomial[This]](mmc:MultinomialMixtureChoice[M,This,_]) : this.type = {
      mmc.setOutcome(this); 
      this.~(mmc.multinomial) // either here or in mmc.setOutcome; not sure which is more natural
    }
  	//override def toString = "MultinomialOutcome(" + domain.get(indx).toString + "=" + indx + ")"
    override def setByIndex(newIndex:Int)(implicit d:DiffList) = {
      if (source != null) source.ungenerate(this)
      super.setByIndex(newIndex)
      if (source != null) source.generate(this)
    }
    case class MultinomialOutcomeSourceChangeDiff(oldSource:Multinomial[This], newSource:Multinomial[This]) extends Diff {
      def variable = MultinomialOutcome.this
      def redo = { if (oldSource != null) oldSource.ungenerate(variable)(null); source = newSource; newSource.generate(variable)(null) }
      def undo = { newSource.ungenerate(variable)(null); source = oldSource; if (oldSource != null) oldSource.generate(variable)(null) }
    }
  }
  
  /** The outcome of a coin flip, with boolean value.  this.value:Boolean */
	case class Flip extends Bool with MultinomialOutcome[Flip]
  case class Coin(p:Double, variance:Double) extends Multinomial[Flip] {
    def this(p:Double) = this(p:Double, 1.0)
    assert (p >= 0.0 && p <= 1.0)
    this.counts(0) = (1.0-p)/variance
    this.counts(1) = p/variance
  }


  
  class MultinomialMixtureChoiceTemplate extends TemplateWithStatistics1[MultinomialMixtureChoice[Topic,Word,Z]] {
  	def score(s:Stat) = {
  		val mmc = s.s1
  		val ret = mmc.multinomial.logpr(s.s1.outcome.index) + mmc.source.logpr(mmc.index)
  		//assert (ret == ret)
  		ret
  	}
  }



}

