package cc.factorie.example
import cc.factorie.er1._
import cc.factorie.logic1._
import scala.collection.mutable.HashMap

/** A simple example, modeling smoking, cancer and frienships. */
object LogicDemo1a {

	def main(args:Array[String]) : Unit = {
		// Define entity, attribute and relation types
		class Person (val name:String) extends ItemizedVariable[Person] {
			val smokes = new Smokes; class Smokes extends BoolAttributeOf[Person]
			val cancer = new Cancer; class Cancer extends BoolAttributeOf[Person]
			override def toString = name
		}
		object Friends extends Relation[Person,Person];
  
		// Define boilerplate, to support access to attributes in the first-order logic syntax 
		object Smokes extends AttributeGetter[Person,Person#Smokes](_.smokes)
		object Cancer extends AttributeGetter[Person,Person#Cancer](_.cancer)

		// Define model
		val model = new Model (
			// Apriori, you are only 1/10 as likely to have cancer than not
			Forany[Person] { p => p->Cancer } * 0.1, 
			// If you smoke, you are 2 times more likely to have cancer
			Forany[Person] { p => p->Smokes ==> p->Cancer } * 2.0,
			// For each of your friends that smoke, you are 1.5 times more likely to smoke yourself
			Forany[Person] { p => p->Friends->Smokes <==> p->Smokes } * 1.5
    )

		// Create the data
		val amy = new Person("Amy"); amy.smokes := true
		val bob = new Person("Bob"); bob.smokes := true
		val cas = new Person("Cas"); cas.smokes := true
		val don = new Person("Don")
		Friends(amy,bob); Friends(bob,amy)
		Friends(cas,don); Friends(don,cas)
		
		// Do 2000 iterations of Gibbs sampling, gathering sample counts every 20 iterations
		val inferencer = new VariableSamplingInferencer(new GibbsSampler1[Bool](model))
		inferencer.burnIn = 100; inferencer.iterations = 2000; inferencer.thinning = 20
		val marginals = inferencer.infer(List(don.cancer, don.smokes))
    println("p(don.smokes == true) = "+marginals(don.smokes).pr(1))
    println("p(don.cancer == true) = "+marginals(don.cancer).pr(1))
	}
}


