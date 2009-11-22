package cc.factorie.example
import cc.factorie.er._
import scala.collection.mutable.ArrayBuffer

/** A simple example, modeling smoking, cancer and frienships. */
object LogicDemo1 {

  def main(args:Array[String]) : Unit = {
    // Define entity, attribute and relation types
    class Person (val name:String) extends ItemizedVariable[Person] with Entity[Person] {
      type AccessorType = PersonAccessor
      // When we have Scala 2.8 this next line will simply be:
      // object smokes extends BooleanVariable with Attribute
      val smokes = new Smokes; class Smokes extends BooleanVariable with Attribute
      val cancer = new Cancer; class Cancer extends BooleanVariable with Attribute
      override def toString = name
    }
    //object Friends extends Relation[Person,Person];
  
    // Define boilerplate, to support access to attributes in the entity-relationship syntax
    class PersonAccessor extends EntityAccessor[Person] with AccessorHead[Person,Person] {
    	def smokes = getAttribute(_.smokes)
      def cancer = getAttribute(_.cancer)
    } 

    // Define model
    val model = new Model (
      // Apriori, you are 10 times more likely not to have cancer
      //Forany[Person] { p => Not(p) } * 10, 
      //Forany[Person] { p => p.cancer } * 0.1, 
      //Forany[Person] { p => Not(p.cancer) } * 10, 
      Forany[Person] { p => Not(accessor2formula(p.cancer)) } * 10, 
      // If you smoke, you are 2 times more likely to have cancer
      Forany[Person] { p => p.smokes ==> p.cancer } * 2.0
      // For each of your friends that smoke, you are 1.5 times more likely to smoke yourself
      //Forany[Person] { p => p.friends.smokes <==> p->Smokes } * 1.5
    )

    // Create the data
    val amy = new Person("Amy"); amy.smokes := true
    val bob = new Person("Bob"); bob.smokes := true
    val cas = new Person("Cas"); cas.smokes := true
    val don = new Person("Don")
    //Friends(amy,bob); Friends(bob,amy)
    //Friends(cas,don); Friends(don,cas)
    
    // Do 2000 iterations of Gibbs sampling, gathering sample counts every 20 iterations
    val inferencer = new VariableSamplingInferencer(new GibbsSampler1[BooleanVariable](model))
    inferencer.burnIn = 100; inferencer.iterations = 2000; inferencer.thinning = 20
    val marginals = inferencer.infer(List(don.cancer, don.smokes))
    println("p(don.smokes == true) = "+marginals(don.smokes).pr(1))
    println("p(don.cancer == true) = "+marginals(don.cancer).pr(1))
  }
}


