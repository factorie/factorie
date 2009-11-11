package cc.factorie.example
import cc.factorie.er2._
import cc.factorie.logic2._
import scala.collection.mutable.{HashMap,ArrayBuffer}

object LogicDemo2a {
  
	case class Arg[A] extends Accessor[A,A] { // Where is this supposed to come from?
    def forward = (a:A) => List(a)
    def reverse = (a:A) => { /*println("Arg a="+a);*/ List(a) }
  }

  def main(args:Array[String]) : Unit = {
    // Define entity, attribute and relation types
    class Person(val name:String, val mother:Person) extends Variable with AccessorUnit {
      type AccessorUnitType = Person1
      val smokes = new Smokes; class Smokes extends Bool with AttributeOf[Person] { override def toString = super.toString+":"+owner.toString; }
      val cancer = new Cancer; class Cancer extends Bool with AttributeOf[Person] { override def toString = super.toString+":"+owner.toString; }
      var children = new ArrayBuffer[Person];
      if (mother != null) mother.children += this
      override def toString = "Person("+name+")"
    }

    // Define boilerplate, to support access to attributes in the first-order logic syntax
    class PersonAccessor[A,B](prefix:Accessor[A,B], forward1:B=>Iterable[Person], reverse1:Person=>Iterable[B]) extends MultiAccessor(prefix, forward1, reverse1) {
      def this() = this(null, null, null) 
      def smokes = new AttributeAccessor[A,Person,Person#Smokes](this, _.smokes)
      def cancer = new AttributeAccessor[A,Person,Person#Cancer](this, _.cancer)
      def mother = new PersonAccessor(this, (p:Person) => if (p.mother == null) Nil else List(p.mother), (p:Person)=>p.children)
      def children = new PersonAccessor[A,Person](this, _.children, p=> if (p.mother == null) Nil else List(p.mother))
    }
    class Person1 extends PersonAccessor[Person,Person](Arg[Person],a=>List(a),a=>List(a))
   
    val amy = new Person("amy", null)
    val bob = new Person("bob", amy)
    val cas = new Person("cas",amy)
    val don = new Person("don", amy)
    val eli = new Person("eli", cas)

    val siblings = (new Person1).mother.children
    println(siblings.forward(bob))
   
    def printFactors(f:Iterable[Factor]) = {
      println("Factor count = "+f.toList.size) 
      println(f)      
    }

    println("\ntemplate1")
    val template1 = (Forany[Person] { p => p.smokes ==> p.cancer }) % 2.0
    printFactors(template1.factors(amy.smokes))
    println("stats "+template1.stats(amy.smokes))
    
    println("\ntemplate2")
    val template2 = Forany[Person] { p => Not(p.mother.smokes) ^ Not(p.children.smokes) ==> Not(p.smokes) }
    printFactors(template2.factors(amy.smokes)) // matches a lot because 
    printFactors(template2.factors(bob.smokes)) // doesn't match because bob doesn't have children
    printFactors(template2.factors(cas.smokes))

    println("\ntemplate2b")
    val template2b = Forany[Person] { p => p.mother.smokes }
    printFactors(template2b.factors(amy.smokes))
    println("stats "+template2b.stats(amy.smokes))
    printFactors(template2b.factors(bob.smokes))
    println("stats "+template2b.stats(bob.smokes))

    println("\ntemplate3")
    //val template3 = Forany[Person] { p => Score(p.smokes, p.mother.smokes) }
    //printFactors(template3.factors(cas.smokes))

    println("\ntemplate4")
    //val template4 = Forany[Person] { p => Score(p.mother.smokes) }
    //printFactors(template4.factors(amy.smokes))
  }
}
