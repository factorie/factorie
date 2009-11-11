package cc.factorie.example
import cc.factorie.er1._
import cc.factorie.logic1._
import scala.collection.mutable.HashMap

object LogicDemo1b {

	def main(args:Array[String]) : Unit = {
		// Define entity, attribute and relation types
		class Person (val name:String) extends ItemizedVariable[Person] {
			val smokes = new Smokes; class Smokes extends Bool with AttributeOf[Person] with SampleCounts
			val cancer = new Cancer; class Cancer extends Bool with AttributeOf[Person] with SampleCounts
			val mother = new Mother; class Mother extends PrimitiveVariable[Person] with AttributeOf[Person]
			val age = new Age; class Age extends IntRangeVariable(0,8) with AttributeOf[Person] // in decades
			override def toString = name
		}
		class Company (val name:String) extends ItemizedVariable[Company] {
		  override def toString = name
		}
		object Friends extends ItemizedRelation[Person,Person];
		object Respects extends ItemizedRelation[Person,Person];
		object Spouse extends SymmetricFunction[Person];
		object Ceo extends Relation[Person,Company];
		object Employer extends Relation[Person,Company];
  
		// Define boilerplate to support access to attributes in the first-order logic syntax 
		object Smokes extends AttributeGetter[Person,Person#Smokes](_.smokes)
		object Cancer extends AttributeGetter[Person,Person#Cancer](_.cancer)
		object Age extends AttributeGetter[Person,Person#Age](_.age)

		// Define model
		val model = new Model (
			"CancerPrior" =: Forany[Person] { p => p->Cancer } % 0.1, 
			"SmokingCausesCancer" =: Forany[Person] { p => p->Smokes ==> p->Cancer } % 2.0,
			"FriendsSmokingMatch" =: Forany[Person] { p => p->Friends->Smokes <==> p->Smokes } % 1.5,
			//"FriendsAgeMatch" =: Forany[Person] { p => p->Friends->Age === p->Age} % 2.0,
			//"FriendsEmployersMatch" =: Forany[Person] { p => p->Friends->Employer === p->Employer} % 2.0,
			//"NoRespectSmokers" =: Forany2[Person,Person] { (p1,p2) => Not(p1->Smokes) ^ p2->Smokes ==> Not(Respects(p1,p2)) }
			//"NoRespectSmokers" =: Forany2[Person,Person] {p1 => {Forany Respects(p1)}  { (p1,p2) => Not(p1->Smokes) ^ p2->Smokes ==> Not(Respects(p1,p2)) }}
    )

		// Create the data
		val r = new Random
		val people = List("Amy","Bob","Cas","Don","Eli","Flo","Gus","Hal","Ira","Joe","Kim","Lou","Meg","Ned","Oli","Peg").map(new Person(_))
		val person = new HashMap[String,Person] ++ people.map(p=>(p.name,p)) // a map for access by string name
		val companies = List("ABC","BAE","CBC","DHL","EMC").map(new Company(_))
		people.foreach(_.smokes := r.nextBoolean)
		people.foreach(_.age := r.nextInt(8))
		people.foreach(Employer(_,Domain[Company].randomValue))
		for (p1 <- people; p2 <- people; if (r.nextBoolean)) Friends(p1,p2)
		for (p1 <- people; if (Spouse(p1) == null)) Spouse(p1,Domain[Person].randomValue)
  
		// Print parts of the data
		for (p <- people)
			println("%s smokes=%5s cancer=%5s spouse=%s employer=%5s friends=%s".
					format(p.name, p.smokes.value.toString, p.cancer.value.toString, Spouse(p), Employer(p).toList.toString, Friends(p).toList.toString))
    System.exit(0)
                                

		// Just for fun, print the factors that touch the variables indicating Don's friendships
		println(model.factors(Friends(person("Don"))))

		// Do 2000 iterations of Gibbs sampling, gathering sample counts every 20 iterations
		val sampler = new GibbsSampler(model)
		val numSamples = 100
		for (i <- 1 to numSamples) {
			sampler.process(people.map(_.cancer) + Friends, 20)
			people.foreach(_.cancer.incrementSample)
		}
		for (p <- people)
			println("%s p(cancer)=%f".format(p.name, p.cancer.samplePr(1)))
	}
}


