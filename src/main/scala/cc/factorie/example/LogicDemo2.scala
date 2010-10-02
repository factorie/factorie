/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.example
import cc.factorie._
import cc.factorie.er._
import scala.collection.mutable.ArrayBuffer

/** A simple example, modeling smoking, cancer and frienships. */
object LogicDemo2 {

  // Define entity, attribute and relation types
  class Person (val name:String, var mother:Person) extends ItemizedObservation[Person] with Entity[Person] {
    type GetterType = PersonGetter
    class GetterClass extends PersonGetter // Person#GetterClass#super
    // When we have Scala 2.8 this next line will simply be:
    // object smokes extends BooleanVariable with Attribute
    val smokes = new Smokes; class Smokes extends BooleanVariable with Attribute
    val cancer = new Cancer; class Cancer extends BooleanVariable with Attribute
    val children = new ArrayBuffer[Person]
    if (mother != null) mother.children += this
    override def toString = name
  }
  //val friend = new Friends; class Friends extends Relation[Person,Person];
  object friend extends Relation[Person,Person]
  println("friend classname "+friend.getClass.getName)

  // Define boilerplate, to support access to attributes in the entity-relationship syntax
  class PersonGetter extends EntityGetter[Person] {
    def smokes = getAttribute(_.smokes)
    def cancer = getAttribute(_.cancer)
    def mother = getManyToOne[Person](_.mother, _.children)
    def children = getOneToMany[Person](_.children, _.mother)
    def siblings = getSymmetricManyToMany[Person](p => {
      println("PersonGetter.siblings.p="+p)
      println("PersonGetter.siblings.p.mother="+p.mother)
      println("PersonGetter.siblings.p.mother.children="+p.mother.children)
      p.mother.children.filter(p2=>p2 ne p)
    })
    def friends = getRelationDst[friend.type,Person,Person](friend) // the people whom this Person considers friends
    def friendly = getRelationSrc[friend.type,Person,Person](friend) // the people who consider this Person a friend
  } 

  def main(args:Array[String]) : Unit = {

    // Define model
    val model = new Model (
      // Apriori, you are 10 times more likely not to have cancer
      Forany[Person] { p => Not(p.cancer) } * 10 % "CancerPrior",
      
      // If you smoke, you are 2 times more likely to have cancer
      Forany[Person] { p => p.smokes ==> p.cancer } * 2.0 % "SmokingCausesCancer",
      
      // If your mother doesn't smoke and one of your children doesn't smoke, you ar 4 times less likely to smoke, for each of your children
      Forany[Person] { p => Not(p.mother.smokes) ^ Not(p.children.smokes) ==> Not(p.smokes) } * 4 % "MotherChildSmokes",
      
      // For each of your friends that smoke, you are 5 times more likely to smoke yourself.
      // TODO Interesting.  Think further about the differences between the two clauses below
      Forany[Person] { p => p.friends.smokes <==> p.smokes } * 5 % "FriendsSmoke",
      Forany[Person] { p => p.friends.smokes <==> ! p.smokes } * 0.2 % "FriendsSmoke2"
    )

    // Create the data
    val amy = new Person("Amy",null);  amy.smokes := true
    amy.mother = amy // Didn't yet implement 'null' mother; so she is her own.
    val bob = new Person("Bob", amy);  bob.smokes := true
    val cas = new Person("Cas", amy);  cas.smokes := true  
    val don = new Person("Don", cas);  don.smokes := false
    val eli = new Person("Eli", cas);  eli.smokes := false
    friend(amy,bob); friend(amy,eli)
    friend(bob,amy); friend(bob,eli)
    friend(don,eli)
    friend(eli,amy); friend(eli,bob); friend(eli,don)

    val friendship = friend(amy,bob)
    println("friendship classname "+friendship.getClass.getName)
    println("friendship printName "+friendship.printName)

    val siblings = newGetterUnit[Person].mother.children
    println("bob's siblings "+(newGetterUnit[Person].siblings).forward(bob))
    
    println("bob's friends: "+(newGetterUnit[Person].friends).forward(bob))
    
    def printFactors(f:Iterable[Factor]) = {
      println("Factor count = "+f.toList.size) 
      println(f)      
    }
    
    println("\nmodel")
    printFactors(model.factors(amy.smokes))

    println("\ntemplate1")
    val template1 = (Forany[Person] { p => p.smokes ==> p.cancer }) * 2.0
    printFactors(template1.factors(amy.smokes))
    println("stats "+template1.factors(amy.smokes).map(_.statistics))
    
    println("\ntemplate2")
    val template2 = Forany[Person] { p => Not(p.mother.smokes) ^ Not(p.children.smokes) ==> Not(p.smokes) }
    printFactors(template2.factors(amy.smokes)) // matches a lot because 
    printFactors(template2.factors(bob.smokes)) // doesn't match because bob doesn't have children
    printFactors(template2.factors(cas.smokes))

    println("\ntemplate2b")
    val template2b = Forany[Person] { p => p.mother.smokes }
    printFactors(template2b.factors(amy.smokes))
    println("stats "+template2b.factors(amy.smokes).map(_.statistics))
    printFactors(template2b.factors(bob.smokes))
    println("stats "+template2b.factors(bob.smokes).map(_.statistics))

    println("\ntemplate3")
    val template3 = Foreach[Person] { p => Score(p.smokes, p.mother.smokes) }
    printFactors(template3.factors(cas.smokes))

    println("\ntemplate4")
    val template4 = Foreach[Person] { p => Score(p.mother.smokes) } 
    printFactors(template4.factors(amy.smokes))
    //System.exit(0)
    
    // Do 2000 iterations of sampling, gathering sample counts every 20 iterations
    println("\ninference don.smokes="+don.smokes+" cas.smokes="+cas.smokes)
    val inferencer = new VariableSamplingInferencer(new VariableSettingsSampler[BooleanVariable](model))
    inferencer.burnIn = 100; inferencer.iterations = 2000; inferencer.thinning = 20
    val marginals = inferencer.infer(List(don.cancer, friend(don,cas), friend(cas,don)))
    println("p(don.cancer == true) = "+marginals(don.cancer).pr(1))
    println("p(friend(don,cas) == true) = "+marginals(friend(don,cas)).pr(1))
    println("p(friend(cas,don) == true) = "+marginals(friend(cas,don)).pr(1))
  }
}


/*

 Old LogicDemo1b saved here for future reference:
  
package cc.factorie.example
import cc.factorie.er1._
import cc.factorie.logic1._
import scala.collection.mutable.HashMap

object LogicDemo1b {

  def main(args:Array[String]) : Unit = {
    // Define entity, attribute and relation types
    class Person (val name:String) extends ItemizedObservation[Person] {
      val smokes = new Smokes; class Smokes extends Bool with AttributeOf[Person]
      val cancer = new Cancer; class Cancer extends Bool with AttributeOf[Person]
      val mother = new Mother; class Mother extends RefVariable[Person] with AttributeOf[Person]
      val age = new Age; class Age extends DiscreteVariable with AttributeOf[Person] // in decades
      Domain[Age].size = 10
      override def toString = name
    }
    class Company (val name:String) extends ItemizedObservation[Company] {
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
      "CancerPrior" =: Forany[Person] { p => p->Cancer } * 0.1, 
      "SmokingCausesCancer" =: Forany[Person] { p => p->Smokes ==> p->Cancer } * 2.0,
      "FriendsSmokingMatch" =: Forany[Person] { p => p->Friends->Smokes <==> p->Smokes } * 1.5,
      //"FriendsAgeMatch" =: Forany[Person] { p => p->Friends->Age === p->Age} * 2.0,
      //"FriendsEmployersMatch" =: Forany[Person] { p => p->Friends->Employer === p->Employer} * 2.0,
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

    // Do 2000 iterations of sampling, gathering sample counts every 20 iterations
    val inferencer = new VariableSamplingInferencer(new VariableSettingsSampler[Bool](model))
    inferencer.burnIn = 100; inferencer.iterations = 2000; inferencer.thinning = 20
    val marginals = inferencer.infer(people.map(_.cancer), people.map(_.cancer)) //+ Friends
    for (p <- people)
      println("%s p(cancer)=%f".format(p.name, marginals(p.cancer).pr(1)))
  }
}

 
 */
