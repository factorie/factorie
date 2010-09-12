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
object LogicDemo1 {

  def main(args:Array[String]) : Unit = {
    // Define entity, attribute and relation types
    class Person (val name:String) extends ItemizedObservation[Person] with Entity[Person] {
      // TODO Would it be possible to use instead: val getter = () => new PersonGetter
      type GetterType = PersonGetter
      class GetterClass extends PersonGetter
      object smokes extends BooleanVariable with Attribute
      object cancer extends BooleanVariable with Attribute
      //val friends = new Relation[Person](_.friends)
      override def toString = name
    }
    //object Friends extends Relation[Person,Person];
    
    // Define boilerplate, to support access to attributes in the entity-relationship syntax
    class PersonGetter extends EntityGetter[Person] {
      def smokes = getAttribute(_.smokes)
      def cancer = getAttribute(_.cancer)
      //def friends = getRelation[Person](_.friends, _.friends)
    } 

    // Define model
    val model = new Model (
      // Apriori, you are 1/10 times more likely to have cancer than not
      Forany[Person] { p => p.cancer } * 0.1, 
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
    
    // Do 2000 iterations of sampling, gathering sample counts every 20 iterations
    val inferencer = new VariableSamplingInferencer(new VariableSettingsSampler[BooleanVariable](model))
    inferencer.burnIn = 100; inferencer.iterations = 2000; inferencer.thinning = 20
    val marginals = inferencer.infer(List(don.cancer, don.smokes))
    println("p(don.smokes == true) = "+marginals(don.smokes).pr(1))
    println("p(don.cancer == true) = "+marginals(don.cancer).pr(1))
  }
}


