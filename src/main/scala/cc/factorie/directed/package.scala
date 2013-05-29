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



package cc.factorie

import cc.factorie._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

package object directed {

  /** Create a new GenerativeFactor, make it the "parent" generating factor for this variable, 
      and add this new factor to the given model. */
  implicit def generatedVarExtras[V<:Var](v:V) = new GeneratedVarWrapper(v)
  implicit def generatedMutableVarExtras[V<:MutableVar[_]](v:V) = new GeneratedMutableVarWrapper(v)



  /*@deprecated
  object GenerativeModel extends Model {
    /** Only works on Iterable[GeneratedVar] */
    def factors(variables:Iterable[Variable]): Seq[Factor] = {
      val result = new scala.collection.mutable.ArrayBuffer[Factor];
      variables.foreach(v => v match {
        // TODO Also handle ContainerVariables!!!  Consider also interaction with handling of GeneratedVar.isDeterministic
        case cv: ContainerVariable[_] => throw new Error("ContainerVariables not yet handled in GenerativeModel.")
        case gv:GeneratedVar => {
          if (gv.parentFactor != null) result += gv.parentFactor
          if (gv.childFactors ne Nil) {
            for (childFactor <- gv.childFactors) {
              result += childFactor
              if (childFactor.child.isDeterministic) result ++= factors(Seq(childFactor.child))
            }
          }
        }
      })
      // TODO If a parent is a deterministic function (through a deterministic factor), also return factors that are parents of the deterministic factor
      // TODO Likewise for children?  Or perhaps not necessary.
      normalize(result)
    }
  }*/

  // Removed because it was causing implicit conflicts and because it was too much "magic".  
  // Users should have to create their own (implicit) models.
  // -akm 17 Jan 2012
  //implicit val defaultGenerativeModel = new GenerativeFactorModel

  /*implicit def seqDouble2ProportionsValue(s:Seq[Double]): ProportionsValue = new ProportionsValue {
    val value: IndexedSeq[Double] = s.toIndexedSeq
    def apply(i:Int) = value(i)
    def length = value.length
    //def sampleInt = maths.nextDiscrete(value)(cc.factorie.random)
  }*/
  
  //implicit val denseDirichletEstimator = new DenseDirichletEstimator
  //implicit val mutableProportionsEstimator = new MutableProportionsEstimator
  
  // TODO Consider generic mixtures like this:
  //new Word(str) ~ Mixture(phis)(Discrete(_))

}

