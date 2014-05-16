/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
/*& Family */
/*&
 * Family Tutorial
 * ===============
 **/

package cc.factorie.tutorial
object TutorialFamily extends App{
  import cc.factorie._ 
  import cc.factorie.la._ 
    
//  val family1 = new Family1[BooleanVariable] {
//    def score(b:BooleanValue) = if (b.booleanValue) 1.0 else 0.0
//  }
//
//  val family2 = new DotFamilyWithStatistics2[BooleanVariable,BooleanVariable] {
//    val weights = new DenseTensor2(Array(Array(1.0, 0.0), Array(0.0, 1.0)))
//  }
//

// TODO Consider giving a Family to singleton Factors, to support learning on them. 
  
}
