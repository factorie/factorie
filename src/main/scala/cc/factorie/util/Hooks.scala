/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
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



package cc.factorie.util
import scala.collection.mutable.ArrayBuffer

/** Lightweight subscribe/publish mechanism, implemented as a 
    simple list of 0-argument functions that be registered and then called. 
    Useful for providing notifications from deep within some processing. 
    @author Andrew McCallum
    @see Hooks1
    @see Hooks2
    @see Hooks3
*/
class Hooks0 extends ArrayBuffer[()=>Unit] {
  def apply(): Unit = this.foreach(_.apply())
}


/** Lightweight subscribe/publish mechanism, implemented as a 
    simple list of 1-argument functions that be registered and then called. 
    Useful for providing notifications from deep within some processing. 
    @author Andrew McCallum
    @see Hooks0
    @see Hooks2
    @see Hooks3
*/
class Hooks1[A] extends ArrayBuffer[A=>Unit] {
  def apply(a:A) : Unit = this.foreach(_.apply(a))
}

/** Lightweight subscribe/publish mechanism, implemented as a 
    simple list of 2-argument functions that be registered and then called. 
    Useful for providing notifications from deep within some processing. 
    @author Andrew McCallum
    @see Hooks0
    @see Hooks1
    @see Hooks3
*/
class Hooks2[A,B] extends ArrayBuffer[(A,B)=>Unit] {
  def apply(a:A,b:B) : Unit = this.foreach(_.apply(a,b))
}

/** Lightweight subscribe/publish mechanism, implemented as a 
    simple list of 3-argument functions that be registered and then called. 
    Useful for providing notifications from deep within some processing. 
    @author Andrew McCallum
    @see Hooks0
    @see Hooks1
    @see Hooks2
*/
class Hooks3[A,B,C] extends ArrayBuffer[(A,B,C)=>Unit] {
  def apply(a:A,b:B,c:C) : Unit = this.foreach(_.apply(a,b,c))
}


