/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0.
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

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
  def apply : Unit = this.foreach(_.apply)
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


