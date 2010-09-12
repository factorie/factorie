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
import scala.reflect.Manifest
import scala.collection.mutable.{ArrayBuffer,HashMap}

/** A sampler that has a Manifest for its context type.  
    Samplers are implicit converted to these before being added to a SamplerSuite. 
    The Manifest is necessary for type checking the AnyRef arguments to 'process'. 
    @author Andrew McCallum */
class GenericSampler[C](val sampler:Sampler[C])(implicit mc:Manifest[C]) extends Sampler[C] {
  //println("GenericSampler m="+mc)
  val contextClass = mc.erasure
  val contextManifest = mc
  /** If argument is the right type, then call process method. */
  val contextClassCache = new HashMap[Class[_],Boolean]
  def compatible(c:Class[_]): Boolean = {
    //mc.erasure.isAssignableFrom(c)  // This takes 44.4 seconds for LDADemo
    contextClassCache.getOrElseUpdate(c, contextManifest >:> Manifest.classType(c)) // This takes 42.8 seconds for LDADemo
    // No caching Manifest comparison with >:> took 468 seconds.  Wow!
  }
  def process0[T<:AnyRef](context:T): DiffList = 
    if (compatible(context.getClass)) {
      val c:C = context.asInstanceOf[C] // TODO How slow is this check?
      //|**("GenericSampler.process")
      val d = process(c) 
      //**|
      d
    } else 
      null
  def process1(context:C) = sampler.process1(context)
}

/** A collection of samplers that might play beautiful music together.  
    Can you call this.process passing AnyRef, and the suite will offer each sampler (in order) the opportunity to handle this context.
    The first sampler in the suite to accept it, gets it.  
    It is envisioned that the individual samplers in the suite may send variables back to the suite or coordinate among the suite. 
    @author Andrew McCallum
*/
class SamplerSuite extends ArrayBuffer[GenericSampler[_]] with Sampler[AnyRef] with cc.factorie.util.Trackable {
  /*def this() = this(Nil)
  def this(ss:Sampler[_]*) = this(ss)
  this ++= ss*/
  
  def process1(context:AnyRef) : DiffList = {
    val samplers = this.iterator
    while (samplers.hasNext) {
      //|**("SamplerSuite")
      val sampler = samplers.next
      //println("SamplerSuite context "+context+" sampler "+sampler.sampler)
      val d:DiffList = sampler.process0(context)
      //**|
      if (d != null) {
        //println("SamplerSuite sampler "+sampler.sampler+" diff "+d)
        return d
      }
    }
    return null
  }
  
  override def noDiffList: this.type = {
    this.foreach(_.sampler.noDiffList)
    super.noDiffList
  }
  
}
