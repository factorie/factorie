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
import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import scala.util.Random
import cc.factorie.la._
import scala.reflect.Manifest
import java.io._
import util.ClassPathUtils

/** The "domain" of a variable---a representation of all of its values, each having type 'ValueType'.
    This most generic superclass of all Domains does not provide much functionality.
    Key functionality of subclasses:
    VectorDomain provides the maximum dimensionality of its vectors.
    DiscreteVectorDomain have a dimensionDomain:DiscreteDomain,
     providing a size and DiscreteValue objects.
    DiscreteDomain extends DiscreteVectorDomain, having single value members: DiscreteValue;
     it is its own dimensionDomain.
    CategoricalDomain provides a densely-packed mapping between category values and integers.
    @author Andrew McCallum
    @since 0.8 */
trait Domain[+VT] extends ValueType[VT] {
  /** Serialize this domain to disk in the given directory. */
  def save(dirname:String): Unit = {}
  /** Deserialize this domain from disk in the given directory. */
  def load(dirname:String): Unit = {}
  /** The name of the file (in directory specified in "save" and "load") to which this Domain is saved. */
  def filename:String = this.getClass.getName
}

/** A domain that provides an Iterable[] over its values. */
trait IterableDomain[+VT] extends Domain[VT] {
  def values: Iterable[VT]
}


/** The domain object for variables that don't have a meaningful domain. */  // TODO Explain this better; see Vars and SpanVariable
object GenericDomain extends Domain[Any]

/** Add this trait to a Variable to give it a Domain with Value type VT. */
trait VarAndValueGenericDomain[+This<:Variable,+VT] extends VarAndValueType[This,VT] {
  this: This =>
  //type ValueType = VT
  def domain = GenericDomain.asInstanceOf[Domain[VT]]
}
