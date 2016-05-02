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

package cc.factorie.variable


/** The "domain" of a variable---a representation of all of its values, each having type 'ValueType'.
    This most generic superclass of all Domains does not provide much functionality.
    Key functionality of subclasses:
    DiscreteVectorDomain have a dimensionDomain:DiscreteDomain,
     providing a size and DiscreteValue objects.
    DiscreteDomain extends DiscreteVectorDomain, having single value members: DiscreteValue;
     it is its own dimensionDomain.
    CategoricalDomain provides a densely-packed mapping between category values and integers.
    @author Andrew McCallum
    @since 0.8 */
trait Domain extends Serializable {
  type Value <: Any
}
// TODO Consider removing type argument from Domain?  
// It does provide a nice check between Value of Var and Domain, but I'm not sure how useful a check it is? -akm

/** A domain that provides (and is itself) an Iterable[] over its values.
    @author Andrew McCallum */
trait IterableDomain[+A] extends Domain with Iterable[A] {}

