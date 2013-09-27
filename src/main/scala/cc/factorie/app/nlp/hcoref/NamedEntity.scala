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

package cc.factorie.app.nlp.hcoref
import cc.factorie._
import cc.factorie.app.nlp._
import scala.collection.mutable.{ArrayBuffer,ListBuffer}
import cc.factorie.util.{Cubbie,CubbieRefs}
import cc.factorie.util.Attr
import cc.factorie.variable.StringVariable

// TODO Consider not having NamedEntity at all, but just adding a EntityName attribute to any Entity that needs it. -akm

/** The string-valued name for an entity. */
class EntityName(val entity:Entity, s:String) extends StringVariable(s)

/** An entity represented by a string-valued name. */
class NamedEntity(s:String) extends Entity {
  attr += new EntityName(this, s)
  def name = attr[EntityName]
  @deprecated("Use name.value instead") def string = name.value
}

@deprecated("Use NamedEntity instead.") class EntityVariable(s:String) extends NamedEntity(s) 


class NamedEntityCubbie extends EntityCubbie {
  val name = StringSlot("name")
  def newEntity = new NamedEntity("UNINITIALIZED")
  def newEntityCubbie = new NamedEntityCubbie
  def storeNamedEntity(ne:NamedEntity): this.type = {
    storeEntity(ne)
    name := ne.name.value
    this
  }
  def fetchNamedEntity(cr:CubbieRefs): NamedEntity = {
    val ne = fetchEntity(cr).asInstanceOf[NamedEntity]
    ne.name.set(name.value)(null)
    ne
  }
}
