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

package cc.factorie.app.bib
import cc.factorie.util.Cubbie
import cc.factorie.app.nlp.coref.{Entity, HasCanopyAttributes, Prioritizable}
import collection.mutable.HashMap

trait EntityCubbie extends Cubbie {
  val canopies = new StringListSlot("canopies")
  val inferencePriority = new DoubleSlot("ipriority")
  val parentRef = RefSlot("parentRef", () => newEntityCubbie)
  def newEntityCubbie:EntityCubbie
  def fetch(e:Entity with HasCanopyAttributes[_] with Prioritizable) ={
    e.priority = inferencePriority.value
  }
  def store[T<:Entity with HasCanopyAttributes[T] with Prioritizable](e:T) ={
    canopies := e.canopyAttributes.map(_.canopyName).toSeq
    inferencePriority := e.priority
  }
}
class BagOfWordsCubbie extends Cubbie{
  def store(bag:BagOfWords) = {_map ++= bag.asHashMap;this}
  def fetch:HashMap[String,Double] = {
    val result = new HashMap[String,Double]
    for((k,v) <- _map)result += k -> v.toString.toDouble
    result
  }
}
class FieldsCubbie extends Cubbie
class PersonCubbie extends EntityCubbie{ // Split from AuthorCubbie eventually
  def newEntityCubbie:EntityCubbie = new PersonCubbie
}
class AuthorCubbie extends EntityCubbie {
  protected var _author:AuthorEntity=null
  val isMention = BooleanSlot("mention")
  val firstName = StringSlot("fn")
  val middleName = StringSlot("mn")
  val lastName = StringSlot("ln")
  val suffix = StringSlot("sf")
  //TODO: maybe name should be a bag, there may be multiple nick names
  val nickName = StringSlot("nn") // nickname  e.g. William Bruce Croft, nickname=Bruce; or William Freeman, nickname=Bill
  val emails = new CubbieSlot("emails", () => new BagOfWordsCubbie)
  val topics = new CubbieSlot("topics", () => new BagOfWordsCubbie)
  val keywords = new CubbieSlot("keywords", () => new BagOfWordsCubbie)
  val venues = new CubbieSlot("venues", () => new BagOfWordsCubbie)
  val coauthors = new CubbieSlot("coauthors", () => new BagOfWordsCubbie)
  val pid = RefSlot("pid", () => new PaperCubbie) // paper id; set in author mentions, propagated up into entities
  def fetch(e:AuthorEntity) ={
    super.fetch(e)
    e.attr[FullName].setFirst(firstName.value)(null)
    e.attr[FullName].setMiddle(middleName.value)(null)
    e.attr[FullName].setLast(lastName.value)(null)
    e.attr[FullName].setSuffix(suffix.value)(null)
    //e.attr[FullName].setNickName(nickName.value)(null)
    e.attr[BagOfTopics] ++= topics.value.fetch
    e.attr[BagOfVenues] ++= venues.value.fetch
    e.attr[BagOfCoAuthors] ++= coauthors.value.fetch
    e.attr[BagOfKeywords] ++= keywords.value.fetch
    e.attr[BagOfEmails] ++= emails.value.fetch
    e._id = this.id.toString
    _author=e
  }
  def store(e:AuthorEntity) ={
    firstName := e.attr[FullName].firstName
    middleName := e.attr[FullName].middleName
    lastName := e.attr[FullName].lastName
    topics := new BagOfWordsCubbie().store(e.attr[BagOfTopics].value)
    venues := new BagOfWordsCubbie().store(e.attr[BagOfVenues].value)
    coauthors := new BagOfWordsCubbie().store(e.attr[BagOfCoAuthors].value)
    keywords := new BagOfWordsCubbie().store(e.attr[BagOfKeywords].value)
    emails := new BagOfWordsCubbie().store(e.attr[BagOfEmails].value)
    this.id=e.id
  }
  def author:AuthorEntity=_author
  override def newEntityCubbie:EntityCubbie = new AuthorCubbie
}
// superclass of PaperCubbie and CommentCubbie
class EssayCubbie extends Cubbie {
  val created = DateSlot("created")
  val modified = DateSlot("modified")
  val kind = StringSlot("kind") // article, inproceedings, patent, synthetic (for creating author coref edit), comment, review,...
  val title = StringSlot("title")
  val abs = StringSlot("abs") // abstract
  //should authors go up here?
}
// Articles, Patents, Proposals,...
class PaperCubbie extends EssayCubbie with EntityCubbie {
  protected var _paper:PaperEntity=null
  val authors = StringListSlot("authors") // should this be in essay cubbie?
  val institution = StringSlot("institution")
  val venue = StringSlot("venue") // booktitle, journal,...
  val series = StringSlot("series")
  val year = IntSlot("year")
  val keywords = new CubbieSlot("keywords", () => new BagOfWordsCubbie)
  val volume = IntSlot("volume")
  val number = IntSlot("number")
  val chapter = StringSlot("chapter")
  val pages = StringSlot("pages")
  val editor = StringSlot("editor")
  //val address = StringSlot("address") // An attribute of the venue?
  val edition = StringSlot("edition")
  val url = StringSlot("url") // But we want to be able to display multiple URLs in the web interface
  val pid = RefSlot("pid", () => new PaperCubbie) // paper id; the paper mention chosen as the canonical child
  def paper:PaperEntity=_paper
  def newEntityCubbie:EntityCubbie = new PaperCubbie
}
class BibTeXCubbie{
  //all bibtex
}
class VenueCubbie extends Cubbie {
  
}

// For web site user comments, tags and ratings
//  Note that this is not a "paper", 
//  but I think we want web users to be able to comment on and cite comments as if their were papers
//  So in that case, a comment-on-comment would have pid equal to the _id of the first comment?
//  But a CommentCubbie is not a PaperCubbie; so we introduce the "cid" field.
// Still, consider making a common superclass of PaperCubbie and CommentCubbie.
class CommentCubbie extends EssayCubbie {
  val rating = IntSlot("rating") // multiple dimensions, like in many conference's paper review forms?
  // Should comment authors also be able to tag their comments?
  val cid = RefSlot("cid", () => new CommentCubbie) // comment id, for comment-on-comment
  val pid = RefSlot("pid", () => new PaperCubbie) // paper id
}

class TagCubbie extends Cubbie {
  val created = DateSlot("created")
  val userid = StringSlot("userid")
  val tag = StringSlot("tag")
  val eid = RefSlot("eid", () => new EssayCubbie) // essay id
}

// TODO Remove this, not used.
class RedirCubbie extends Cubbie {
  val src = RefSlot("src", () => new PaperCubbie)
  val dst = RefSlot("dst", () => new PaperCubbie)
}
