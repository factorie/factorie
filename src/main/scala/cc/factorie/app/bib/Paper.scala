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
import collection.mutable.HashMap
import cc.factorie.app.nlp.coref._


trait EntityCubbie[T<:HierEntity with HasCanopyAttributes[T] with Prioritizable] extends Cubbie {
  val canopies = new StringListSlot("canopies")
  val inferencePriority = new DoubleSlot("ipriority")
  val parentRef = RefSlot("parentRef", () => newEntityCubbie)
  val isMention = BooleanSlot("isMention")
  val groundTruth = new StringSlot("gt")
  val bagOfTruths = new CubbieSlot("gtbag", () => new BagOfWordsCubbie)
  def newEntityCubbie:EntityCubbie[T]
  def fetch(e:T) ={
    e.priority = inferencePriority.value
    e.attr[IsMention].set(isMention.value)(null)
    e.isObserved=isMention.value
    e.attr[IsEntity].set(e.isRoot)(null)
    e.attr[EntityExists].set(e.isConnected)(null)
    if(groundTruth.isDefined)e.groundTruth = Some(groundTruth.value)
    if(bagOfTruths.isDefined && e.attr[BagOfTruths]!=null)e.attr[BagOfTruths] ++= bagOfTruths.value.fetch
    //note that entity parents are set externally not inside the cubbie
  }
  def store(e:T) ={
    this.id=e.id      
    canopies := e.canopyAttributes.map(_.canopyName).toSeq
    inferencePriority := e.priority
    isMention := e.attr[IsMention].booleanValue
    if(e.parentEntity!=null)parentRef := e.parentEntity.id
    if(e.groundTruth != None)groundTruth := e.groundTruth.get
  }
}

class FieldsCubbie extends Cubbie
/*
class PersonCubbie extends EntityCubbie{ // Split from AuthorCubbie eventually
  def newEntityCubbie:EntityCubbie = new PersonCubbie
}
*/
class AuthorCubbie extends EntityCubbie[AuthorEntity] {
  protected var _author:AuthorEntity=null

 val index = IntSlot("idx")
  val firstName = StringSlot("fn")
  val middleName = StringSlot("mn")
  val lastName = StringSlot("ln")
  val suffix = StringSlot("sf")
  val firstNameBag = new CubbieSlot("fnb", () => new BagOfWordsCubbie)
  val middleNameBag = new CubbieSlot("mnb", () => new BagOfWordsCubbie)
  //val bagOfTruths = new CubbieSlot("gtbag", () => new BagOfWordsCubbie)
  //TODO: maybe name should be a bag, there may be multiple nick names
  val nickName = StringSlot("nn") // nickname  e.g. William Bruce Croft, nickname=Bruce; or William Freeman, nickname=Bill
  val emails = new CubbieSlot("emails", () => new BagOfWordsCubbie)
  val topics = new CubbieSlot("topics", () => new BagOfWordsCubbie)
  val keywords = new CubbieSlot("keywords", () => new BagOfWordsCubbie)
  val venues = new CubbieSlot("venues", () => new BagOfWordsCubbie)
  val coauthors = new CubbieSlot("coauthors", () => new BagOfWordsCubbie)
  //
  //val topicsTensor = new CubbieSlot("topicst", () => new BagOfWordsCubbie)
  //val keywordsTensor = new CubbieSlot("keywordst", () => new BagOfWordsCubbie)
  //val venuesTensor = new CubbieSlot("venuest", () => new BagOfWordsCubbie)
  //val coauthorsTensor = new CubbieSlot("coauthorst", () => new BagOfWordsCubbie)
  val pid = RefSlot("pid", () => new PaperCubbie) // paper id; set in author mentions, propagated up into entities
//  val groundTruth = new StringSlot("gt")
  override def fetch(e:AuthorEntity) ={
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
    e.attr[BagOfFirstNames] ++= firstNameBag.value.fetch
    e.attr[BagOfMiddleNames] ++= middleNameBag.value.fetch
    //
//    e.attr[TensorBagOfTopics] ++= topicsTensor.value.fetch
//    e.attr[TensorBagOfVenues] ++= venuesTensor.value.fetch
//    e.attr[TensorBagOfCoAuthors] ++= coauthorsTensor.value.fetch
//    e.attr[TensorBagOfKeywords] ++= keywordsTensor.value.fetch

    e._id = this.id.toString
    if(pid.isDefined)e.paperMentionId = pid.value.toString
//    if(groundTruth.isDefined)e.groundTruth = Some(groundTruth.value)
    _author=e
  }
  override def store(e:AuthorEntity) ={
    super.store(e)
    firstName := e.attr[FullName].firstName
    middleName := e.attr[FullName].middleName
    lastName := e.attr[FullName].lastName
    suffix := e.attr[FullName].suffix
    topics := new BagOfWordsCubbie().store(e.attr[BagOfTopics].value)
    venues := new BagOfWordsCubbie().store(e.attr[BagOfVenues].value)
    coauthors := new BagOfWordsCubbie().store(e.attr[BagOfCoAuthors].value)
    keywords := new BagOfWordsCubbie().store(e.attr[BagOfKeywords].value)
    emails := new BagOfWordsCubbie().store(e.attr[BagOfEmails].value)
    firstNameBag := new BagOfWordsCubbie().store(e.attr[BagOfFirstNames].value)
    middleNameBag := new BagOfWordsCubbie().store(e.attr[BagOfMiddleNames].value)
    //
//    topicsTensor := new BagOfWordsCubbie().store(e.attr[BagOfTopics].value)
//    venuesTensor := new BagOfWordsCubbie().store(e.attr[BagOfVenues].value)
//    coauthorsTensor := new BagOfWordsCubbie().store(e.attr[BagOfCoAuthors].value)
//    keywordsTensor := new BagOfWordsCubbie().store(e.attr[BagOfKeywords].value)

    if(e.attr[BagOfTruths]!=null && e.attr[BagOfTruths].value.size>0)bagOfTruths := new BagOfWordsCubbie().store(e.attr[BagOfTruths].value)
    this.id=e.id
    //println("pid: "+e.paperMentionId)
    if(e.paperMentionId != null)pid := e.paperMentionId
    if(!e.isObserved && e.paperMentionId!=null)println("Warning: non-mention-author with id "+e.id+ " has a non-null promoted mention.")
    //if(e.groundTruth != None)groundTruth := e.groundTruth.get
  }
  def author:AuthorEntity=_author
  override def newEntityCubbie:EntityCubbie[AuthorEntity] = new AuthorCubbie
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
class PaperCubbie extends EssayCubbie with EntityCubbie[PaperEntity] {
  protected var _paper:PaperEntity=null
  val authors = new CubbieSlot("authors", () => new BagOfWordsCubbie)
  val topics = new CubbieSlot("topics", () => new BagOfWordsCubbie)
  val venueBag = new CubbieSlot("venueBag", () => new BagOfWordsCubbie)
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
  def newEntityCubbie:EntityCubbie[PaperEntity] = new PaperCubbie
  override def fetch(e:PaperEntity) ={
    super.fetch(e)
    e.attr[Title].set(title.value)(null)
    if(pid.isDefined)e.promotedMention.set(pid.value.toString)(null) else e.promotedMention.set(null.asInstanceOf[String])(null)
    e.attr[BagOfTopics] ++= topics.value.fetch
    e.attr[BagOfAuthors] ++= authors.value.fetch
    e.attr[BagOfAuthors] ++= venueBag.value.fetch
    e.attr[BagOfAuthors] ++= keywords.value.fetch
    e._id = this.id.toString
  }
  override def store(e:PaperEntity) ={
    super.store(e)
    title := e.attr[Title].value
    if(e.promotedMention.value!=null)pid := e.promotedMention.value
    if(!e.isEntity && e.promotedMention.value!=null)println("Warning: non-entity-paper with id "+e.id+ " has a non-null promoted mention.")
    topics := new BagOfWordsCubbie().store(e.attr[BagOfTopics].value)
    authors := new BagOfWordsCubbie().store(e.attr[BagOfAuthors].value)
    venueBag := new BagOfWordsCubbie().store(e.attr[BagOfAuthors].value)
    keywords := new BagOfWordsCubbie().store(e.attr[BagOfAuthors].value)
    this.id=e.id
  }
}
class BibTeXCubbie{

  //all bibtex
  /*
address: Publisher's address (usually just the city, but can be the full address for lesser-known publishers)
annote: An annotation for annotated bibliography styles (not typical)
author: The name(s) of the author(s) (in the case of more than one author, separated by and)
booktitle: The title of the book, if only part of it is being cited
chapter: The chapter number
crossref: The key of the cross-referenced entry
edition: The edition of a book, long form (such as "first" or "second")
editor: The name(s) of the editor(s)
eprint: A specification of an electronic publication, often a preprint or a technical report
howpublished: How it was published, if the publishing method is nonstandard
institution: The institution that was involved in the publishing, but not necessarily the publisher
journal: The journal or magazine the work was published in
key: A hidden field used for specifying or overriding the alphabetical order of entries (when the "author" and "editor" fields are missing). Note that this is very different from the key (mentioned just after this list) that is used to cite or cross-reference the entry.
month: The month of publication (or, if unpublished, the month of creation)
note: Miscellaneous extra information
number: The "(issue) number" of a journal, magazine, or tech-report, if applicable. (Most publications have a "volume", but no "number" field.)
organization: The conference sponsor
pages: Page numbers, separated either by commas or double-hyphens.
publisher: The publisher's name
school: The school where the thesis was written
series: The series of books the book was published in (e.g. "The Hardy Boys" or "Lecture Notes in Computer Science")
title: The title of the work
type: The field overriding the default type of publication (e.g. "Research Note" for techreport, "{PhD} dissertation" for phdthesis, "Section" for inbook/incollection)
url: The WWW address
volume: The volume of a journal or multi-volume book
year: The year of publication (or, if unpublished, the year of creation)

   */
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
