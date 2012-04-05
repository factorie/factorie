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

trait EntityCubbie extends Cubbie {
  val canopies = new StringListSlot("canopies")
  val inferencePriority = new DoubleSlot("ipriority")
}

class BagOfWordsCubbie extends Cubbie
class FieldsCubbie extends Cubbie

class PersonCubbie extends EntityCubbie // Split from AuthorCubbie eventually

class AuthorCubbie extends EntityCubbie {
  val isMention = BooleanSlot("mention")
  val firstName = StringSlot("fn")
  val middleName = StringSlot("mn")
  val lastName = StringSlot("ln")
  val nickName = StringSlot("nn") // nickname  e.g. William Bruce Croft, nickname=Bruce; or William Freeman, nickname=Bill
  val emails = new CubbieSlot("emails", () => new BagOfWordsCubbie)
  val topics = new CubbieSlot("topics", () => new BagOfWordsCubbie)
  val keywords = new CubbieSlot("keywords", () => new BagOfWordsCubbie)
  val venues = new CubbieSlot("venues", () => new BagOfWordsCubbie)
  val coauthors = new CubbieSlot("coauthors", () => new BagOfWordsCubbie)
  val pid = RefSlot("pid", () => new PaperCubbie) // paper id; set in author mentions, propagated up into entities
}

// superclass of PaperCubbie and CommentCubbie
class EssayCubbie extends Cubbie {
  val created = DateSlot("created")
  val modified = DateSlot("modified")
  val kind = StringSlot("kind") // article, inproceedings, patent, synthetic (for creating author coref edit), comment, review,...
  val title = StringSlot("title")
  val abs = StringSlot("abs") // abstract
}

// Articles, Patents, Proposals,...
class PaperCubbie extends EssayCubbie with EntityCubbie {
  val authors = StringListSlot("authors") // 
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