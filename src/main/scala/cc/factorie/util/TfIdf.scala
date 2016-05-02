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

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

import cc.factorie._

/**
 * Created by johnsullivan on 6/5/15.
 */
trait TfIdf extends Idf {
  this: Tf with Idf =>
  type IdfCounts = Map[String, Double]
  type Document = Map[String, Double]
  type Corpus = Iterable[Document]
  type Term = String

  def reweightCorpus(corpus: Corpus, thresholdOpt:Option[Int] = None) = {
    val idfs = idfCounts(corpus)
    corpus.map{ doc =>
      val rawTfidf = doc.map { case (term, _) =>
        term -> tfidf(term, doc, idfs)
      }
      thresholdOpt match {
        case Some(threshold) => rawTfidf.toSeq.sortBy{case(_, v) => - v}.take(threshold).toMap
        case None => rawTfidf
      }
    }
  }

  def tfidf(term: Term, document: Document, idfCounts: IdfCounts) = tf(term, document) * idf(term, idfCounts)

  def docTfIdf(document: Document, idfCounts: IdfCounts) = document.map { case (term, count) =>
      term -> tfidf(term, document, idfCounts)
  }
}

trait Tf {
  this: TfIdf =>
  def tf(term:Term, document: Document):Double
}

trait BinaryTf extends Tf {
  this: TfIdf =>
  def tf(term: Term, document: Document) = if(document.contains(term)) 1.0 else 0.0
}

trait RawTf extends Tf {
  this: TfIdf =>
  def tf(term: Term, document: Document) = document.getOrElse(term, 0.0)
}

trait ScaledTf extends Tf {
  this: TfIdf =>
  def tf(term: Term, document: Document) = document.getOrElse(term, 0.0) / document.keySet.size
}

trait LogNormTf extends Tf {
  this: TfIdf =>
  def tf(term: Term, document: Document) = if(document.getOrElse(term, 0.0) == 0.0) 0.0 else 1.0 + Math.log(document(term))
}

trait DoubleNormKTf extends Tf {
  this: TfIdf =>
  val k:Double
  assert(k >= 0.0 && k <= 1.0)
  def tf(term: Term, document: Document) = k + ((1 - k) * (document.getOrElse(term, 0.0) / document.values.max))
}

trait HalfNormTf extends DoubleNormKTf {
  this: TfIdf =>
  val k = 0.5
}

trait Idf {
  this: TfIdf =>

  def idfCounts(c:Corpus):IdfCounts

  def idfFromFile(filePath:String):IdfCounts = new BufferedReader(new FileReader(filePath)).toIterator.map { line =>
    val Array(term, sDouble(idf)) = line.split(":")
    term -> idf
  }.toMap
  def idfToFile(idfCounts: IdfCounts, filePath:String): Unit = {
    val wrt = new BufferedWriter(new FileWriter(filePath))
    idfCounts.zipWithIndex.foreach{ case ((term, idf), idx) =>
      wrt.write(term + ":" + idf)
      wrt.newLine()
      if(idx % 1000 == 0) {
        print(".")
        wrt.flush()
      }
    }
    wrt.flush()
    wrt.close()
  }

  def defaultValue(term: Term) = 1.5
  def idf(term: Term, idfCounts: IdfCounts) = idfCounts.getOrElse(term, defaultValue(term))
}

trait InverseFreqLaplaceIdf extends Idf {
  this: TfIdf =>

  val lambda:Double
  assert(lambda >= 0.0 && lambda <= 1.0)
  def idfCounts(c:Corpus):IdfCounts = {
    c.zipWithIndex.flatMap{ case (doc, docId) =>
      doc.keys.map(_ -> docId)
    }.groupBy(_._1).mapValues(_.size) // # of docs each term appears in
      .map{ case (term, numDocs) =>
      term -> Math.log(lambda + ( c.size.toDouble / (1 + numDocs)))
    }
  }
}

trait InverseFreqIdf extends InverseFreqLaplaceIdf {
  this: TfIdf =>
  val lambda = 0.0
}

trait InverseFreqSmoothIdf extends InverseFreqLaplaceIdf {
  this: TfIdf =>
  val lambda = 1.0
}

object TfIdfTest {
  def main(args:Array[String]) {

    val docs = Array("""The ribs and terrors in the whale
   Arched over me a dismal gloom
While all God’s sun-lit waves rolled by
   And left me deepening down to doom""",
      """I saw the opening maw of hell
   With endless pains and sorrows there
Which none but they that feel can tell
   Oh I was plunging to despair""",
      """In black distress I called my God
   When I could scarce believe him mine
He bowed his ear to my complaints
   No more the whale did me confine""",
      """With speed he flew to my relief
   As on a radiant dolphin borne
Awful  yet bright  as lightening shone
   The face of my Deliverer God""",
      """My song for ever shall record
   That terrible that joyful hour
I give the glory to my God,
   His all the mercy and the power""").map(_.split("\\s+").groupBy(identity).mapValues(_.size.toDouble))

    val tfidf = new TfIdf with HalfNormTf with InverseFreqIdf

    val idfs = tfidf.idfCounts(docs)
    println(idfs)

    docs.foreach {doc => println(tfidf.docTfIdf(doc, idfs))}

  }
}