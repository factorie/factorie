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



package cc.factorie.example
import scala.xml._
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import cc.factorie._
import cc.factorie.app.tokenseq._
import cc.factorie.app.tokenseq.labeled
import cc.factorie.er._

object Football {
  val printLexiconsOnly = false

  // Define the variable classes
  class Token(word:String, labelString:String) extends labeled.Token[Sentence,Label,Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(tag:String, token:Token) extends labeled.Label[Sentence,Token,Label](tag, token)
  class Sentence extends labeled.TokenSeq[Token,Label,Sentence]

  // Define the model
  val model = new Model(
    Foreach[Label] { label => Score(label) },
    Foreach[Label] { label => Score(label, label.token) },
    Foreach[Label] { label => Score(label.prev, label, label.token) },
    Foreach[Label] { label => Score(label.prev, label) }
  )
  
  // Define the objective function
  val lexiconDir = "/Users/mccallum/research/projects/football/lexicons/"
  val perLexicon = new Lexicon(lexiconDir+"people")
  val orgLexicon = new Lexicon(lexiconDir+"orgs")
  //val positionLexicon = new Lexicon(lexiconDir+"positions")
  val objective = new Model(
    new TemplateWithVectorStatistics1[Label] {
      val oIndex = Domain[Label].index("O")
      val orgIndex = Domain[Label].index("ORG")
      val perIndex = Domain[Label].index("PER")
      def score(s:Stat) = {
        val label: Label = s._1
        val token = label.token
        var result = 0.0
        val perLex = perLexicon.contains(token)
        val orgLex = orgLexicon.contains(token)
        val perLes = perLexicon.containsSingle(token)
        val orgLes = orgLexicon.containsSingle(token)
        //if (perLex) println("PERLEX "+token.word+"  "+token.window(2).map(_.word).toList)
        //if (orgLex) println("ORGLEX "+token.word+"  "+token.window(2).map(_.word).toList)
        //if (perLes) println("PERLES "+token.word)
        if (label.intValue == perIndex) { if (perLex) result += 1.0 else if (token.isCapitalized && token.word.length > 1 && perLes) result += 0.3 else result -= 1.0 }
        //if (label.index == perIndex) { if (perLex) result += 1.0 else if (token.isCapitalized && token.word.length > 1 && perLes) result += 0.3 }
        if (label.intValue == orgIndex) { if (orgLex) result += 1.0 else if (token.isCapitalized && token.word.length > 1 && orgLes) result += 0.3 else result -= 1.0 }
        if (label.intValue == orgIndex) { if (orgLex) result += 1.0 else if (token.isCapitalized && token.word.length > 1 && orgLes) result += 0.3 }
        if (label.intValue == oIndex) { if (orgLex || perLex) result -= 1.0 }
        if (!token.isCapitalized || token.isPunctuation || token.isDigits) if (label.intValue == oIndex) result += 0.5 else result -= 1.5
        if (token.isCapitalized && label.intValue != oIndex && token.hasPrev && token.prev.word == ".") result -= 0.5
        //if (token.isCapitalized && label.index == orgIndex && orgLes) result += 0.3
        //if (token.isCapitalized && label.index == perIndex && perLes) result += 0.3
        result
      }
    }
  )
  
  
  
  
  def main(args:Array[String]): Unit = {
    val directories: Seq[String] = 
      if (args.length > 1) args 
      else if (printLexiconsOnly) List("/Users/mccallum/research/projects/football/data") 
      else List(
        "/Users/mccallum/research/projects/football/data/1987/12/01",
        "/Users/mccallum/research/projects/football/data/1987/12/02",
        "/Users/mccallum/research/projects/football/data/1987/12/03",
        "/Users/mccallum/research/projects/football/data/1987/12/04",
        "/Users/mccallum/research/projects/football/data/1987/12/05",
        "/Users/mccallum/research/projects/football/data/1987/12/06",
        "/Users/mccallum/research/projects/football/data/1987/12/07",
        "/Users/mccallum/research/projects/football/data/1987/12/08",
        "/Users/mccallum/research/projects/football/data/1987/12/09",
        "/Users/mccallum/research/projects/football/data/1987/12/10",
        "/Users/mccallum/research/projects/football/data/1987/12/11",
        "/Users/mccallum/research/projects/football/data/1987/12/12",
        "/Users/mccallum/research/projects/football/data/1987/12/13",
        "/Users/mccallum/research/projects/football/data/1987/12/14"
        )
    Domain[Label].index("PER")
    Domain[Label].index("ORG")
    // Read in the data
    val documents = new ArrayBuffer[Sentence]
    for (dirname <- directories) {
      for (file <- files(new File(dirname))) {
        val article = XML.loadFile(file)
        //println(article \\ "head" \\ "title" text)
        //println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
        val content = article \\ "head" \\ "docdata" \\ "identified-content"
        if (printLexiconsOnly) printLexicons(content)
        else {
          print("Reading ***"+(article\\"head"\\"title").text+"***")
          documents +=
            labeled.TokenSeq.fromPlainText(Source.fromString((article \\ "body" \\ "body.content").text), () => new Sentence, (word,lab)=>new Token(word,lab), "O", 
                featureFunction, "[A-Za-z0-9]+|\\p{Punct}".r)
          println("  "+documents.last.size)
          documents.last.foreach(t=> print(t.word+" ")); println
        }  
      }
    }
    if (printLexiconsOnly) System.exit(0)
    documents.foreach(d => d.addNeighboringFeatureConjunctions(List(-1), List(-1,0), List(0), List(1)))
    documents.foreach(d => d.foreach(t => t ++= charNGrams(t.word, 2,5).map(g => "NGRAMS="+g)))
    
    // Train and print diagnostics
    val labels = documents.flatMap(seq => seq.map(_.label))
    val learner = new VariableSettingsSampler[Label](model,objective) with SampleRank with GradientAscentUpdates {
      override def postIterationHook(): Boolean = {
        println("Iteration "+iterationCount)
        var docCount = 1
        for (doc <- documents) {
          println("Document "+docCount)
          for (entity <- doc.entities) { 
            println(entity._1+" "+entity._2.map(t=>t.word).mkString(" "))
          }
          docCount += 1
        } 
        true
      }
    }
    learner.processAll(labels, 40)
  }
  
  
  // Helper functions
  
  def featureFunction(in:Seq[String]): Seq[String] = {
    val word = in.head
    val result = new ArrayBuffer[String]
    if (!word.matches("\\d+")) result += "W="+word
    result += "SHAPE="+wordShape(word, 2)
    result
  }
  def printLexicons(content:NodeSeq): Unit = {
    for (person <- content \\ "person") {
      val names = person.text.trim.replaceAll(" *\\(.*\\)","").replaceAll(" JR","").split("\\s*,\\s*")
      val namesr = names.reverse
      println("p "+namesr.mkString(" "))
      if (namesr.length == 3 && namesr(1).length == 1) println("p "+namesr(0)+" "+namesr(2)) // also print names without the middle initial
    }
    for (org <- content \\ "org") {
      val names = org.text.trim.replaceAll("\\n"," ").replaceAll("ASSN", "ASSOCIATION").replaceAll(" *\\(.*\\)","").split("\\s*,\\s*")
      println("o "+names.reverse.mkString(" "))
    }
  }
  def files(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }
}
