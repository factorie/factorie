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
import cc.factorie._
import cc.factorie.optimize._
import cc.factorie.app.chain._
import cc.factorie.app.nlp._
import scala.xml._
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Football {
  val printLexiconsOnly = false

  // Define the variable classes
  object TokenFeaturesDomain extends CategoricalTensorDomain[String]
  class TokenFeatures(val token:Token) extends BinaryFeatureVectorVariable[String] {
    def domain = TokenFeaturesDomain
  }
  object LabelDomain extends CategoricalDomain[String]
  class Label(tag:String, val token:Token) extends LabeledCategoricalVariable(tag) {
    def domain = LabelDomain
  }

  // Define the model
  val model = new CombinedModel(
    //Foreach[Label] { label => Score(label) },
    new DotTemplateWithStatistics1[Label] {
      //def statisticsDomains = Tuple1(LabelDomain)
      lazy val weights = new la.DenseTensor1(LabelDomain.size)
    },
    //Foreach[Label] { label => Score(label, label.token) },
    new DotTemplateWithStatistics2[Label,TokenFeatures] {
      //def statisticsDomains = ((LabelDomain, TokenFeaturesDomain))
      lazy val weights = new la.DenseTensor2(LabelDomain.size, TokenFeaturesDomain.dimensionSize)
      def unroll1(label:Label) = Factor(label, label.token.attr[TokenFeatures])
      def unroll2(tf:TokenFeatures) = throw new Error()
    },
    //Foreach[Label] { label => Score(label.prev, label, label.token) },
    new DotTemplateWithStatistics3[Label,Label,TokenFeatures] {
      //def statisticsDomains = ((LabelDomain, LabelDomain, TokenFeaturesDomain))
      lazy val weights = new la.DenseTensor3(LabelDomain.size, LabelDomain.size, TokenFeaturesDomain.dimensionSize)
      def unroll1(label:Label) = Factor(label, label.token.next.attr[Label], label.token.next.attr[TokenFeatures])
      def unroll2(label:Label) = Factor(label.token.prev.attr[Label], label, label.token.attr[TokenFeatures])
      def unroll3(tf:TokenFeatures) = throw new Error()
    },
    //Foreach[Label] { label => Score(label.prev, label) }
    new DotTemplateWithStatistics2[Label,Label] {
      def statisticsDomains = ((LabelDomain, LabelDomain))
      lazy val weights = new la.DenseTensor2(LabelDomain.size, LabelDomain.size)
      def unroll1(label:Label) = Factor(label, label.token.next.attr[Label])
      def unroll2(label:Label) = Factor(label.token.prev.attr[Label], label)
    }

  )
  
  // Define the objective function
  val lexiconDir = "/Users/mccallum/research/projects/football/lexicons/"
  val perLexicon = new Lexicon(lexiconDir+"people")
  val orgLexicon = new Lexicon(lexiconDir+"orgs")
  //val positionLexicon = new Lexicon(lexiconDir+"positions")
  throw new Error("Objective needs to be re-implemented.")
  val objective = new CombinedModel(
    /*new TemplateWithVectorStatistics1[Label] {
      val oIndex = LabelDomain.index("O")
      val orgIndex = LabelDomain.index("ORG")
      val perIndex = LabelDomain.index("PER")
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
    }*/
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
    LabelDomain.index("PER")
    LabelDomain.index("ORG")
    // Read in the data
    val documents = new ArrayBuffer[cc.factorie.app.nlp.Document]
    for (dirname <- directories) {
      for (file <- files(new File(dirname))) {
        val article = XML.loadFile(file)
        //println(article \\ "head" \\ "title" text)
        //println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
        val content = article \\ "head" \\ "docdata" \\ "identified-content"
        if (printLexiconsOnly) printLexicons(content)
        else {
          print("Reading ***"+(article\\"head"\\"title").text+"***")
          documents += LoadPlainText.fromString(file.getName, Source.fromString((article \\ "body" \\ "body.content").text).mkString, false)
          for (token <- documents.last.tokens) {
            token.attr += new Label("O", token)
            token.attr += new TokenFeatures(token) ++= featureFunction(token.string)
          }
          println("  "+documents.last.length)
          documents.last.tokens.foreach(t=> print(t.string+" ")); println
        }  
      }
    }
    if (printLexiconsOnly) System.exit(0)
    documents.foreach(d => Observations.addNeighboringFeatureConjunctions(d.tokens, (t:Token) => t.attr[TokenFeatures], List(-1), List(-1,0), List(0), List(1)))
    documents.foreach(d => d.tokens.foreach(t => t.attr[TokenFeatures] ++= cc.factorie.app.strings.charNGrams(t.string, 2,5).map(g => "NGRAMS="+g)))
    
    // Train and print diagnostics
    val labels = documents.map(_.tokens).flatMap(seq => seq.map(_.attr[Label]))
    val sampler = new GibbsSampler(model, objective) {
      override def postIterationHook(): Boolean = {
        println("Iteration "+iterationCount)
        var docCount = 1
        for (doc <- documents) {
          println("Document "+docCount)
          for (entity <- Observations.extractContiguous(doc.tokens, (t:Token) => t.attr[Label].categoryValue)) { 
            println(entity._1+" "+entity._2.map(t=>t.string).mkString(" "))
          }
          docCount += 1
        } 
        true
      }
    }
    val learner = new SampleRank(sampler, new StepwiseGradientAscent)
    learner.processAll(labels, 40)
  }
  
  
  // Helper functions
  
  def featureFunction(word:String): Seq[String] = {
    val result = new ArrayBuffer[String]
    if (!word.matches("\\d+")) result += "W="+word
    result += "SHAPE="+cc.factorie.app.strings.stringShape(word, 2)
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
