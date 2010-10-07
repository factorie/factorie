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
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io.File

/** Example of model for dependency parsing.
    Not finished.  Overly simple inference (just sampling!), projectivity not maintained.
    But interesting example of flexibility of the "Template.statistics" method. */
object DepParsing1 {
  
  class Token(val word:String, posString:String, trueParentPosition:Int, trueLabelString:String) extends BinaryFeatureVectorVariable[String] with VarInSeq[Token] {
    val parent = new Node(this, trueParentPosition, trueLabelString)
    val pos = new POS(posString)
    override def toString = "Token("+word+":"+position+")"
  }

  class Node(val token:Token, val truePosition:Int, trueLabelString:String) extends RefVariable[Token] with TrueSetting with IterableSettings {
    lazy val trueValue: Token = seq(truePosition)
    def seq = token.seq
    val label = new Label(trueLabelString)
    def valueIsTruth = value == trueValue
    def setToTruth(implicit d:DiffList): Unit = set(trueValue)
    def parent = value
    /** Bool with value true if parent is to the right, false if parent is to the left. */
    def direction: Direction = if (parent.position > token.position) new Direction(true) else new Direction(false)
    def distance: Distance = new Distance(math.abs(parent.position - token.position))
    def position = value.position
    // Preserve projectivity when setting a new parent.  Not yet implemented.
    /*override def set(newParent:Token)(implicit d:DiffList): Unit = {
      super.set(newParent)
      val min = math.min(token.position, parent.position)
      val max = math.max(token.position, parent.position)
      for (b <- token.between(newParent)) {
        val bparent = b.parent
        if (bparent.value != null && (bparent.position < min || bparent.position > max))
          bparent.set(seq(Global.random.nextInt(max-min+1)+min))
      }
    }*/
    def setRandomly: Unit = { value = seq.sampleUniformly }
    def settings = new SettingIterator {
      var i = -1
      val max = token.seq.length - 1
      def hasNext = i < max // && !(i == token.position && i+1 == max)
      def next(difflist:DiffList) = { 
        i += 1; //if (i == token.position) i += 1;
        val d = newDiffList;
        //println("next seq.length="+seq.length)
        set(seq(i))(d); 
        d }
      def reset = i = -1
      override def variable : Node = Node.this
    }
  }

  class Direction(right:Boolean) extends BooleanObservation(right)

  class Distance(d:Int) extends DiscreteVariable {
    if (d < domain.size-1) set(d)(null) else set(domain.size-1)(null)
  }
  Domain[Distance].size = 6

  class POS(posString:String) extends CategoricalVariable(posString) 
  class Label(trueString:String) extends LabelVariable(trueString)
  class Sentence extends VariableSeq[Token] {
    this += new Token("<ROOT>", ".", 0, "ROOT")
  }
  
  val model = new Model(
    new Template1[Node] with DotStatistics2[Token,Token] with SparseWeights { def statistics(n:Node) = Stat(n.token, n.parent) },
    new Template1[Node] with DotStatistics1[POS] { def statistics(n:Node) = Stat(n.token.pos) },
    new Template1[Node] with DotStatistics1[POS] { def statistics(n:Node) = Stat(n.parent.pos) },
    new Template1[Node] with DotStatistics1[Token] { def statistics(n:Node) = Stat(n.token) },
    new Template1[Node] with DotStatistics1[Token] { def statistics(n:Node) = Stat(n.parent) },
    new Template1[Node] with DotStatistics2[POS,POS] { def statistics(n:Node) = Stat(n.token.pos, n.parent.pos) },
    new Template1[Node] with DotStatistics2[POS,Token] with SparseWeights { def statistics(n:Node) = Stat(n.token.pos, n.parent) },
    new Template1[Node] with DotStatistics2[Token,POS] with SparseWeights { def statistics(n:Node) = Stat(n.token, n.parent.pos) },
    new Template1[Node] with DotStatistics3[POS,POS,POS] { def statistics(n:Node) = for (b <- n.token.between(n.parent)) yield Stat(n.token.pos, b.pos, n.parent.pos) },
    new Template1[Node] with DotStatistics3[POS,POS,POS] { def statistics(n:Node) = if (n.token.hasPrev) Stat(n.token.prev.pos, n.token.pos, n.parent.pos) else Nil },
    new Template1[Node] with DotStatistics3[POS,POS,POS] { def statistics(n:Node) = if (n.token.hasNext) Stat(n.token.pos, n.token.next.pos, n.parent.pos) else Nil },
    new Template1[Node] with DotStatistics3[POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasPrev) Stat(n.token.pos, n.parent.prev.pos, n.parent.pos) else Nil },
    new Template1[Node] with DotStatistics3[POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasNext) Stat(n.token.pos, n.parent.pos, n.parent.next.pos) else Nil },
    new Template1[Node] with DotStatistics4[POS,POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasNext && n.token.hasNext) Stat(n.token.pos, n.token.next.pos, n.parent.pos, n.parent.next.pos) else Nil },
    new Template1[Node] with DotStatistics4[POS,POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasNext && n.token.hasPrev) Stat(n.token.pos, n.token.prev.pos, n.parent.pos, n.parent.next.pos) else Nil },
    new Template1[Node] with DotStatistics4[POS,POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasPrev && n.token.hasNext) Stat(n.token.pos, n.token.next.pos, n.parent.pos, n.parent.prev.pos) else Nil },
    new Template1[Node] with DotStatistics4[POS,POS,POS,POS] { def statistics(n:Node) = if (n.parent.hasPrev && n.token.hasPrev) Stat(n.token.pos, n.token.prev.pos, n.parent.pos, n.parent.prev.pos) else Nil },
    new Template1[Node] with DotStatistics2[Token,Direction] with SparseWeights { def statistics(n:Node) = Stat(n.token, n.direction) },
    new Template1[Node] with DotStatistics2[Direction,Distance] { def statistics(n:Node) = Stat(n.direction, n.distance) },
    new Template1[Node] with DotStatistics1[Direction] { def statistics(n:Node) = Stat(n.direction) },
    new Template1[Node] with DotStatistics1[Distance] { def statistics(n:Node) = Stat(n.distance) },
    new Template1[Node] with DotStatistics3[POS,Direction,Distance] { def statistics(n:Node) = Stat(n.token.pos, n.direction, n.distance) },
    new Template1[Node] with DotStatistics3[POS,POS,Direction] { def statistics(n:Node) = Stat(n.token.pos, n.parent.pos, n.direction) },
    new Template1[Node] with DotStatistics4[POS,POS,Direction,Distance] { def statistics(n:Node) = Stat(n.token.pos, n.parent.pos, n.direction, n.distance) }
  )
  
  val objective = new Model(
    new TemplateWithStatistics1[Node] {
      def score(s:Stat) = {
        val node = s._1
        //println("objective "+node.valueIsTruth)
        if (node.valueIsTruth) 1.0 else 0.0
        //-math.abs(node.position - node.truePosition)
      }
    }
  )
  
  def main(args:Array[String]): Unit = {
    val datafile = if (args.length > 0) args(0) else "/Users/mccallum/research/data/parsing/depparsing/train.owpl"
    val sentences = new ArrayBuffer[Sentence];
    var sentence = new Sentence
    val source = Source.fromFile(new File(datafile))
    for (line <- source.getLines()) {
      if (line.length < 2) {
        if (sentence.length > 0) { sentences += sentence; sentence = new Sentence }
      } else {
        val fields = line.split("\\s+")
        if (fields.length != 4) println("Skipping line: "+line)
        else {
          val word = fields(0)
          val pos = fields(1)
          val parentPosition = Integer.parseInt(fields(2))
          val token = new Token(word, pos, parentPosition, fields(3))
          val w = simplify(word)
          token += w //.substring(0, math.min(w.length, 6))
          //token += "POS="+pos
          sentence += token
        }
      }
    }
    println("Read "+sentences.length+" sentences, "+sentences.foldLeft(0)(_+_.length)+" words.")
    println("Domain[Token] size = "+Domain[Token].size)
    println("Domain[POS] size = "+Domain[POS].size)
    println("Domain[Distance] size = "+Domain[Distance].size)
    
    val nodes = sentences.flatMap(_.map(_.parent))
    nodes.foreach(_.setRandomly)
    val learner = new VariableSettingsSampler[Node](model, objective) with SampleRank with GradientAscentUpdates {
      //val learner = new VariableSettingsSampler[Node](objective)
      temperature = 0.01
      override def postIterationHook(): Boolean = {
        for (sentence <- sentences.take(20)) printSentence(sentence)
        println("Average score = "+objective.aveScore(nodes))
        super.postIterationHook
      }
      override def proposalsHook(proposals:Seq[Proposal]): Unit = {
        //proposals.foreach(p => println("%-6f %-6f %s".format(p.modelScore, p.objectiveScore, p.diff.toString)))
        val bestModel = proposals.maxByDouble(_.modelScore)
        val bestObjective = proposals.maxByDouble(_.objectiveScore)
        println(bestModel.diff)
        println(bestObjective.diff)
        println("modelBest modelScore "+bestModel.modelScore)
        println("modelBest objecScore "+bestModel.objectiveScore)
        println("objecBest modelScore "+bestObjective.modelScore)
        println("objecBest objecScore "+bestObjective.objectiveScore)
        super.proposalsHook(proposals)
      }
      override def postProcessHook(n:Node, d:DiffList): Unit = {
        printSentence(n.token.seq)
        super.postProcessHook(n,d)
      }
    }
    learner.processAll(nodes, 10)
    
  }
  
  case class Entry(score:Double, r:Int)
  def eisner(model:Model, sentence:Seq[Node]): Unit = {
    val length = sentence.length
    // Score of setting i's parent to j.
    val score = Array.tabulate(length-1, length-1)((i,j) => {
      val d = new DiffList
      sentence(i).set(null)(null) // just to make sure that we populate the DiffList
      sentence(i).set(sentence(j).token)(d)
      d.score(model)
    })
    // Chart of incomplete items, right and left
    val ir = Array.ofDim[Entry](length,length)
    val il = Array.ofDim[Entry](length,length)
    // Chart of complete items, right and left
    val cr = Array.ofDim[Entry](length,length)
    val cl = Array.ofDim[Entry](length,length)
    // Initialize the chart
    for (s <- 0 until length) {
      il(s)(s) = Entry(0.0, s)
      ir(s)(s) = Entry(0.0, s)
      cl(s)(s) = Entry(0.0, s)
      cr(s)(s) = Entry(0.0, s)
    }
    // Fill the chart
    for (k <- 0 until length; s <- 0 until length; val t = s + k; if (t < length)) {
      il(s)(t) = (for (r <- s until t) yield Entry(cr(s)(r).score + cl(r+1)(t).score + score(t)(s), r)).maxByDouble(_.score)
      ir(s)(t) = (for (r <- s until t) yield Entry(cr(s)(r).score + cl(r+1)(t).score + score(s)(t), r)).maxByDouble(_.score)
      cl(s)(t) = (for (r <- s until t) yield Entry(cl(s)(r).score + il(r)(t).score, r)).maxByDouble(_.score)
      cr(s)(t) = (for (r <- s+1 to  t) yield Entry(ir(s)(r).score + cr(r)(t).score, r)).maxByDouble(_.score)
    }
    // Use the chart to set the parents, recursively
    def setParents(start:Int, end:Int, parent:Token): Unit = {
      val mid = cr(start)(end).r
      sentence(mid).set(parent)(null)
      setParents(start, mid, sentence(mid).token)
      setParents(mid, end, sentence(mid).token)
    }
    setParents(0, length-1, sentence(0).token)
  }
  
  def printSentence(sentence:Seq[Token]): Unit = {
    for (token <- sentence) println("%-3d %-20s %-3d %-3d %s".
        format(token.position, token.word, token.parent.truePosition, token.parent.position,
        if (token.parent.valueIsTruth) " " else "*"))
    println("#correct = "+objective.score(sentence.map(_.parent)))
    println
  }
  
  def simplify(word:String): String = {
    if (word.matches("(19|20)\\d\\d")) "<YEAR>" 
    else if (word.matches("\\d+")) "<NUM>"
    else if (word.matches(".*\\d.*")) word.replaceAll("\\d","#").toLowerCase
    else word.toLowerCase
  }
}
