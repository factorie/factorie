package cc.factorie.example
import scala.xml._
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import cc.factorie.application.LabeledTokenSeqs
import cc.factorie.er._

object Football {

  // Define the variable classes
  class Token(word:String, labelString:String) extends LabeledTokenSeqs.Token[Label,Token](word) {
    val label = new Label(labelString, this)
  }
  class Label(tag:String, token:Token) extends LabeledTokenSeqs.Label[Token,Label](tag, token)

  // Define the model:
  val model = new Model(
    Foreach[Label] { label => Score(label) },
    Foreach[Label] { label => Score(label.prev, label, label.token) }
  )

  val lexiconDir = "/Users/mccallum/research/projects/football/lexicons/"
  val personLexicon = new LabeledTokenSeqs.Lexicon(lexiconDir+"people")
  //val orgLexicon = new LabeledTokenSeqs.Lexicon(lexiconDir+"orgs")
  //val positionLexicon = new LabeledTokenSeqs.Lexicon(lexiconDir+"positions")
  
  val objective = new Model(
    new TemplateWithVectorStatistics1[Label] {
      val orgIndex = Domain[Label].index("ORG")
      val personIndex = Domain[Label].index("ORG")
    	def score(s:Stat) = {
    		val label: Label = s.s1
    		val token = label.token
    		label.index match {
          case personIndex => if (personLexicon.contains(token)) 1.0 else -1.0
          //case orgIndex => if (orgLexicon.contains(token)) 1.0 else -1.0
        }
      }
    }
  )
  def main(args:Array[String]): Unit = {
    val directories: Seq[String] = if (args.length > 1) args else List("/Users/mccallum/research/projects/football/data") 
    for (dirname <- directories) {
    	for (file <- files(new File(dirname))) {
    		val article = XML.loadFile(file)
    		//println(article \\ "head" \\ "title" text)
    		//println("  charcount "+ (article \\ "body" \\ "body.content").text.length)
    		val content = article \\ "head" \\ "docdata" \\ "identified-content"
        for (person <- content \\ "person") {
          val names = person.text.trim.replaceAll(" *\\(.*\\)","").replaceAll(" JR","").split("\\s*,\\s*")
          println("p "+names.reverse.mkString(" "))
        }
        for (org <- content \\ "org") {
          val names = org.text.trim.replaceAll("\\n"," ").replaceAll(" *\\(.*\\)","").split("\\s*,\\s*")
          println("o "+names.reverse.mkString(" "))
        }
    	}
    }
  }
  def files(directory:File): Seq[File] = {
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry <- directory.listFiles) {
    	if (entry.isFile) result += entry
    	else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }
}
