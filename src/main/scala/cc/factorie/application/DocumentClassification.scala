package cc.factorie.application
import scala.util.matching.Regex
import java.io.File
import scala.io.Source
import scala.reflect.Manifest

object DocumentClassification {
	import cc.factorie.application.FeatureVectorClassification._
 
	val defaultLexer = "\\w+".r
  
	abstract class Document[L<:Label[This,L],This<:Document[L,This]](override val name:String, labelString:String) extends FeatureVectorClassification.Instance[L,This](name, labelString) {
    this: This =>
		/** Populate the document from the words in the file. */
		def this(file:File, labelString:String, lexer:Regex) = {
			this(file.toString, labelString)
			val source = Source.fromFile(file)
			lexer.findAllIn(source.mkString).foreach(m => this += m.toString)
    }
		/** By default use the defaultLexer */
		def this(file:File, labelString:String) = this(file, labelString, defaultLexer)
		/** By default take the directory name to be the label string. */
		def this(file:File) = this(file, file.getParentFile.getName)
  }
  
  abstract class Label[D<:Document[This,D],This<:Label[D,This]](labelString:String, override val instance:D) extends FeatureVectorClassification.Label[D,This](labelString, instance)
  
  def newModel[L<:Label[D,L],D<:Document[L,D]](implicit lm:Manifest[L],dm:Manifest[D]) =
    new Model(
      new LabelTemplate[L],
      new LabelInstanceTemplate[L,D]
    )

  def newObjective[L<:Label[D,L],D<:Document[L,D]](implicit lm:Manifest[L]) = new TrueLabelTemplate[L]()(lm)
  //def newObjective[L<:Label[_,L]](implicit lm:Manifest[L]) = new TrueLabelTemplate[L]()(lm)


}
