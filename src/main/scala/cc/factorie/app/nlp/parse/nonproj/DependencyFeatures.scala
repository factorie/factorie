package cc.factorie.app.nlp.parse.nonproj

import scala.util.parsing.json.JSON
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.util.matching.Regex
import scala.collection.mutable.HashSet

import ParserSupport._

/**
 * Templated dependency parsing feature families to use with FeatureGenerator's 
 * from a feature specification source.
 * 
 * @author Brian Martin
 * @since 9/8/12
 */

object DependencyFeatures {

  val locationAbbrevs = HashMap(
    "S_LAMBDA" -> "l",
    "S_STACK"  -> "s",
    "S_BETA"   -> "b",
    "R_H"      -> "h",     // head
    "R_LMD"    -> "lmd",   // left-most dependent
    "R_RMD"    -> "rmd"    // right-most dependent
  )
  
  val formAbbrevs = HashMap(
    "F_FORM"   -> "f",
    "F_LEMMA"  -> "m",
    "F_POS"    -> "p",
    "F_DEPREL" -> "d",
    "F_LNPL"   -> "lnpl", // left-nearest punctuation of lambda
    "F_RNPL"   -> "rnpl", // right-nearest punctuation of lambda
    "F_LNPB"   -> "lnpb", // left-nearest punctuation of beta
    "F_RNPB"   -> "rnpb"  // right-nearest punctuation of beta
  )
  
  val locationFns: HashMap[String, (Int) => (ParseState) => DepToken] = HashMap(
    "b"   -> ((offset: Int) => (state: ParseState) => state.inputToken(offset)),
    "l"   -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset)),
    "s"   -> ((offset: Int) => (state: ParseState) => state.stackToken(offset)),
    "l_h" -> ((_: Int) => (state: ParseState) => state.lambdaToken(0)),
    "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).leftmostDependent),
    "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).leftmostDependent),
    "l_lmd" -> ((offset: Int) => (state: ParseState) => state.lambdaToken(offset).rightmostDependent),
    "b_lmd" -> ((offset: Int) => (state: ParseState) =>  state.stackToken(offset).rightmostDependent)
  )

  val formFns = HashMap(
    "f"   -> ((t: DepToken) => t.form),
    "m"   -> ((t: DepToken) => t.lemma),
    "p"   -> ((t: DepToken) => t.pos),
    "d"   -> ((t: DepToken) => if (!t.hasHead) "null" else t.head.label),
    "b0"  -> ((t: DepToken) => {
      if (t.pos == -2)
        "null"
      else {
        val s = t.state
        (t.state.lambdaToken(0) eq t.state.sentenceTokens(1)).toString()
      }
    }),
    "b1"  -> ((t: DepToken) => (t.state.stackToken(0) eq t.state.sentenceTokens.last).toString()),
    "b2"  -> ((t: DepToken) => (t.state.input - t.state.stack == 1).toString())
    //"lnpl -> 
    //"rnpl -> 
    //"lnpb -> 
    //"rnpb -> 
  )
  
  def generators(locationOffsetAndForm: String): Function1[ParseState,String] = {
    
    val LocationOffsetAndForm = """([a-z_]*)[+]?([-0-9]*):([a-z]*[0-9]?)""".r

    locationOffsetAndForm match {
      case LocationOffsetAndForm(location, offset, form) => {
        val offsetInt = if (offset == "") 0 else offset.toInt
	    val locationFn: ParseState => DepToken = locationFns(location)(offsetInt)
	    val formFn:     DepToken => String     = formFns(form)
	    (state: ParseState) => location + offset + ":" + formFn(locationFn(state))
      }
      case _ => throw new Error("Couldn't parse location and form from feature generator string.")
    }

  }

}

object LoadParserFeatureSpec {
  def fromXML(source: String) = { 
    import xml._
    
    val x = XML.load(source)
    val fs = (x \\ "feature") filter { f => (f \ "@visible").text.toString() != "false" }
    
	val featureSpec = (for (((f0, f1), f2) <- ((fs \ "@f0") zip (fs \ "@f1") zip (fs \ "@f2"))) yield
	  (f0 :: f1 :: f2 :: Nil).filter(!_.isEmpty).map(_.text)) toList
    
	generateFeatureGenerators(featureSpec)
  }
  
  def fromJSON(source: String) = {
    val someJson = JSON.parseFull(stripJSONComments(source))
    val featureSpec = someJson match {
      case map: Some[Map[String, List[List[String]]]] => map.get("features")
      case _ => throw new Error()
    }
    generateFeatureGenerators(featureSpec)
  }
  
  val DELIMITER = "|"
  
  def generateSingletonFeature(f: String): DependencyFeatureGenerator = new SingletonDependencyFeatureGenerator(f)
  
  def generateCompositeFeatureGenerators(features: Seq[DependencyFeatureGenerator], delimiter: String = DELIMITER) =
    (s: ParseState) => features.map(_.apply(s)).mkString(delimiter)
  
  def generateFeatureGenerators(featureSpec: List[List[String]]): Seq[DependencyFeatureGenerator] = {
    featureSpec.map(fs => {
      val fGens = fs.map(f => generateSingletonFeature(f))
      if (fGens.length > 1)
        new CompositeDependencyFeatureGenerator(fGens)
      else
        fGens.head
    })
  }
  
  private def stripJSONComments(s: String) = s.split("\n").map(_.split("#").head).mkString("\n")
}

    
abstract class DependencyFeatureGenerator extends Function1[ParseState, String]
  
class SingletonDependencyFeatureGenerator(f: String) extends DependencyFeatureGenerator {
  lazy val featureFn = DependencyFeatures.generators(f)
  def apply(s: ParseState): String = featureFn(s)
}

object CompositeDependencyFeatureGenerator {
  val delimiter = "|"
}
class CompositeDependencyFeatureGenerator(gens: Seq[DependencyFeatureGenerator]) extends DependencyFeatureGenerator {
  def apply(s: ParseState) = gens.map(_.apply(s)).mkString(CompositeDependencyFeatureGenerator.delimiter)
}