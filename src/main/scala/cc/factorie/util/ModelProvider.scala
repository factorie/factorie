package cc.factorie.util

import java.io._
import java.net.URL
import java.nio.file.{Paths, Path}

import cc.factorie.app.nlp.lexicon.LexiconsProvider

import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.{TypeTag, typeTag}

/**
 * [[ModelProvider]] is a generic trait that serves to provide trained models (and/or lexicons) to factorie classes
 * without access to the classpath or system properties of the JVM within which factorie is running. This should replace
 * the existing idiom of using [[cc.factorie.util.ClasspathURL]] to resolve models. [[ModelProvider.classpath]] provides
 * the functionality previously supplied by [[cc.factorie.util.ClasspathURL]]. ModelProvider provides resources as
 * [[java.io.InputStream]], though the returned objects are instances of [[java.io.BufferedInputStream]] for performance.
 * Generally to use [[ModelProvider]] you should simply require an implicit model provider to the primary constructor
 * of your model. To set up:
 * {{{
 *   class MyNER()(implicit mp:ModelProvider[MyNER]) {
 *     def deserialize(is:InputStream) {
 *       // do deser
 *     }
 *     Option(mp.provide).foreach(deserialize)
 *   }
 * }}}
 * Implicit conversions on [[ISAble]] make usage easy by supplying conversions from types that refer to resources like
 * [[java.io.File]] or [[java.nio.file.Path]] to [[java.io.InputStream]]. Alternatively instances of
 * [[java.io.InputStream]] can be supplied directly and will not be altered. Concretely:
 * {{{
 *   import cc.factorie.util.ModelProvider
 *   import java.nio.file.Path
 *
 *   val serNerModel = Paths.get("path/to/my/NerModel.factorie")
 *   val ner = new MyNER()(serNerModel)
 *
 *   //or
 *
 *   implicit val nerMp:ModelProvider[MyNER] = new File("path/to/my/NerModel.factorie")
 *   val ner = new MyNer
 *
 *   //or
 *
 *   //these models are big
 *   var ner = new MyNer()(new BufferedInputStream(new FileInputStream("path/to/my/NerModel.factorie"), 32768))
 *
 * }}}
 *
 * Alternatively if you have many models in a directory tree that conforms to their package structure, you can use
 * [[ModelProvider.root]] as follows:
 * {{{
 *   import cc.factorie.util.ModelProvider
 *
 *   val modelsRoot = ModelProvider.root(new File("path/to/my/models/"))
 *   import modelsRoot._
 *
 *   val pos = new MyPOS
 *   val ner = new MyNER
 *   val coref = new MyCoref
 * }}}
 * Note that [[ModelProvider.root]] will attempt to provide all models relative to that root, if you have another path
 * to certain models you will need to provide it afterwards to override it.
 *
 * To inspect what model path is currently being provided for a given class, use [[ModelProvider.providerFor]].
 * @author johnsullivan
 * @tparam Consumer The class of the Model that is required
 */
trait ModelProvider[+Consumer] {

  def provide:InputStream
  def coordinates:String
}

object ModelProvider {
  import ISAble._


  def classpath[Consumer : ClassTag ](suffix:String=".factorie"):ModelProvider[Consumer] =
    new ModelProvider[Consumer] {
      private val url = ClasspathURL[Consumer](suffix)
      val coordinates = url.toString
      val provide:InputStream = buffered(url)
    }

  def empty[C]:ModelProvider[C] = new ModelProvider[C] {
    val coordinates = "null"
    def provide:InputStream = null
  }

  def classpathDir[Consumer](implicit ct:ClassTag[Consumer]):ModelProvider[Consumer] =
    new ModelProvider[Consumer] {
      private val url = ClasspathURL.fromDirectory[Consumer]("")
      val coordinates = url.toString
      val provide:InputStream = buffered(url)
    }

  def classpathRelative[Consumer](implicit ct:ClassTag[Consumer]):(String => ModelProvider[Consumer]) =
    new (String => ModelProvider[Consumer]) {
      def apply(s:String) = new ModelProvider[Consumer] {
        private val url = ClasspathURL.fromDirectory[Consumer](s)
        val coordinates = url.toString
        val provide:InputStream = buffered(url)
      }
    }

  def relative[Consumer, Loc, Coord](l:Path) =
    new (String => ModelProvider[Consumer]) {
      def apply(s:String) = new ModelProvider[Consumer] {
        private val path = l resolve s
        val coordinates = path.toString
        val provide:InputStream = buffered(path)
      }
    }

  def resource[Consumer](nameTransform:String => String=identity)(implicit ct:ClassTag[Consumer]):ModelProvider[Consumer] = new ModelProvider[Consumer] {
    private val url = ct.runtimeClass.getClass.getResource(nameTransform(ct.runtimeClass.getSimpleName))
    val coordinates = url.toString
    val provide:InputStream = buffered(url)
  }

  implicit def provide[C, Loc : ISAble](coord:Loc):ModelProvider[C] = new ModelProvider[C] {
    val coordinates = coord.toString
    val provide:InputStream = buffered(coord)
  }

  def providerFor[Consumer](implicit mp:ModelProvider[Consumer]):String = mp.coordinates

  def root(f:Path) = new {
    implicit def provideAll[C : ClassTag]:ModelProvider[C] = new ModelProvider[C] {
      private val modelPath = f resolve classTag[C].runtimeClass.getCanonicalName.replace(".","/") + ".factorie"
      val coordinates = modelPath.toString
      val provide:InputStream = buffered(modelPath)
    }
  }

  def url[Consumer](url:URL):ModelProvider[Consumer] = new ModelProvider[Consumer] {
    val coordinates: String = url.toString
    def provide: InputStream = url.openStream()
  }
}

/**
 * An extension for [[cc.factorie.util.CmdOptions]] that makes it easy to provide models that need model providers.
 * Concretely:
 * {{{
 *   package cool.models
 *   import cc.factorie.util.{CmdOptions, ModelProviderCmdOptions}
 *
 *   class MyCoolModel()(implicit mp:ModelProvider[MyCoolModel])
 *
 *   object MyCoolOpts extends CmdOptions with ModelProviderCmdOptions {
 *    implicit val coolModel = new ModelCmdOption[MyCoolModel]
 *    // other cmdopts as normal
 *   }
 *
 *   object CoolModelRunnings {
 *
 *     def main(args:Array[String]) {
 *      MyCoolOpts parse args
 *
 *      val model = new MyCoolModel()(MyCoolOpts.coolModel.value)
 *    }
 *   }
 * }}}
 * To provide a model at the command line, you would use:
 * `java ... cool.models.CoolModelRunnings --cool.models.MyCoolModel="path/to/my/coolmodel.factorie"`
 * Note that the resulting command-line argument is the full package path of the model. The parsing of the path provided
 * to the model uses [[java.nio.file.Path]] so anything that works with that should parse properly.
 */
trait ModelProviderCmdOptions extends CmdOptions {
  cmd =>
  class ModelCmdOption[ModelClass : TypeTag : ClassTag](val defaultValue:ModelProvider[ModelClass], val name:String, val required: Boolean) extends cc.factorie.util.CmdOption[ModelProvider[ModelClass]] {

    def this(mp:ModelProvider[ModelClass], req:Boolean) = this(mp, classTag[ModelClass].runtimeClass.getName, req)
    def this(mp:ModelProvider[ModelClass]) = this(mp, false)
    def this() = this(ModelProvider.empty)

    cmd += this

    val valueType = typeTag[ModelClass]

    val shortName: Char = classTag[ModelClass].runtimeClass.getName()(0)

    def setValue(v: ModelProvider[ModelClass]) { _value = v}

    val valueName: String = s"ModelProvider[$name]"
    val helpMsg: String = s"This should be a valid path to a file that is a serialized model for $name"

    private var _value:ModelProvider[ModelClass] = defaultValue
    private var _invokedCount = 0

    def value: ModelProvider[ModelClass] = _value

    override def parse(args: Seq[String], index: Int): Int = if(args(index) == "--"+name) { // we don't support shortName
      _value = Paths get args(index + 1)
      _invokedCount += 1
      math.min(index + 2, args.length)
    } else if(args(index).startsWith("--" + name + "=")) {
      _value = Paths get args(index).drop(name.length + 3)
      _invokedCount += 1
      index + 1
    } else index

    override def unParse:Seq[String] = Seq(s"$name=${value.coordinates}")

    def hasValue: Boolean = !(classTag[ModelClass] == classTag[Nothing])
    def invokedCount = _invokedCount


  }

  class LexiconsProviderCmdOption(val name:String, useFullPath:Boolean=false, val defaultValue:LexiconsProvider= LexiconsProvider.classpath(), val required:Boolean=false) extends cc.factorie.util.CmdOption[LexiconsProvider] {

    cmd += this

    val shortName = 'l'

    private var _value:LexiconsProvider = defaultValue
    private var _invokedCount = 0

    def setValue(v: LexiconsProvider) = {_value = v}

    val valueName = "LexiconsProvider"

    val helpMsg = "This should be a valid path to the root directory containing a set of static Lexicons"
    def value = _value

    override def parse(args: Seq[String], index: Int) = if(args(index) == "--"+name) {
      _value = LexiconsProvider.fromFile(new File(args(index + 1)), useFullPath)
      _invokedCount += 1
      math.min(index + 2, args.length)
    } else if(args(index).startsWith("--"+name+"=")){
      _value = LexiconsProvider.fromString(args(index).drop(name.length + 3), useFullPath)
      _invokedCount += 1
      index + 1
    } else index

    override def unParse:Seq[String] = Seq(s"--$name=${value.lexiconRoot}")




    val hasValue = true
    def invokedCount = _invokedCount
  }

}

