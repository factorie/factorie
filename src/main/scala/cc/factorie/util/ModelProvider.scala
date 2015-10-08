package cc.factorie.util

import java.io._
import java.net.URL
import java.nio.file.{Files, Path}
import scala.language.implicitConversions
import scala.reflect.{ClassTag, classTag}

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


  def classpath[Consumer : ClassTag](suffix:String=".factorie"):ModelProvider[Consumer] =
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
}

/** Typeclass for things that can become [[java.io.InputStream]] */
trait ISAble[-A] extends (A => InputStream)

object ISAble {
  implicit object IsIs extends ISAble[InputStream] {
    def apply(is:InputStream) = is
  }

  implicit object UrlIs extends ISAble[URL] {
    def apply(u:URL) = u.openStream()
  }

  implicit object FileIs extends ISAble[File] {
    def apply(f:File) = new FileInputStream(f)
  }

  implicit object PathIs extends ISAble[Path] {
    def apply(p:Path) = Files.newInputStream(p)
  }

  def inputStreamOf[A](a:A)(implicit conv:ISAble[A]):InputStream = conv(a)
  def buffered[A](a:A, buf:Int = 8192)(implicit conv:ISAble[A]):BufferedInputStream = conv(a) match {
    case b:BufferedInputStream => b
    case is => new BufferedInputStream(is, buf)
  }
}

