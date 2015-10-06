package cc.factorie.util

import java.io.{InputStream, File}
import java.net.{URI, URL}
import java.nio.file.{FileSystems, Paths, Path}
import scala.io.Source
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.collection.JavaConverters._

/**
 * [[ModelProvider]] is a generic trait that serves to provide trained models (and/or lexicons) to factorie classes
 * without access to the classpath or system properties of the JVM within which factorie is running. This should replace
 * the existing idiom of using [[cc.factorie.util.ClasspathURL]] to resolve models. [[ModelProvider.classpath]] provides
 * the functionality previously supplied by [[cc.factorie.util.ClasspathURL]]. Generally to use [[ModelProvider]] you
 * should simply require an implicit model provider to the primary constructor of you model. To set up:
 * {{{
 *   class MyNER()(implicit mp:ModelProvider[MyNER, InputStream]) {
 *     def deserialize(is:InputStream) {
 *       // do deser
 *     }
 *     Option(mp.provide).foreach(deserialize)
 *   }
 * }}}
 * Implicit conversions on [[ModelProvider]] make usage easy. Concretely:
 * {{{
 *   import cc.factorie.util.ModelProvider
 *
 *   val serNerModel = new DataInputStream(new FileInputStream("myModel"))
 *   val ner = new MyNER()(serNerModel)
 *
 *   //or
 *
 *   implicit val nerMp:ModelProvider[MyNER, InputStream] = new DataInputStream(new FileInputStream("myModel"))
 *   val ner = new MyNer
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
 * @tparam Coords The type of resolver required for for the model (ie. [[java.io.File]], [[java.io.InputStream]], etc)
 */
trait ModelProvider[+Consumer, Coords] {
  def provide:Coords
}

object ModelProvider {

  def classpath[Consumer, Coord](suffix:String=".factorie")(implicit ct:ClassTag[Consumer], urlConv:URL => Coord):ModelProvider[Consumer, Coord] =
    new ModelProvider[Consumer, Coord] {
      def provide = ClasspathURL[Consumer](suffix)
    }

  def empty[C, Coords]:ModelProvider[C, Coords] = new ModelProvider[C, Coords] {
    def provide:Coords = null.asInstanceOf[Coords]
  }

  def classpathDir[Consumer, Coord](implicit ct:ClassTag[Consumer], conv:URL => Coord):ModelProvider[Consumer, Coord] =
    new ModelProvider[Consumer, Coord] {
      def provide: Coord = ClasspathURL.fromDirectory[Consumer]("")
    }

  def classpathRelative[Consumer, Coord](implicit ct:ClassTag[Consumer], conv:URL => Coord):(String => ModelProvider[Consumer, Coord]) =
    new (String => ModelProvider[Consumer, Coord]) {
      def apply(s:String) = new ModelProvider[Consumer, Coord] {
        def provide: Coord = ClasspathURL.fromDirectory[Consumer](s)
      }
    }

  def relative[Consumer, Loc, Coord](l:String)(implicit conv:String => Coord) =
    new (String => ModelProvider[Consumer, Coord]) {
      def apply(s:String) = new ModelProvider[Consumer, Coord] {
        def provide: Coord = (if(l.endsWith("/")) l else l + "/") + s
      }
    }

  def resource[Consumer](nameTransform:String => String=identity)(implicit ct:ClassTag[Consumer]):ModelProvider[Consumer, URL] = new ModelProvider[Consumer, URL] {
    def provide: URL = ct.runtimeClass.getClass.getResource(nameTransform(ct.runtimeClass.getSimpleName))
  }

  implicit def provide[C, Loc, Coords](coord:Loc)(implicit conv:Loc => Coords):ModelProvider[C, Coords] = new ModelProvider[C, Coords] {
    val provide = conv(coord)
  }

  def providerFor[Consumer](implicit mp:ModelProvider[Consumer, _]):String = mp.provide.toString

  def root[Loc](f:Loc)(implicit conv:Loc => File) = new {
    implicit def provideAll[C](implicit ct:ClassTag[C]):ModelProvider[C, URL] = new ModelProvider[C, URL] {
      def provide = new File(f.getAbsolutePath + "/" + ct.runtimeClass.getCanonicalName.replace(".","/") + ".factorie").toURI.toURL
    }
  }
}

trait FileResource[A, B] extends (A => B)

object FileResource {

  implicit object ISSourceRes extends FileResource[InputStream, io.Source] {
    def apply(is:InputStream) = io.Source.fromInputStream(is)
  }

  implicit object UrlStreamRes extends FileResource[URL, InputStream] {
    def apply(u:URL) = u.openStream()
  }

  implicit object FileUrlRes extends FileResource[File, URL] {
    def apply(f:File) = f.toURI.toURL
  }

  implicit object StringFileRes extends FileResource[String, File] {
    def apply(s:String) = FileUtils.fromString(s)
  }

  implicit object FilePathRes extends FileResource[File, Path] {
    def apply(f:File) = f.toPath
  }

  implicit object UrlPathRes extends FileResource[URL, Path] {
    def apply(u:URL) = {
      val arr = u.toString.split("!")
      if(arr.length > 1) { // this is needed because nio.Files doesn't automatically provide a filesystem for zip files
        val fs = FileSystems.newFileSystem(URI.create(arr(0)), Map.empty[String, Any].asJava) // we need to close this later on
        fs.getPath(arr(1))
      } else {
        Paths.get(u.toURI)
      }
    }
  }

  implicit object UrlSourceRes extends FileResource[URL, Source] {
    def apply(u:URL) = Source.fromInputStream(u.openStream())
  }

  // these methods together with the implicit objects above will convert from string/file/url to source as needed implicitly
  /*
  implicit def convert[A,B](a:A)(implicit a2b:FileResource[A,B]):B = a2b(a)
  implicit def convert2[A,B,C](a:A)(implicit a2b:FileResource[A,B], b2c:FileResource[B,C]):C = b2c(a2b(a))
  implicit def convert3[A,B,C,D](a:A)(implicit a2b:FileResource[A,B], b2c:FileResource[B,C], c2d:FileResource[C,D]):D = c2d(b2c(a2b(a)))
  implicit def convert4[A,B,C,D,E](a:A)(implicit a2b:FileResource[A,B], b2c:FileResource[B,C], c2d:FileResource[C,D], d2e:FileResource[D,E]):E = d2e(c2d(b2c(a2b(a))))
  */
}

