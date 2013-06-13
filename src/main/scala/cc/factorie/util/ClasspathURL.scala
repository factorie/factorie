package cc.factorie.util
import java.io.IOException
import java.net.URLConnection
import java.net.URLStreamHandler
import java.net.URLStreamHandlerFactory
import java.io.File
import java.io.InputStream
import scala.language.existentials
import java.lang.Error
import scala.Error

/** A constructor for URLs that handle our own "classpath:" protocol, (which allows for substitutions specified in Java System Properties) 
    Although "Classpath" is in this object's name, it will also successfully create URLs for standard non-classpath protocols.
    In our definition of a classpath URL:
      the "protocol" is "classpath";
      the "host" is Property name that will be looked up in Java System Properties, and whose value (if present) will be used as a substitute for the "substitutable" part of this URL; 
      the "path" is the "substitutable" part of the path of the resource to be looked up in the class path;
      the "ref" is the "non-substitutable" suffix of the path of the resource to be looked up in the class path.
    For example, when opening a connection to URL "classpath://cc.factorie.app.nlp.wordnet.WordNet/cc/factorie/app/nlp/wordnet/#dict/adj.exc",
    we will first look for a Java System Property named "cc.factorie.app.nlp.wordnet.WordNet"; if it is present and has value "file:/home/me/my-wordnet-dir/",
    we will open a connection to "file:/home/me/my-wordnet-dir/dict/adj.exc"; if the Property is not present,
    we will instead look up in the classpath "cc/factorie/app/nlp/wordnet/dict/adj.exc".
    @author Andrew McCallum */
object ClasspathURL {
  val oldHandlers = System.getProperty("java.protocol.handler.pkgs")
  if (oldHandlers eq null) {
    System.setProperty("java.protocol.handler.pkgs", "cc.factorie.util.classpath")
  } else {
    System.setProperty("java.protocol.handler.pkgs", "cc.factorie.util.classpath|"+oldHandlers)
  }

  def newURL(propertyName:String, substitutablePath:String, fixedSuffix:String = null): java.net.URL = {
    if (propertyName != null && propertyName.length > 0) {
      val substPath = if (substitutablePath(0) == '/') substitutablePath else "/"+substitutablePath
      new java.net.URL("factorie://"+propertyName+substPath+(if (fixedSuffix != null && fixedSuffix.length > 0) "#"+fixedSuffix else ""))
    } else {
      var substPath = if (substitutablePath(0) == '/') substitutablePath.drop(1) else substitutablePath
      new java.net.URL("factorie:"+substitutablePath+(if (fixedSuffix != null && fixedSuffix.length > 0) "#"+fixedSuffix else ""))
    }
  }
  /** Create a URL for the default classpath URL for loading resources associated with a class.
      "protocol" is "classpath"
      "host" is cls.getName
      "path" is cls.getName.replace(".","/")+substitutableSuffix
      "ref" is fixedSuffix */
  def apply(cls:Class[_], substitutableSuffix:String, fixedSuffix:String): java.net.URL = newURL(cls.getName, cls.getName.replace(".", "/")+substitutableSuffix, fixedSuffix)
  def apply(cls:Class[_], substitutableSuffix:String): java.net.URL = newURL(cls.getName, cls.getName.replace(".", "/")+substitutableSuffix)
  def withoutClass(cls:Class[_], suffix:String): java.net.URL = {
    newURL(cls.getName, suffix)
  }
  // Do odd checks for "m.erasure == None" just to preserve the ability to use ClasspathURL[MyClass](".factorie") as well as ClasspathURL("propertyName, substitutablePath")
  def apply[C](substitutableSuffix:String, fixedSuffix:String)(implicit m: Manifest[C]): java.net.URL = {
    //println("ClasspathURL.apply sub=%s fixed=%s".format(substitutableSuffix, fixedSuffix))
    if (m.erasure eq classOf[Nothing]) newURL(null, substitutableSuffix, fixedSuffix) // Note these variable name have different meaning in newURL 
    else apply(m.erasure, substitutableSuffix, fixedSuffix)
  }
  def apply[C](substitutableSuffix:String)(implicit m: Manifest[C]): java.net.URL = if (m.erasure eq classOf[Nothing]) new java.net.URL(substitutableSuffix) else apply(m.erasure, substitutableSuffix)
}
