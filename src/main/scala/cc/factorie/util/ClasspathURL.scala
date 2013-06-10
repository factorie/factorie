package cc.factorie.util
import java.io.IOException
import java.net.URLConnection
import java.net.URLStreamHandler
import java.net.URLStreamHandlerFactory
import java.io.File
import java.io.InputStream

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
  private def newURL(urlString:String): java.net.URL =
    if (urlString.startsWith("classpath:")) new java.net.URL(null, urlString, new ClasspathURLStreamHandler)
    else new java.net.URL(urlString)
  private def newURL(propertyName:String, substitutablePath:String, fixedSuffix:String = null): java.net.URL = {
    if (propertyName != null && propertyName.length > 0) {
      val substPath = if (substitutablePath(0) == '/') substitutablePath else "/"+substitutablePath
      newURL("classpath://"+propertyName+substPath+(if (fixedSuffix != null && fixedSuffix.length > 0) "#"+fixedSuffix else ""))
    } else {
      var substPath = if (substitutablePath(0) == '/') substitutablePath.drop(1) else substitutablePath
      newURL("classpath:"+substitutablePath+(if (fixedSuffix != null && fixedSuffix.length > 0) "#"+fixedSuffix else ""))
    }
  }
  /** Create a URL for the default classpath URL for loading resources associated with a class.
      "protocol" is "classpath"
      "host" is cls.getName
      "path" is cls.getName.replace(".","/")+substitutableSuffix
      "ref" is fixedSuffix */
  def apply(cls:Class[_], substitutableSuffix:String, fixedSuffix:String): java.net.URL = newURL(cls.getName, cls.getName.replace(".", "/")+substitutableSuffix, fixedSuffix)
  def apply(cls:Class[_], substitutableSuffix:String): java.net.URL = newURL(cls.getName, cls.getName.replace(".", "/")+substitutableSuffix)
  // Do odd checks for "m.erasure == None" just to preserve the ability to use ClasspathURL[MyClass](".factorie") as well as ClasspathURL("propertyName, substitutablePath")
  def apply[C](substitutableSuffix:String, fixedSuffix:String)(implicit m: Manifest[C]): java.net.URL = {
    println("ClasspathURL.apply sub=%s fixed=%s".format(substitutableSuffix, fixedSuffix))
    if (m.erasure eq classOf[Nothing]) newURL(null, substitutableSuffix, fixedSuffix) // Note these variable name have different meaning in newURL 
    else apply(m.erasure, substitutableSuffix, fixedSuffix)
  }
  // Conflicts with first "apply" above.  Use apply(substitutableSuffix, null) instead.
  def apply[C](substitutableSuffix:String)(implicit m: Manifest[C]): java.net.URL = if (m.erasure eq classOf[Nothing]) newURL(substitutableSuffix) else apply(m.erasure, substitutableSuffix)     
}

/** A {@link URLStreamHandler} that handles resources on the classpath.
    The "protocol" is "classpath"
    The "host" is the Java System Property name whose value may override the "path", e.g. "cc.factorie.app.nlp.wordnet.WordNet" with value "file://Users/me/my-wordnet-dir".  The "host" is optional.
    The "path" is the prefix of the resource name, e.g. "cc/factorie/app/nlp/wordnet/WordNet"
    The "ref" is the suffix of the resource name, e.g. "dict/adj.exc"
    Usage: new URL(null, "classpath://overriding.property.name/default/package#subpackage/resource.extension", new cc.factorie.util.ClasspathURLStreamHandler(ClassLoader.getSystemClassLoader)
    @author Andrew McCallum */
class ClasspathURLStreamHandler extends URLStreamHandler {
  def this(customClassLoader:ClassLoader) = { this(); classLoader = customClassLoader }
  /** The classloader to find resources from. */
  private var classLoader:ClassLoader = ClassLoader.getSystemClassLoader
  override protected def openConnection(url:java.net.URL): URLConnection = {
    // The URL.getHost is the name of the Java System Property, which if present whose value will override the getPath (but not the getRef)
    val propertyName = url.getHost
    val locationProperty = if (propertyName.length > 0) System.getProperty(propertyName, null) else null
    if (locationProperty ne null) {
      val newURLString = locationProperty+(if (url.getRef != null && url.getRef.length > 0) url.getRef else "")
      //println(getClass.getName+": Using property "+propertyName+" value "+locationProperty)
      // Try to load from URL specified by System property
      try {
        var newURL = ClasspathURL(newURLString) // The System Property substituted URL can itself also be a classpath URL.  This makes infinite recursion possible, but we try to detect it below.
        println("ClasspathURLStreamHandler newURL "+newURL)
        if (newURL.getProtocol == "classpath" && newURL.getHost == propertyName) { // We would have recursion
          println("ClasspathURLStreamHandler path=%s ref=%s".format(newURL.getPath, newURL.getRef))
          newURL = ClasspathURL(newURL.getPath.drop(1), newURL.getRef) // avoid recursion by removing the "host" propertyName portion of the new URL
          println("ClasspathURLStreamHandler new non-recursive URL "+newURL)
          //throw new ClasspathURLRecursionException("Java System Property '"+propertyName+"' has value '"+locationProperty+"'.\nThis would cause infinite recursion in cc.factorie.util.ClasspathURLStreamHandler.")
        }
        val connection = newURL.openConnection
        connection.connect() // to generate an exception if the resource is not present
        connection
      } catch {
        case e:ClasspathURLRecursionException => throw e
        case e:java.net.MalformedURLException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"'.\nIt should be set to a valid URL, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e)
        case e:ClasspathURLResourceException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"',\n but resource named '"+newURLString+"' could not be found in the classpath.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to any valid URL containing the named resource,\n such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e)
        case e:java.io.IOException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"',\n but resource named '"+newURLString+"' could not be found.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to any valid URL containing the named resource,\n such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e) 
      }
    } else {
      val classpathURL = classLoader.getResource(url.getPath)
      println("ClasspathURLStreamHandler ClassLoader URL "+classpathURL)
      if (classpathURL eq null) throw new ClasspathURLResourceException("ClassLoader could not find resource "+url.getPath)
      classpathURL.openConnection
    }
  }
}

class ClasspathURLResourceException(message:String) extends Exception(message)
class ClasspathURLRecursionException(message:String) extends Exception(message)

/** Set up URL to handle "classpath:" protocols.

  To register the handler, call URL.setURLStreamHandlerFactory() with your configured factory. 
  Then do new URL("classpath:org/my/package/resource.extension") like the first example and away you go.

  Note: JVM Handler Registration Issue: Note that this method may only be called once per JVM, 
  and note well that Tomcat will use this method to register a JNDI handler (AFAIK). 
  Try Jetty; at worst, you can use the method first and then it has to work around you!

  Basic solution outline from:
  http://stackoverflow.com/questions/861500/url-to-load-resources-from-the-classpath-in-java
  "License: I release this to the public domain, and ask that if you wish to modify that you 
  start a OSS project somewhere and comment here with the details. A better implementation 
  would be to have a URLStreamHandlerFactory that uses ThreadLocals to store URLStreamHandlers 
  for each Thread.currentThread().getContextClassLoader().
  
  @author Andrew McCallum
*/
class ConfigurableStreamHandlerFactory(protocol:String, urlHandler:URLStreamHandler) extends URLStreamHandlerFactory {
  private val protocolHandlers = new scala.collection.mutable.HashMap[String, URLStreamHandler]
  addHandler(protocol, urlHandler)
  def addHandler(protocol:String, urlHandler:URLStreamHandler): Unit = protocolHandlers(protocol) = urlHandler
  def createURLStreamHandler(protocol:String): URLStreamHandler = protocolHandlers(protocol)
}

/** Given the 'relativeClass' argument, provide a function, which given a String name,
    returns an InputStream for the named resource from within a JAR file.
    The JAR file is located by searching in one of two places: (1) If the 'propertyName'
    is set among the Java System Properties then this Property value is used as
    the location in the local file system at which the JAR file containing 
    the named resource can be found.  (2) If the 'propertyName' is not set,
    then the function looks for the named resource in the classpath, relative
    to the 'relativeClass'. */
case class InputStreamFromClasspath(relativeClass:Class[_]) extends Function1[String,InputStream] {
  def apply(name:String): InputStream = {
    import java.util.jar.JarFile
    val propertyName = relativeClass.getName
    //println(getClass.getName+": Getting '"+propertyName+"' resource named "+name)
    val locationProperty = System.getProperty(propertyName, null) // if a directory, must include trailing "/"
    if (locationProperty ne null) {
      val resourceName = locationProperty.substring(1+locationProperty.indexOf(':'))
      //println(getClass.getName+": Using property "+propertyName+" value "+locationProperty)
      // Try to load from URL specified by System property
      try {
        val url = ClasspathURL(locationProperty+name) // Use this instead of java.net.URL so that "classpath:" protocol URLs will work.
        val stream = url.openConnection.getInputStream
        require(stream.available > 0)
        stream
      } catch {
        case e:java.io.FileNotFoundException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"',\n but resource named '"+resourceName+"/"+name+"' could not be found.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to any valid URL containing the named resource,\n such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e) 
        case e:ClasspathURLResourceException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"',\n but resource named '"+resourceName+"/"+name+"' could not be found in the classpath.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to any valid URL containing the named resource,\n such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e)
        case e:java.net.MalformedURLException => throw new Error("Java System Property '"+propertyName+"' has value '"+locationProperty+"'.\nIt should be set to a valid URL, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e)
      }
//      try {
//        val jarFile = new JarFile(file)
//        val jarEntry = jarFile.getJarEntry(name)
//        jarFile.getInputStream(jarEntry)
//      } catch {
//        case e:Exception => throw new Error("Error loading resource '"+name+"' from jar '"+file+"'", e)
//      }
    } else {
      // Try to load from classpath
      try {
        val stream = relativeClass.getResourceAsStream(name)
        if (stream eq null) throw new Error("Could not find resource named '"+name+"' at location '"+propertyName+"'.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to a URL where the resource can be found, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource.\nFor example add java command-line argument -D"+propertyName+"=file:/my/resource/location")
        require(stream.available > 0)
        stream
      } catch {
        case e:Exception => throw new Error("Could not find resource named '"+name+"' at location '"+propertyName+"'.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to a URL where the resource can be found, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource.\nFor example add java command-line argument -D"+propertyName+"=file:/my/resource/location", e)
      }
    }
  }
}
