package cc.factorie.util
import java.io.IOException
import java.net.URLConnection
import java.net.URLStreamHandler
import java.net.URLStreamHandlerFactory
import java.io.File
import java.io.InputStream

/** A constructor for URLs that handles "classpath:" protocol.
    @author Andrew McCallum */
object URL {
  def apply(urlString:String): java.net.URL =
    if (urlString.startsWith("classpath:")) new java.net.URL(null, urlString, new ClasspathURLStreamHandler)
    else new java.net.URL(urlString)
}


/** A {@link URLStreamHandler} that handles resources on the classpath.
    Usage: new URL(null, "classpath:some/package/resource.extension", new cc.factorie.util.ClasspathURLStreamHandler(ClassLoader.getSystemClassLoader)
    @author Andrew McCallum */
class ClasspathURLStreamHandler extends URLStreamHandler {
  def this(customClassLoader:ClassLoader) = { this(); classLoader = customClassLoader }
  /** The classloader to find resources from. */
  private var classLoader:ClassLoader = ClassLoader.getSystemClassLoader
  override protected def openConnection(url:java.net.URL): URLConnection = {
    val classpathURL = classLoader.getResource(url.getPath)
    require(classpathURL ne null, "ClassLoader could not find resource "+url.getPath)
    classpathURL.openConnection
  }
}

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
    val locationProperty = System.getProperty(propertyName, null)
    if (locationProperty ne null) {
      //println(getClass.getName+": Using property "+propertyName+" value "+locationProperty)
      // Try to load from URL specified by System property
      try {
        val url = cc.factorie.util.URL(locationProperty+"/"+name) // Use this instead of java.net.URL so that "classpath:" protocol URLs will work.
        val stream = url.openConnection.getInputStream
        require(stream.available > 0)
        stream
      } catch {
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
        if (stream eq null) throw new Error("Could not find resource named '"+name+"' at location '"+propertyName+"'.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to a URL where the resource can be found, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource.")
        require(stream.available > 0)
        stream
      } catch {
        case e:Exception => throw new Error("Could not find resource named '"+name+"' at location '"+propertyName+"'.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to a URL where the resource can be found, such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", e)
      }
    }
  }
}
