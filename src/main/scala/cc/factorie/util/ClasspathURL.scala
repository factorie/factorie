package cc.factorie.util
import java.io.IOException
import java.net.URLConnection
import java.net.URLStreamHandler
import java.net.URLStreamHandlerFactory

/** A constructor for URLs that handles "classpath:" protocol.
    @author Andrew McCallum */
object URL {
  def apply(urlString:String): java.net.URL =
    if (urlString.startsWith("classpath:")) new java.net.URL(null, urlString, new ClasspathURLStreamHandler(ClassLoader.getSystemClassLoader))
    else new java.net.URL(urlString)
}


/** A {@link URLStreamHandler} that handles resources on the classpath.
    Usage: new URL(null, "classpath:some/package/resource.extension", new cc.factorie.util.ClasspathURLStreamHandler(ClassLoader.getSystemClassLoader)
    @author Andrew McCallum */
class ClasspathURLStreamHandler extends URLStreamHandler {
  def this(customClassLoader:ClassLoader) = { this(); classLoader = customClassLoader }
  /** The classloader to find resources from. */
  private var classLoader:ClassLoader = getClass.getClassLoader
  override protected def openConnection(u:java.net.URL): URLConnection = classLoader.getResource(u.getPath).openConnection
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
