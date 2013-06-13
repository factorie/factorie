package cc.factorie.util.classpath.factorie

import java.net.{URLConnection, URLStreamHandler}

/** A {@link URLStreamHandler} that handles resources on the classpath.
    The "protocol" is "classpath"
    The "host" is the Java System Property name whose value may override the "path", e.g. "cc.factorie.app.nlp.wordnet.WordNet" with value "file://Users/me/my-wordnet-dir".  The "host" is optional.
    The "path" is the prefix of the resource name, e.g. "cc/factorie/app/nlp/wordnet/WordNet"
    The "ref" is the suffix of the resource name, e.g. "dict/adj.exc"
    Usage: new URL(null, "classpath://overriding.property.name/default/package#subpackage/resource.extension", new cc.factorie.util.ClasspathURLStreamHandler(ClassLoader.getSystemClassLoader)
    @author Andrew McCallum */
class Handler extends URLStreamHandler {
  def this(customClassLoader:ClassLoader) = { this(); classLoader = customClassLoader }
  /** The classloader to find resources from. */
  private var classLoader:ClassLoader = ClassLoader.getSystemClassLoader
  override protected def openConnection(url:java.net.URL): URLConnection = {
    //println("ClasspathURLStreamHandler.openConnection "+url)
    // The URL.getHost is the name of the Java System Property, which if present whose value will override the getPath (but not the getRef)
    val propertyName = url.getHost
    val locationProperty = if (propertyName.length > 0) System.getProperty(propertyName, null) else null
    if (locationProperty ne null) {
      val newURLString = locationProperty+(if (url.getRef != null && url.getRef.length > 0) url.getRef else "")
      //println(getClass.getName+": Using property "+propertyName+" value "+locationProperty)
      // Try to load from URL specified by System property
      if (! newURLString.startsWith("factorie")) {
        val newURL = new java.net.URL(newURLString + url.getPath)
        val connection = newURL.openConnection
        //connection.connect() // to generate an exception if the resource is not present
        connection
      } else {
        val path = if (url.getPath.apply(0) == '/') url.getPath.drop(1) else url.getPath // TODO Yipes. Why is this necessary? -akm
        val classpathURL = classLoader.getResource(path)
        //println("ClasspathURLStreamHandler ClassLoader URL "+classpathURL)
        if (classpathURL eq null) throw new Error("2: Java System Property '"+propertyName+"' has value '"+locationProperty+"',\n but resource named '"+newURLString+"' could not be found in the classpath.\nEither add to the classpath a jar containing this resource, or set Java System Property '"+propertyName+"' to any valid URL containing the named resource,\n such as file:/my/resource/location or file:relative/location or jar:file:/my/resource.jar or http://foo.com/resource", new ClasspathURLResourceException("ClassLoader could not find resource "+url.getPath))
        classpathURL.openConnection
      }
    } else {
      val path = if (url.getPath.apply(0) == '/') url.getPath.drop(1) else url.getPath // TODO Yipes. Why is this necessary? -akm
      val classpathURL = classLoader.getResource(path)
      //println("ClasspathURLStreamHandler ClassLoader URL "+classpathURL)
      if (classpathURL eq null) throw new ClasspathURLResourceException("ClassLoader could not find resource "+url.getPath)
      classpathURL.openConnection
    }
  }
}
class ClasspathURLResourceException(message:String) extends Exception(message)
class ClasspathURLRecursionException(message:String) extends Exception(message)
