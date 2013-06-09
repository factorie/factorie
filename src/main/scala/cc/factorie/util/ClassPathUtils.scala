package cc.factorie.util

import java.io.{InputStream, FileInputStream, File}

/**
 * @author sriedel
 */
object ClassPathUtils {
  /**
   * Takes a relative path name and finds the corresponding file in the classpath. Returns
   * its absolute path.
   */
  def resolveRelativePathUsingClassPath(path: String): String = {
    val resourceURL = getClass.getResource("/" + path)
    if (resourceURL == null)
      new File(path).getAbsolutePath else
      new File(resourceURL.toURI).getAbsolutePath
  }

  /**
   * Loads a resource as stream. This returns either a resource in the classpath,
   * or in case no such named resource exists, from the file system.
   */
  def getStreamFromClassPathOrFile(name: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(name)
    if (is == null) {
      new FileInputStream(name)
    }
    else {
      is
    }

  }
  
  /** Given the first two arguments return a function, which given a String name,
      returns an InputStream for the named resource from within a JAR file.
      The JAR file is located by searching in one of two places: (1) If the 'propertyName'
      is set among the Java System Properties then this Property value is used as
      the location in the local file system at which the JAR file containing 
      the named resource can be found.  (2) If the 'propertyName' is not set,
      then the function looks for the named resource in the classpath, relative
      to the 'relativeClass'. */
  def inputStreamFromJar(relativeClass:Class[_], propertyName:String)(name:String): InputStream = {
    import java.util.jar.JarFile
    val jarLocationProperty = System.getProperty(propertyName, null)
    if (jarLocationProperty ne null) {
      // Try to load from .jar in file system location specified by System property cc.factorie.app.nlp.lexicon.jar
      val file = new File(jarLocationProperty)
      if (!file.exists) throw new Error("File not found at System Property "+propertyName+" value: "+jarLocationProperty)
      try {
        val jarFile = new JarFile(file)
        val jarEntry = jarFile.getJarEntry(name)
        jarFile.getInputStream(jarEntry)
      } catch {
        case e:Exception => throw new Error("Error loading resource '"+name+"' from jar '"+file+"'", e)
      }
    } else {
      // Try to load from .jar on classpath
      try {
        relativeClass.getResourceAsStream(name)
      } catch {
        case e:Exception => throw new Error("Could not find resource for cc.factorie.app.nlp.lexicon: "+name+".  \nDownload factorie-nlp-lexicon.jar and then either add it classpath or set Java System Property 'cc.factorie.app.nlp.lexicon.jar' to its file system location.", e)
      }
    }
  }
  
}
