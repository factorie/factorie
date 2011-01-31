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
}