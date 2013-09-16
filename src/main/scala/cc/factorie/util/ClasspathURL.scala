package cc.factorie.util
import scala.language.existentials
import scala.Error

object ClasspathURL {
  def fromDirectory[C](suffix:String)(implicit m: Manifest[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) =>
        try { new java.net.URL(url + suffix) }
        catch {
          case t: java.net.MalformedURLException => throw new Error(s"System property ${m.runtimeClass.getName} contains malformed url ${url+suffix}. Either fix the URL or unset the system property to open a file from the classpath.", t)
        }
      case None =>
        m.runtimeClass.getResource(suffix) match {
          case null => throw new Error(s"No file named $suffix found in classpath for class ${m.runtimeClass.getName}, and no value found in system property ${m.runtimeClass.getName}. To fix this either add a file with the right name to the classpath or set the system property to point to a directory containing the file.")
          case a: java.net.URL => a
        }

    }
  }
  def apply[C](suffix:String)(implicit m: Manifest[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) => try { new java.net.URL(url) }
        catch {
          case t: java.net.MalformedURLException => throw new Error(s"System property ${m.runtimeClass.getName} contains malformed url ${url+suffix}. Either fix the URL or unset the system property to open a file from the classpath.", t)
        }
      case None => m.runtimeClass.getResource(m.runtimeClass.getSimpleName+suffix) match {
        case null => throw new Error(s"No file named ${m.runtimeClass.getSimpleName + suffix} found in classpath for class ${m.runtimeClass.getName}, and no value found in system property ${m.runtimeClass.getName}. To fix this either add a file with the right name to the classpath or set the system property to point to a directory containing the file.")
        case a: java.net.URL => a
      }
    }
  }
}
