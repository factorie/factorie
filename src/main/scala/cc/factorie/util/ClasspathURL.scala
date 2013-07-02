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

object ClasspathURL {
  def fromDirectory[C](suffix:String)(implicit m: Manifest[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) => new java.net.URL(url + suffix)
      case None => m.runtimeClass.getResource(suffix)
    }
  }
  def apply[C](suffix:String)(implicit m: Manifest[C]): java.net.URL = {
    Option(System.getProperty(m.runtimeClass.getName)) match {
      case Some(url) => new java.net.URL(url)
      case None => m.runtimeClass.getResource(m.runtimeClass.getSimpleName+suffix)
    }
  }
}
