package cc.factorie.util

import java.io.File

object FileUtils extends FileUtils

class FileUtils {

  /**
   * Returns a list of the file names of files with the given ending under the given directory
   * @param fileName name of directory from which to get list of file names
   * @param ending file name ending to match
   * @return Seq of filenames with the given ending from the given directory
   */
  def getFileListFromDir(fileName: String, ending: String = ""): Seq[String] = {
    val dir = new File(fileName)
    println("Getting file list from directory: " + fileName)
    if (dir != null) {
      dir.listFiles.filter(_.getName.endsWith(ending)).map(_.getAbsolutePath)
    } else {
      println("Directory not found: " + fileName)
      null
    }
  }
}
