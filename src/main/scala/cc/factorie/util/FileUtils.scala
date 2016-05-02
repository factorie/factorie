/* Copyright (C) 2008-2016 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.util

import java.io.File

object FileUtils extends FileUtils

class FileUtils {

  /**
   * Tries to convert a string to a readable, writable file, failing otherwise
   */
  def fromString(s:String):File = {
    val f = new File(s)
    /*
    if(f.exists() && f.canRead && f.canWrite) {
      f
    } else {
      throw new IllegalArgumentException("%s cannot be resolved to a readable, writable file".format(s))
    }
    */
    f
  }

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

  /**
   * Get a recursive listing of all files beneath the given directory.
   * TODO make this tail recursive
   * @param dir directory from which to get list of files
   * @return Seq of files that are contained recursively beneath the original directory
   */
  def getRecursiveListOfFiles(dir: File): Seq[File] = {
    val files = dir.listFiles
    files.filterNot(_.isDirectory) ++ files.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  /**
   * Get a recursive listing of all files beneath the given directory name
   * @param dirName name of directory from which to get the list of files
   */
  def getRecursiveListOfFiles(dirName: String): Seq[File] = getRecursiveListOfFiles(new File(dirName))
}
