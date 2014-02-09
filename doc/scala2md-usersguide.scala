#!/bin/bash
export SCRIPT_PATH=$(greadlink -f "$0") && exec scala "$0" "$@"
!#

import scala.io.Source
import java.io._
	
/* Get list of files to convert */
val scriptPath = System.getenv("SCRIPT_PATH")
val filePrefix = scriptPath.substring(0, scriptPath.lastIndexOf('/'))
val outputPathName = s"$filePrefix/html/"
	
val tutorialsDirName = s"$filePrefix/../src/main/scala/cc/factorie/tutorial/"
val tutorialsPattern = "(UsersGuide.*)|(Tutorial.*)"
val tutorialsDir = new File(tutorialsDirName)
val fileNames = tutorialsDir.listFiles.map(_.getName)
	
fileNames.foreach(fileName => {
  val fileNamePattern = "(Tutorial|UsersGuide)([0-9]+).*".r
  fileName match {
    case fileNamePattern(tutorialType, num) => {
      val group = if(tutorialType == "Tutorial") "tutorial" else "usersguide"
      val weight = Integer.parseInt(num) + 10
      val outputFileName = s"$outputPathName${fileName.replace(".scala", ".md")}"
      val outputFile = new File(outputFileName)
      val outputFileWriter = new PrintWriter(new BufferedWriter(new FileWriter(outputFile)))
    
      val lines = Source.fromFile(s"$tutorialsDirName$fileName").getLines
  
      println(s"Processing file ${fileName} -> ${outputFileName}")
  
      /* Write top matter for Jekyll */
      outputFileWriter.println("---")
      outputFileWriter.println("title: \"" + lines.toSeq.head.trim.drop(3).dropRight(2).trim + "\"")
      outputFileWriter.println("layout: default")
      outputFileWriter.println(s"group: ${group}")
      outputFileWriter.println(s"weight: ${weight}")
      outputFileWriter.println("---")
  
      val singleLineComment = "\\p{Space}*/\\*&\\p{Space}*(.*)\\*/\\p{Space}*".r
      val startComment = "\\p{Space}*/\\*&\\p{Space}*(.*)".r
      val inComment = "\\p{Space}*\\*(?!\\*)\\p{Space}*(.*)".r
      val endComment1 = "\\p{Space}*(.*)\\*/\\p{Space}*".r
      val endComment2 = "\\p{Space}*(.*)\\*\\*/\\p{Space}*".r

      var state = 0;
      lines.foreach(line => {
		line.trim match{
		  case singleLineComment(m) => { if(state != 0 && state != 3) outputFileWriter.println("```"); outputFileWriter.println(m); state = 3 }
		  case startComment(m) => { if(state != 0 && state != 3) outputFileWriter.println("```"); outputFileWriter.println(m); state = 2 }
		  case endComment2(m) => { if(m.trim != "") outputFileWriter.println(m); state = 3 }
		  case endComment1(m) => { if(m.trim != "") outputFileWriter.println(m); state = 3 }
		  case inComment(m) => { if(state == 2) outputFileWriter.println(m) else outputFileWriter.println(line) }
		  case _ => { if(state == 3 && !line.matches("\\p{Space}*")){outputFileWriter.println("\n```scala"); state = 1}; outputFileWriter.println(line) }
	  }
	})
	if (state == 1) outputFileWriter.println("```")
	  outputFileWriter.close
    }
    case _ => { /* File wasn't a tutorial/usersguide file */ }
  }
  
})
