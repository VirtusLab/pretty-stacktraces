package com.virtuslab.stacktracebuddy.io

import java.io.File
import java.nio.file.Paths

class TastyFilesLocator(classpathDirectories: List[File]):

  def findTastyFile(className: String): Option[File] =
    val classPath = className.stripSuffix("$").split("\\.")
    val allPossibleFiles = classpathDirectories.flatMap { cpd =>
      val filePath = Paths.get(cpd.toPath.toAbsolutePath.toString, classPath*)
      val tastyPath = filePath.resolveSibling(filePath.getFileName.toString + ".tasty")
      val tastyFile = tastyPath.toFile
      if tastyFile.exists then
        Some(tastyFile)
      else 
        None
    }

    allPossibleFiles match
      case Nil => None
      case head :: Nil => Some(head)
      case head :: tail => throw RuntimeException(s"Conflicting classpaths for $className")
