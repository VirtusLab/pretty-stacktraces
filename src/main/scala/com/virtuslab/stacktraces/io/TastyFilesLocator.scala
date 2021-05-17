package org.virtuslab.stacktraces.io

import org.virtuslab.stacktraces.model.ClasspathWrapper
import org.virtuslab.stacktraces.model.TastyWrapper

import java.io.File
import java.nio.file.Paths

class TastyFilesLocator(classpathDirectories: List[ClasspathWrapper]):

  def findTastyFile(className: String): Option[TastyWrapper] =
    val classPath = className.stripSuffix("$").split("\\.")
    val allPossibleFiles = classpathDirectories.flatMap { case ClasspathWrapper(cpd, opJarName) =>
      val filePath = Paths.get(cpd.toPath.toAbsolutePath.toString, classPath*)
      val tastyPath = filePath.resolveSibling(filePath.getFileName.toString + ".tasty")
      val tastyFile = tastyPath.toFile
      if tastyFile.exists then
        Some(TastyWrapper(tastyFile, opJarName))
      else 
        None
    }

    allPossibleFiles match
      case Nil => None
      case head :: Nil => Some(head)
      case head :: tail => throw RuntimeException(s"Conflicting classpaths for $className")
