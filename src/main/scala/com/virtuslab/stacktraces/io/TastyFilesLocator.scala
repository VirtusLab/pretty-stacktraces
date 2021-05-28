package org.virtuslab.stacktraces.io

import org.virtuslab.stacktraces.model.ClasspathWrapper
import org.virtuslab.stacktraces.model.TastyWrapper

import java.io.File
import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardCopyOption

class TastyFilesLocator(classLoader: ClassLoader):

  def tastyFilesFromStackTrace(classNameToPath: Map[String, String]): List[TastyWrapper] =
    classNameToPath.values.toList.distinct.flatMap { clPath =>
    val inputStream = classLoader.getResourceAsStream(clPath + ".tasty")
      if inputStream != null then
        val tastyFile: Path = Files.createTempFile("pretty-stacktraces", ".tasty")
        tastyFile.toFile.deleteOnExit()
        Files.copy(inputStream, tastyFile, StandardCopyOption.REPLACE_EXISTING);
        Some(TastyWrapper(tastyFile.toFile, null))
      else 
        None
    }

  def classNameToPath(classNames: List[String]): Map[String, String] =
    classNames.map { cn =>
      val clArray = cn.stripSuffix("$").split("\\.")
      cn -> (Paths.get(clArray.head, clArray.tail*).toString)
    }.toMap
