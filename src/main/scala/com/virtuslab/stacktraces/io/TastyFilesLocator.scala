package org.virtuslab.stacktraces.io

import org.virtuslab.stacktraces.model.ClasspathWrapper
import org.virtuslab.stacktraces.model.TastyWrapper

import java.io.File
import java.io.InputStream
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.net.JarURLConnection

class TastyFilesLocator(classLoader: ClassLoader):

  def tastyFilesFromStackTrace(classNameToPath: Map[String, String]): List[TastyWrapper] =
    classNameToPath.values.toList.distinct.flatMap { clPath =>
    val url = classLoader.getResource(clPath + ".tasty")
      if url != null then
        url.getProtocol match
          case "jar" =>
            val outerUrl = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL
            Some(TastyWrapper(Paths.get(outerUrl.toURI).toFile, null))
          case "file" =>
            Some(TastyWrapper(Paths.get(url.toURI).toFile, null))
          case _ =>
            None
      else 
        None
    }

  def classNameToPath(classNames: List[String]): Map[String, String] =
    classNames.map { cn =>
      val clArray = cn.stripSuffix("$").split("\\.")
      cn -> (Paths.get(clArray.head, clArray.tail*).toString)
    }.toMap
