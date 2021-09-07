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
import java.net.URLClassLoader
import java.net.URL

class TastyFilesLocator(classLoader: ClassLoader, additionalClasspath: Seq[String]):

  val newClassloader = URLClassLoader(additionalClasspath.map(File(_).toURI.toURL).toArray, classLoader)

  def tastyFilesFromStackTrace(classNameToPath: Map[String, String]): List[TastyWrapper] =
    classNameToPath.values.toList.distinct.flatMap { clPath =>
    val url = newClassloader.getResource(clPath + ".tasty")
      if url != null then

        // Get temporary copy of the tasty file from the URL
        val tastyFile: File =
          val inputStream = url.openStream
          val tf: Path = Files.createTempFile("pretty-stacktraces", ".tasty")
          tf.toFile.deleteOnExit()
          Files.copy(inputStream, tf, StandardCopyOption.REPLACE_EXISTING)
          tf.toFile

        // Get path of the external jar (if any)
        val extraJar = url.getProtocol match
          case "jar" =>
            val outerUrl = url.openConnection.asInstanceOf[JarURLConnection].getJarFileURL
            Some(Paths.get(outerUrl.toURI).toFile)
          case "file" =>
            None
          case _ =>
            None
        Some(TastyWrapper(tastyFile, extraJar))
      else 
        None
    }

  def classNameToPath(classNames: List[String]): Map[String, String] =
    classNames.map { cn =>
      val clArray = cn.stripSuffix("$").split("\\.")
      cn -> (Paths.get(clArray.head, clArray.tail*).toString)
    }.toMap
