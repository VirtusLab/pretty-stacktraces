package org.virtuslab.stacktraces.io

import java.net.URLClassLoader
import java.net.URL
import java.io.File

import org.virtuslab.stacktraces.model.ClasspathWrapper

object ClasspathDirectoriesLoader:

  private def getUrls(cl: ClassLoader): Array[URL] = cl match
    case null => Array()
    case u: URLClassLoader => u.getURLs ++ getUrls(cl.getParent)
    case _ => getUrls(cl.getParent)

  def getClasspath: List[File] =
    getUrls(getClass.getClassLoader).map(_.toURI).map(File(_)).toList

  def getClasspathDirectories: List[ClasspathWrapper] =
    getClasspathDirectories(getClasspath)
  def getClasspathDirectories(classPathFiles: Seq[File]): List[ClasspathWrapper] =
    val (directories, jars) = classPathFiles.toList.partition(_.isDirectory)
    val allDirectories = projectClassesToClasspathWrappers(directories) ++ jarsToClasspathWrappers(jars)
    allDirectories

  private def projectClassesToClasspathWrappers(directories: List[File]): List[ClasspathWrapper] =
    directories.map(ClasspathWrapper(_, None))
  
  private def jarsToClasspathWrappers(jars: List[File]): List[ClasspathWrapper] =
    jars.map(j => ClasspathWrapper(Unzipper.unzipFile(j), Some(j.getName)))

