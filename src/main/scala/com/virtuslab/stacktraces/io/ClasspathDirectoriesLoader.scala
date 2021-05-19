package org.virtuslab.stacktraces.io

import java.net.URLClassLoader
import java.net.URL
import java.io.File

import org.virtuslab.stacktraces.model.ClasspathWrapper

object ClasspathDirectoriesLoader:

  private def classpath(cl: ClassLoader): Array[File] = cl match
    case null => Array()
    case u: URLClassLoader => u.getURLs.map(_.toURI).map(new File(_)) ++ classpath(cl.getParent)
    case cl if cl.getClass.getName == "jdk.internal.loader.ClassLoaders$AppClassLoader" =>
      // Required with JDK >= 9
      sys.props.getOrElse("java.class.path", "")
        .split(File.pathSeparator)
        .filter(_.nonEmpty)
        .map(new File(_))
    case _ => classpath(cl.getParent)

  def getClasspath: List[File] =
    getClasspath(Thread.currentThread().getContextClassLoader)
  def getClasspath(loader: ClassLoader): List[File] =
    classpath(loader).toList

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

