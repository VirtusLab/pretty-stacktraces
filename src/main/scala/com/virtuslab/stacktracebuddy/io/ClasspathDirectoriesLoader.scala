package com.virtuslab.stacktracebuddy.io

import java.net.URLClassLoader
import java.net.URL
import java.io.File

object ClasspathDirectoriesLoader:

  private def getUrls(cl: ClassLoader): Array[URL] = cl match
    case null => Array()
    case u: URLClassLoader => u.getURLs ++ getUrls(cl.getParent)
    case _ => getUrls(cl.getParent)

  private def getClasspath: List[File] =
    getUrls(getClass.getClassLoader).map(_.toURI).map(File(_)).toList

  def getClasspathDirectories: List[File] =
    val classPathFiles = getClasspath
    val (directories, jars) = classPathFiles.partition(_.isDirectory)
    val allDirectories = directories ++ Unzipper.unzipFiles(jars)
    allDirectories
