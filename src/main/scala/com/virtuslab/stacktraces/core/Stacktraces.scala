package org.virtuslab.stacktraces.core

import org.virtuslab.stacktraces.model.ClasspathWrapper
import org.virtuslab.stacktraces.model.TastyWrapper
import org.virtuslab.stacktraces.model.PrettyException
import org.virtuslab.stacktraces.model.PrettyStackTraceElement
import org.virtuslab.stacktraces.model.ElementType
import org.virtuslab.stacktraces.io.ClasspathDirectoriesLoader
import org.virtuslab.stacktraces.io.TastyFilesLocator

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import java.nio.file.Paths

object Stacktraces:
  lazy val classpathDirectories = ClasspathDirectoriesLoader.getClasspathDirectories 

  def convertToPrettyStackTrace(e: Throwable): PrettyException =
    convertToPrettyStackTrace(e, classpathDirectories)

  def convertToPrettyStackTrace(
    e: Throwable,
    classpathDirectories: List[ClasspathWrapper]
  ): PrettyException =
    val st = filterInternalStackFrames(e.getStackTrace).flatMap { ste =>
      val tastyFilesLocator = TastyFilesLocator(classpathDirectories)
      tastyFilesLocator.findTastyFile(ste.getClassName) match
        case Some(TastyWrapper(tastyFile, opJarName)) =>
          StacktracesInspector.inspectStackTrace(ste, tastyFile).map(_.copy(jarName = opJarName))
        case None =>
          Some(PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber))
    }.toList
    PrettyException(e, st)

  private def filterInternalStackFrames(st: Array[StackTraceElement]): List[StackTraceElement] =
    st.sliding(2).toList.flatMap {
      case Array(fs, sc) =>
        if sc.getMethodName.contains("$adapted") then Nil else List(fs)
    }
