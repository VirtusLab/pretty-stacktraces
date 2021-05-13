package com.virtuslab.stacktraces.core

import com.virtuslab.stacktraces.model.TastyWrapper
import com.virtuslab.stacktraces.model.PrettyException
import com.virtuslab.stacktraces.model.PrettyStackTraceElement
import com.virtuslab.stacktraces.model.ElementType
import com.virtuslab.stacktraces.io.ClasspathDirectoriesLoader
import com.virtuslab.stacktraces.io.TastyFilesLocator

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import java.nio.file.Paths

object Stacktraces:
  lazy val classpathDirectories = ClasspathDirectoriesLoader.getClasspathDirectories 

  def convertToPrettyStackTrace(e: Exception): PrettyException =
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
