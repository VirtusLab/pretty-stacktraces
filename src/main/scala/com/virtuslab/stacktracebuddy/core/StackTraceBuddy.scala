package com.virtuslab.stacktracebuddy.core

import com.virtuslab.stacktracebuddy.model.TastyWrapper
import com.virtuslab.stacktracebuddy.model.PrettyException
import com.virtuslab.stacktracebuddy.model.PrettyStackTraceElement
import com.virtuslab.stacktracebuddy.model.ElementType
import com.virtuslab.stacktracebuddy.io.ClasspathDirectoriesLoader
import com.virtuslab.stacktracebuddy.io.TastyFilesLocator

import dotty.tools.dotc.util.NameTransformer
import dotty.tools.dotc.core.Names

import scala.quoted.*
import scala.tasty.inspector.*
import scala.collection.JavaConverters.*

import java.io.File
import java.nio.file.Paths

object StackTraceBuddy:
  lazy val classpathDirectories = ClasspathDirectoriesLoader.getClasspathDirectories 

  def convertToPrettyStackTrace(e: Exception): PrettyException =
    val st = filterInternalStackFrames(e.getStackTrace).map { ste =>
      val tastyFilesLocator = TastyFilesLocator(classpathDirectories)
      tastyFilesLocator.findTastyFile(ste.getClassName) match
        case Some(TastyWrapper(tastyFile, opJarName)) =>
          StackTraceBuddyInspector.inspectStackTrace(ste, tastyFile).copy(jarName = opJarName)
        case None =>
          PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName, ste.getLineNumber)
    }.toList
    PrettyException(e, st)

  private def filterInternalStackFrames(st: Array[StackTraceElement]): List[StackTraceElement] =
    st.sliding(2).toList.flatMap {
      case Array(fs, sc) =>
        if sc.getMethodName.contains("$adapted") then Nil else List(fs)
    }
