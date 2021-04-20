package com.virtuslab.stacktracebuddy.core

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
    val st = e.getStackTrace.map { ste =>
      val tastyFilesLocator = TastyFilesLocator(classpathDirectories)
      tastyFilesLocator.findTastyFile(ste.getClassName) match
        case Some(tastyFile) =>
          StackTraceBuddyInspector.inspectStackTrace(ste, tastyFile)
        case None =>
          PrettyStackTraceElement(ste, ElementType.Method, ste.getMethodName, ste.getClassName)
    }.toList
    PrettyException(e, st)
