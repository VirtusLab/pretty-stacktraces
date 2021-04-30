package com.virtuslab.stacktracebuddy.printer

import com.virtuslab.stacktracebuddy.model.ElementType
import com.virtuslab.stacktracebuddy.model.PrettyStackTraceElement
import com.virtuslab.stacktracebuddy.model.PrettyException


object PrettyExceptionPrinter:

  def printStacktrace(pe: PrettyException): Unit =
    val pst = prettyStackTrace {
      addWithColor(LIGHT_ORANGE)(s"Exception in thread ${Thread.currentThread.getName}: ")
      addWithColor(RED)(s"${pe.original.getClass.getName}: ${pe.original.getMessage}")
      add("\n")
      pe.prettyStackTrace.foreach {
        case PrettyStackTraceElement(ste, elementType, prettyName, prettyFile, lineNumber, opJarName) =>
          add("    at ")
          elementType match
            case ElementType.Lambda(tpe, parent) =>
              add("lambda ")
              addWithColor(AMBER)(tpe)
              add(s" of ${parent} ")
            case _ =>
              add(s"${elementType.name} ")
              addWithColor(AMBER)(s"$prettyName ")
          val lineNumberOrNativeMethod = if ste.isNativeMethod then "(Native method)" else lineNumber
          add("in ")
          addWithColor(GREEN)(prettyFile)
          add(":")
          addWithColor(RED)(s"$lineNumberOrNativeMethod ")
          opJarName match
            case Some(name) =>
              add("inside ")
              addWithColor(LIGHT_PURPLE)(name)
            case None =>
              // do nothing
          add("\n")
      }
    }
    println(pst.build)
